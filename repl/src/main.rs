
use std::path::PathBuf;
use std::thread;
use std::process::exit;
use std::sync::Mutex;

use rustyline::error::ReadlineError;
use rustyline::{Cmd, CompletionType, Config, EditMode, Editor, KeyEvent, KeyCode, Modifiers};

use anyhow::Result;
use clap::Parser;
use directories::ProjectDirs;
use signal_hook::{consts::SIGINT, iterator::Signals};

use hyperon::common::shared::Shared;

mod metta_shim;
use metta_shim::*;

mod config_params;
use config_params::*;

mod interactive_helper;
use interactive_helper::*;

static SIGINT_RECEIVED_COUNT: Mutex<usize> = Mutex::new(0);

#[derive(Parser)]
#[command(version, about)]
struct CliArgs {
    /// .metta files to execute.  `metta` will run in interactive mode if no files are supplied
    files: Vec<PathBuf>,

    /// Additional include directory paths
    #[arg(short, long)]
    include_paths: Vec<PathBuf>,
}

fn main() -> Result<()> {
    let cli_args = CliArgs::parse();

    //The repl will treat all file args except the last one as imports
    let (primary_metta_file, other_metta_files) = if let Some((first_path, other_paths)) = cli_args.files.split_last() {
        (Some(first_path), other_paths)
    } else {
        (None, &[] as &[PathBuf])
    };

    //Config directory will be here: TODO: Document this in README.
    // Linux: ~/.config/metta/
    // Windows: ~\AppData\Roaming\TrueAGI\metta\config\
    // Mac: ~/Library/Application Support/io.TrueAGI.metta/
    let repl_params = match ProjectDirs::from("io", "TrueAGI",  "metta") {
        Some(proj_dirs) => ReplParams::new(proj_dirs.config_dir(), cli_args.include_paths, primary_metta_file),
        None => {
            eprint!("Failed to initialize config!");
            ReplParams::default()
        }
    };
    let repl_params = Shared::new(repl_params);

    //Create our MeTTa runtime environment
    let mut metta = MettaShim::new(repl_params.clone());

    //Spawn a signal handler background thread, to deal with passing interrupts to the execution loop
    let mut signals = Signals::new(&[SIGINT])?;
    thread::spawn(move || {
        for _sig in signals.forever() {
            //Assume SIGINT, since that's the only registered handler
            let mut signal_received_cnt = SIGINT_RECEIVED_COUNT.lock().unwrap();
            match *signal_received_cnt {
                0 => println!("Interrupt received, stopping MeTTa..."),
                1 => println!("Stopping in progress.  Please wait..."),
                _ => {
                    println!("Ok, I get it!  Yeesh!");
                    exit(-1);
                },
            }
            *signal_received_cnt += 1;
            drop(signal_received_cnt);
        }
    });

    //If we have .metta files to run, then run them
    if let Some(metta_file) = primary_metta_file {

        //All non-primary .metta files run without printing output
        //TODO: Currently the interrupt handler does not break these
        for import_file in other_metta_files {
            metta.load_metta_module(import_file.clone());
        }

        //Only print the output from the primary .metta file
        let metta_code = std::fs::read_to_string(metta_file)?;
        metta.exec(metta_code.as_str());
        metta.inside_env(|metta| {
            for result in metta.result.iter() {
                println!("{result:?}");
            }
        });
        Ok(())

    } else {

        //Otherwise enter interactive mode
        start_interactive_mode(repl_params, metta).map_err(|err| err.into())
    }
}

// To debug rustyline:
// RUST_LOG=rustyline=debug cargo run --example example 2> debug.log
fn start_interactive_mode(repl_params: Shared<ReplParams>, metta: MettaShim) -> rustyline::Result<()> {

    //Init RustyLine
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();
    let helper = ReplHelper::new(metta);
    let mut rl = Editor::with_config(config)?;

    //QUESTION: Should we provide a config to use vi key bindings vs. Emacs?

    rl.set_helper(Some(helper));
    //KEY BEHAVIOR: Enter and ctrl-M will add a newline when the cursor is in the middle of a line, while
    // ctrl-J will submit the line.
    //TODO: Rustyline seems to have a bug where this is only true sometimes.  Needs to be debugged.
    // Ideally Rustyline could just subsume the whole "accept_in_the_middle" behavior with a design that
    // allows the Validator to access the key event, so the Validator could make the decision without
    // special logic inside rustyline.
    rl.bind_sequence(KeyEvent( KeyCode::Enter, Modifiers::NONE ), Cmd::AcceptOrInsertLine {
        accept_in_the_middle: false,
    });
    rl.bind_sequence(KeyEvent::ctrl('j'), Cmd::AcceptOrInsertLine {
        accept_in_the_middle: true,
    });
    rl.bind_sequence(KeyEvent::alt('n'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyEvent::alt('p'), Cmd::HistorySearchBackward);
    if let Some(history_path) = &repl_params.borrow().history_file {
        if rl.load_history(history_path).is_err() {
            println!("No previous history found.");
        }
    }

    //The Interpreter Loop
    loop {

        //Set the prompt based on resolving a MeTTa variable
        let prompt = {
            let helper = rl.helper_mut().unwrap();
            let mut metta = helper.metta.borrow_mut();
            let prompt = metta.get_config_string("ReplDefaultPrompt").unwrap_or("> ".to_string());
            let styled_prompt = metta.get_config_string("ReplStyledPrompt").unwrap_or(format!("\x1b[1;32m{prompt}\x1b[0m"));
            helper.colored_prompt = styled_prompt;
            prompt
        };

        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                let mut metta = rl.helper().unwrap().metta.borrow_mut();
                metta.exec(line.as_str());
                metta.inside_env(|metta| {
                    for result in metta.result.iter() {
                        println!("{result:?}");
                    }
                });
            }
            Err(ReadlineError::Interrupted) |
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }

    if let Some(history_path) = &repl_params.borrow().history_file {
        rl.append_history(history_path)?
    }

    Ok(())
}
