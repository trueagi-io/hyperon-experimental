
use std::path::PathBuf;
use std::thread;
use std::process::exit;
use std::sync::{Arc, Mutex};

use rustyline::error::ReadlineError;
use rustyline::{Cmd, CompletionType, Config, EditMode, Editor, KeyEvent, KeyCode, Modifiers, EventContext, RepeatCount, EventHandler, ConditionalEventHandler, Event};

use anyhow::Result;
use clap::Parser;
use signal_hook::{consts::SIGINT, iterator::Signals};

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
    /// .metta file to execute.  `metta` will run in interactive mode if no file is supplied
    file: Option<PathBuf>,

    /// Additional include directory paths
    #[arg(short, long)]
    include_paths: Vec<PathBuf>,
}

fn main() -> Result<()> {
    let cli_args = CliArgs::parse();

    //If we have a metta_file, then the working dir is the parent of that file
    //If we are running in interactive mode, it's the working dir at the time the repl is invoked
    let metta_working_dir: PathBuf = match &cli_args.file {
        Some(metta_file) => {
            metta_file.parent().unwrap().into()
        },
        None => {
            match std::env::current_dir() {
                Ok(cwd) => cwd,
                Err(_) => PathBuf::from("./").canonicalize().unwrap(),
            }
        }
    };

    //Create our MeTTa runtime environment
    let mut metta = MettaShim::new(metta_working_dir, cli_args.include_paths);

    //Init our runtime environment
    let repl_params = ReplParams::new(&metta);

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
    if let Some(metta_file) = &cli_args.file {

        //Only print the output from the primary .metta file
        let metta_code = std::fs::read_to_string(metta_file)?;
        metta.exec(metta_code.as_str());
        metta.print_result();
        Ok(())

    } else {

        //Otherwise enter interactive mode
        start_interactive_mode(repl_params, metta).map_err(|err| err.into())
    }
}

// To debug rustyline:
// RUST_LOG=rustyline=debug cargo run --example example 2> debug.log
fn start_interactive_mode(repl_params: ReplParams, mut metta: MettaShim) -> rustyline::Result<()> {

    //Run the built-in repl-init code
    metta.exec(&builtin_init_metta_code());

    //Run the repl init file
    if let Some(repl_config_metta_path) = &repl_params.repl_config_metta_path {
        let init_metta_code = std::fs::read_to_string(repl_config_metta_path)?;
        metta.exec(&init_metta_code);
    }

    let max_len = metta.get_config_int(CFG_HISTORY_MAX_LEN).unwrap_or_else(|| 500);

    //Init RustyLine
    let config = Config::builder()
        .history_ignore_space(true)
        .max_history_size(max_len as usize).unwrap()
        .history_ignore_dups(false).unwrap()
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();
    let helper = ReplHelper::new(metta);
    let mut rl = Editor::with_config(config)?;

    //QUESTION: Should we provide a config to use vi key bindings vs. Emacs?

    rl.set_helper(Some(helper));
    //KEY BEHAVIOR: Enter and ctrl-M will add a newline when the cursor is in the middle of a line, while
    // ctrl-J will submit the line.
    //TODO: Document this behavior in the README.md, and possibly other key bindings also
    rl.bind_sequence(KeyEvent( KeyCode::Enter, Modifiers::NONE ), Cmd::AcceptOrInsertLine {
        accept_in_the_middle: false,
    });
    rl.bind_sequence(KeyEvent::ctrl('j'), EventHandler::Conditional(Box::new(EnterKeyHandler::new(rl.helper().unwrap().force_submit.clone()))));
    rl.bind_sequence(KeyEvent::alt('n'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyEvent::alt('p'), Cmd::HistorySearchBackward);
    if let Some(history_path) = &repl_params.history_file {
        if rl.load_history(history_path).is_err() {
            println!("No previous history found.");
        }
    }

    //The Interpreter Loop
    loop {

        //Set the prompt based on the MeTTa pragma settings
        let prompt = {
            let helper = rl.helper_mut().unwrap();
            let mut metta = helper.metta.borrow_mut();
            let prompt = metta.get_config_string(CFG_PROMPT).expect("Fatal Error: Invalid REPL config");
            let styled_prompt = metta.get_config_string(CFG_STYLED_PROMPT).unwrap_or_else(|| format!("\x1b[1;32m{prompt}\x1b[0m")); //TODO, Let this default be set inside builtin_init_metta_code() when strings can be parsed with escape chars
            helper.colored_prompt = styled_prompt;
            prompt
        };

        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                let mut metta = rl.helper().unwrap().metta.borrow_mut();
                metta.exec(line.as_str());
                metta.print_result();
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

    if let Some(history_path) = &repl_params.history_file {
        rl.append_history(history_path)?
    }

    Ok(())
}

struct EnterKeyHandler {
    force_submit: Arc<Mutex<bool>>
}

impl EnterKeyHandler {
    fn new(force_submit: Arc<Mutex<bool>>) -> Self {
        Self {
            force_submit
        }
    }
}

impl ConditionalEventHandler for EnterKeyHandler {
    fn handle(
        &self,
        _evt: &Event,
        _n: RepeatCount,
        _positive: bool,
        _ctx: &EventContext<'_>
    ) -> Option<Cmd> {
        *self.force_submit.lock().unwrap() = true;
        Some(Cmd::AcceptOrInsertLine {
            accept_in_the_middle: true,
        })
    }
}
