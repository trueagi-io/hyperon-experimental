
use std::path::PathBuf;
use std::borrow::Cow::{self, Borrowed, Owned};

use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::HistoryHinter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Cmd, CompletionType, Config, EditMode, Editor, KeyEvent};
use rustyline::{Completer, Helper, Hinter, Validator};

use anyhow::Result;
use clap::Parser;

mod metta_shim;
use metta_shim::*;

#[derive(Parser)]
#[command(version, about)]
struct CliArgs {
    /// .metta file to execute.  `metta` will run in interactive mode if no file is supplied
    file: Option<PathBuf>,

    /// Additional files to include or directories to append to the search path
    #[arg(short, long)]
    includes: Vec<PathBuf>,
}

#[derive(Helper, Completer, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
    colored_prompt: String,
}

impl Highlighter for ReplHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

fn main() -> Result<()> {
    let cli_args = CliArgs::parse();

    let mut metta = MettaShim::new();

    if let Some(metta_file) = &cli_args.file {

        //If we have a .metta file to run, then run it
        let metta_code = std::fs::read_to_string(&metta_file)?;
        metta.exec(metta_code.as_str());
        metta.inside_env(|metta| {
            for result in metta.result.iter() {
                println!("{result:?}");
            }
        });
        Ok(())

    } else {

        //Otherwise enter interactive mode
        start_interactive_mode(&mut metta).map_err(|err| err.into())
    }
}

// To debug rustyline:
// RUST_LOG=rustyline=debug cargo run --example example 2> debug.log
fn start_interactive_mode(metta: &mut MettaShim) -> rustyline::Result<()> {

    //Init RustyLine
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();
    let h = ReplHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyEvent::alt('n'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyEvent::alt('p'), Cmd::HistorySearchBackward);
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    //The Interpreter Loop
    loop {
        let p = format!("> ");
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{p}\x1b[0m");
        let readline = rl.readline(&p);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

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
    rl.append_history("history.txt")
}
