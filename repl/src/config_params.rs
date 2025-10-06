
use std::path::PathBuf;
use std::io::Write;
use std::fs;

use crate::MettaShim;

const DEFAULT_REPL_METTA: &[u8] = include_bytes!("repl.default.metta");

pub const CFG_PROMPT: &str = "&ReplPrompt";
pub const CFG_STYLED_PROMPT: &str = "&ReplStyledPrompt";
pub const CFG_BRACKET_STYLES: &str = "&ReplBracketStyles";
pub const CFG_COMMENT_STYLE: &str = "&ReplCommentStyle";
pub const CFG_VARIABLE_STYLE: &str = "&ReplVariableStyle";
pub const CFG_SYMBOL_STYLE: &str = "&ReplSymbolStyle";
pub const CFG_STRING_STYLE: &str = "&ReplStringStyle";
pub const CFG_ERROR_STYLE: &str = "&ReplErrorStyle";
pub const CFG_BRACKET_MATCH_STYLE: &str = "&ReplBracketMatchStyle";
pub const CFG_BRACKET_MATCH_ENABLED: &str = "&ReplBracketMatchEnabled";
pub const CFG_HISTORY_MAX_LEN: &str = "&ReplHistoryMaxLen";

#[derive(Default, Debug)]
pub struct ReplParams {
    /// A path to the repl.metta file that's run to configure the repl environment
    pub repl_config_metta_path: Option<PathBuf>,

    /// A file for previous statements in the interactive repl
    pub history_file: Option<PathBuf>,
}

impl ReplParams {
    pub fn new(shim: &MettaShim) -> Self {

        if let Some(config_dir) = shim.config_dir() {

            //Create the default repl.meta file, if it doesn't already exist
            let repl_config_metta_path = config_dir.join("repl.metta");
            if !repl_config_metta_path.exists() {
                let mut file = fs::OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(&repl_config_metta_path)
                    .expect(&format!("Error creating default repl config file at {repl_config_metta_path:?}"));
                file.write_all(&DEFAULT_REPL_METTA).unwrap();
            }

            Self {
                repl_config_metta_path: Some(repl_config_metta_path),
                history_file: Some(config_dir.join("history.txt")),
            }
        } else {
            Self {
                repl_config_metta_path: None,
                history_file: None,
            }
        }
    }

}

/// Returns the MeTTa code to init the Repl's MeTTa params and set them to default values
pub fn builtin_init_metta_code() -> String {
    format!(r#"
        ; !(bind! {CFG_HISTORY_MAX_LEN} (new-state 500)) ; TODO, enable this when value-bridging is implemented
        !(bind! {CFG_PROMPT} (new-state "> "))
        ; !(bind! {CFG_STYLED_PROMPT} (new-state (concat "\x1b[1;32m" (get-state {CFG_PROMPT}) "\x1b[0m"))) ;TODO, Need string stdlib type with concat operation
        !(bind! {CFG_STYLED_PROMPT} (new-state "\x1b[1;32m> \x1b[0m")) 
        !(bind! {CFG_BRACKET_STYLES} (new-state ("94" "93" "95" "96")))
        !(bind! {CFG_COMMENT_STYLE} (new-state "32"))
        !(bind! {CFG_VARIABLE_STYLE} (new-state "33"))
        !(bind! {CFG_SYMBOL_STYLE} (new-state "34"))
        !(bind! {CFG_STRING_STYLE} (new-state "31"))
        !(bind! {CFG_ERROR_STYLE} (new-state "91"))
        !(bind! {CFG_BRACKET_MATCH_STYLE} (new-state "1;7"))
        ; !(bind! {CFG_BRACKET_MATCH_ENABLED} (new-state True)) ; TODO, enable this when value-bridging is implemented
    "#)
}
