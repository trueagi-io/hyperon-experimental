
use std::path::{Path, PathBuf};
use std::io::Write;
use std::fs;

const DEFAULT_INIT_METTA: &[u8] = include_bytes!("init.default.metta");
const DEFAULT_REPL_METTA: &[u8] = include_bytes!("repl.default.metta");

pub const CFG_DEFAULT_PROMPT: &str = "ReplDefaultPrompt";
pub const CFG_STYLED_PROMPT: &str = "ReplStyledPrompt";
pub const CFG_BRACKET_STYLES: &str = "ReplBracketStyles";
pub const CFG_COMMENT_STYLE: &str = "ReplCommentStyle";
pub const CFG_VARIABLE_STYLE: &str = "ReplVariableStyle";
pub const CFG_SYMBOL_STYLE: &str = "ReplSymbolStyle";
pub const CFG_STRING_STYLE: &str = "ReplStringStyle";
pub const CFG_ERROR_STYLE: &str = "ReplErrorStyle";
pub const CFG_BRACKET_MATCH_STYLE: &str = "ReplBracketMatchStyle";
pub const CFG_BRACKET_MATCH_ENABLED: &str = "ReplBracketMatchEnabled";

#[derive(Default, Debug)]
pub struct ReplParams {
    /// Path to the config dir for the whole repl, in an OS-specific location
    pub config_dir: PathBuf,

    /// A path to the init.metta file that's run to customize the MeTTa environment
    pub init_metta_path: PathBuf,

    /// A path to the repl.metta file that's run to configure the repl environment
    pub repl_config_metta_path: PathBuf,

    /// Path to the dir containing the script being run, or the cwd the repl was invoked from in interactive mode
    pub metta_working_dir: PathBuf,

    /// Other include paths, specified either through command-line args or config settings
    include_paths: Vec<PathBuf>,

    /// A file for previous statements in the interactive repl
    pub history_file: Option<PathBuf>,
}

impl ReplParams {
    pub fn new(config_dir: &Path, include_paths: Vec<PathBuf>, metta_file: Option<&PathBuf>) -> Self {

        //If we have a metta_file, then the working dir is the parent of that file
        //If we are running in interactive mode, it's the working dir at the time the repl is invoked
        let metta_working_dir: PathBuf = match metta_file {
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

        //Create the modules dir inside the config dir, if it doesn't already exist
        let modules_dir = config_dir.join("modules");
        std::fs::create_dir_all(&modules_dir).unwrap();

        //Create the default init.metta file and repl.meta file, if they don't already exist
        let init_metta_path = config_dir.join("init.metta");
        if !init_metta_path.exists() {
            let mut file = fs::OpenOptions::new()
                .create(true)
                .write(true)
                .open(&init_metta_path)
                .expect(&format!("Error creating default init file at {init_metta_path:?}"));
            file.write_all(&DEFAULT_INIT_METTA).unwrap();
        }
        let repl_config_metta_path = config_dir.join("repl.metta");
        if !repl_config_metta_path.exists() {
            let mut file = fs::OpenOptions::new()
                .create(true)
                .write(true)
                .open(&repl_config_metta_path)
                .expect(&format!("Error creating default repl config file at {repl_config_metta_path:?}"));
            file.write_all(&DEFAULT_REPL_METTA).unwrap();
        }

        //Push the "modules" dir, as the last place to search after the paths specified on the cmd line
        //TODO: the config.metta file will be able to append / modify the search paths, and can choose not to
        // include the "modules" dir in the future.
        let mut include_paths = include_paths;
        include_paths.push(modules_dir);

        Self {
            config_dir: config_dir.into(),
            init_metta_path,
            repl_config_metta_path,
            metta_working_dir,
            include_paths,
            history_file: Some(config_dir.join("history.txt")),
        }
    }

    /// Returns the search paths, in order to search
    ///
    /// The metta_working_dir is always returned first
    pub fn modules_search_paths<'a>(&'a self) -> impl Iterator<Item=PathBuf> + 'a {
        [self.metta_working_dir.clone()].into_iter().chain(
            self.include_paths.iter().cloned())
    }
}
