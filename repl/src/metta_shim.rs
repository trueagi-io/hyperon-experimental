
use std::path::PathBuf;

use hyperon::ExpressionAtom;
use hyperon::Atom;
use hyperon::space::*;
use hyperon::space::grounding::GroundingSpace;
use hyperon::metta::environment::Environment;
use hyperon::metta::runner::{Metta, atom_is_error};
#[cfg(not(feature = "minimal"))]
use hyperon::metta::runner::stdlib::register_rust_tokens;
#[cfg(feature = "minimal")]
use hyperon::metta::runner::stdlib2::register_rust_tokens;
use hyperon::metta::text::Tokenizer;
use hyperon::metta::text::SExprParser;
use hyperon::common::shared::Shared;

use crate::SIGINT_RECEIVED_COUNT;

/// MettaShim is responsible for **ALL** calls between the repl and MeTTa, and is in charge of keeping
/// Python happy (and perhaps other languages in the future).
///
/// Because so much functionality can be extended with Python, MettaShim must handle many operations
/// you might not expect, such as rendering atoms to text or even dropping atoms
///
pub struct MettaShim {
    pub metta: Metta,
    pub result: Vec<Vec<Atom>>,
}

#[macro_export]
macro_rules! metta_shim_env {
    ( $body:block ) => {
        {
            #[cfg(feature = "python")]
            {
                use pyo3::prelude::*;
                Python::with_gil(|_py| -> PyResult<()> {
                    $body
                    Ok(())
                }).unwrap();
            }
            #[cfg(not(feature = "python"))]
            {
                $body
            }
        }
    };
}

impl Drop for MettaShim {
    fn drop(&mut self) {
        metta_shim_env!{{
            self.result = vec![];
        }}
    }
}

impl MettaShim {

    pub fn new() -> Self {

        //Init the MeTTa interpreter
        let space = DynSpace::new(GroundingSpace::new());
        let tokenizer = Shared::new(Tokenizer::new());
        let mut new_shim = Self {
            metta: Metta::from_space(space, tokenizer, Environment::platform_env().modules_search_paths().map(|path| path.into()).collect()),
            result: vec![],
        };

        //Init HyperonPy if the repl includes Python support
        #[cfg(feature = "python")]
        if let Err(err) = || -> Result<(), String> {
            //Confirm the hyperonpy version is compatible
            py_mod_loading::confirm_hyperonpy_version(">=0.1.0, <0.2.0")?;

            //Load the hyperonpy Python stdlib
            py_mod_loading::load_python_module(&new_shim.metta, "hyperon.stdlib")?;

            Ok(())
        }() {
            eprintln!("Fatal Error: {err}");
            std::process::exit(-1);
        }

        //Load the Rust stdlib
        register_rust_tokens(&new_shim.metta);
        new_shim.load_metta_module("stdlib".into());

        //Add the extend-py! token, if we have Python support
        #[cfg(feature = "python")]
        {
            let extendpy_atom = Atom::gnd(py_mod_loading::ImportPyOp{metta: new_shim.metta.clone()});
            new_shim.metta.tokenizer().borrow_mut().register_token_with_regex_str("extend-py!", move |_| { extendpy_atom.clone() });
        }

        //extend-py! should throw an error if we don't
        #[cfg(not(feature = "python"))]
        new_shim.metta.tokenizer().borrow_mut().register_token_with_regex_str("extend-py!", move |_| { Atom::gnd(py_mod_err::ImportPyErr) });

        //Run the init.metta file
        if let Some(init_meta_file) = Environment::platform_env().initialization_metta_file_path() {
            new_shim.load_metta_module(init_meta_file.into());
        }

        new_shim
    }

    pub fn load_metta_module(&mut self, module: PathBuf) {
        metta_shim_env!{{
            self.metta.load_module(module).unwrap();
        }}
    }

    pub fn exec(&mut self, line: &str) {
        metta_shim_env!{{
            let mut parser = SExprParser::new(line);
            let mut runner_state = self.metta.start_run();

            // This clears any leftover count that might have happened if the user pressed Ctrl+C just after MeTTa
            // interpreter finished processing, but before control returned to rustyline's prompt.  That signal is
            // not intended for the new execution we are about to begin.
            //See https://github.com/trueagi-io/hyperon-experimental/pull/419#discussion_r1315598220 for more details
            *SIGINT_RECEIVED_COUNT.lock().unwrap() = 0;

            while !runner_state.is_complete() {
                //If we received an interrupt, then clear it and break the loop
                let mut signal_received_cnt = SIGINT_RECEIVED_COUNT.lock().unwrap();
                if *signal_received_cnt > 0 {
                    *signal_received_cnt = 0;
                    break;
                }
                drop(signal_received_cnt);

                //Run the next step
                self.metta.run_step(&mut parser, &mut runner_state)
                    .unwrap_or_else(|err| panic!("Unhandled MeTTa error: {}", err));
                self.result = runner_state.intermediate_results().clone();
            }
        }}
    }

    pub fn inside_env<F: FnOnce(&mut MettaShim)>(&mut self, func: F) {
        metta_shim_env!{{
            func(self)
        }}
    }

    pub fn get_config_atom(&mut self, config_name: &str) -> Option<Atom> {
        self.exec(&format!("!(get-state {config_name})"));

        #[allow(unused_assignments)]
        let mut result = None;
        metta_shim_env!{{
            result = self.result.get(0)
                .and_then(|vec| vec.get(0))
                .and_then(|atom| (!atom_is_error(atom)).then_some(atom))
                .cloned()
        }}
        result
    }

    pub fn get_config_string(&mut self, config_name: &str) -> Option<String> {
        let atom = self.get_config_atom(config_name)?;

        #[allow(unused_assignments)]
        let mut result = None;
        metta_shim_env!{{
            //TODO: We need to do atom type checking here
            result = Some(Self::strip_quotes(atom.to_string()));
        }}
        result
    }

    /// A utility function to return the part of a string in between starting and ending quotes
    // TODO: Roll this into a stdlib grounded string module, maybe as a test case for
    //   https://github.com/trueagi-io/hyperon-experimental/issues/351
    fn strip_quotes(the_string: String) -> String {
        if let Some(first) = the_string.chars().next() {
            if first == '"' {
                if let Some(last) = the_string.chars().last() {
                    if last == '"' {
                        if the_string.len() > 1 {
                            return String::from_utf8(the_string.as_bytes()[1..the_string.len()-1].to_vec()).unwrap();
                        }
                    }
                }
            }
        }
        the_string
    }

    pub fn get_config_expr_vec(&mut self, config_name: &str) -> Option<Vec<String>> {
        let atom = self.get_config_atom(config_name)?;
        let mut result = None;
        metta_shim_env!{{
            if let Ok(expr) = ExpressionAtom::try_from(atom) {
                result = Some(expr.into_children()
                    .into_iter()
                    .map(|atom| {
                        //TODO: We need to do atom type checking here
                        Self::strip_quotes(atom.to_string())
                    })
                    .collect())
            }
        }}
        result
    }

    pub fn get_config_int(&mut self, _config_name: &str) -> Option<isize> {
        None //TODO.  Make this work when I have reliable value atom bridging
    }
}

#[cfg(not(feature = "python"))]
mod py_mod_err {
    use std::fmt::Display;
    use hyperon::Atom;
    use hyperon::atom::{Grounded, ExecError, match_by_equality};
    use hyperon::matcher::MatchResultIter;
    use hyperon::metta::*;

    #[derive(Clone, PartialEq, Debug)]
    pub struct ImportPyErr;

    impl Display for ImportPyErr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "extend-py!")
        }
    }

    impl Grounded for ImportPyErr {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Err(ExecError::from("extend-py! not available in metta repl without Python support"))
        }

        fn match_(&self, other: &Atom) -> MatchResultIter {
            match_by_equality(self, other)
        }
    }
}

#[cfg(feature = "python")]
mod py_mod_loading {
    use std::fmt::Display;
    use std::str::FromStr;
    use std::path::PathBuf;
    use pep440_rs::{parse_version_specifiers, Version};
    use pyo3::prelude::*;
    use pyo3::types::{PyTuple, PyDict};
    use hyperon::*;
    use hyperon::Atom;
    use hyperon::atom::{Grounded, ExecError, match_by_equality};
    use hyperon::matcher::MatchResultIter;
    use hyperon::metta::*;
    use hyperon::metta::environment::Environment;
    use hyperon::metta::runner::Metta;

    /// Load the hyperon module, and get the "__version__" attribute
    pub fn get_hyperonpy_version() -> Result<String, String> {
        Python::with_gil(|py| -> PyResult<String> {
            let hyperon_mod = PyModule::import(py, "hyperon")?;
            let version_obj = hyperon_mod.getattr("__version__")?;
            Ok(version_obj.str()?.to_str()?.into())
        }).map_err(|err| {
            format!("{err}")
        })
    }

    pub fn confirm_hyperonpy_version(req_str: &str) -> Result<(), String> {

        let req = parse_version_specifiers(req_str).unwrap();
        let version_string = get_hyperonpy_version()?;
        //NOTE: Version parsing errors will be encountered by users building hyperonpy from source with an abnormal configuration
        // Therefore references to the "hyperon source directory" are ok.  Users who get hyperonpy from PyPi won't hit this issue
        let version = Version::from_str(&version_string)
            .map_err(|_e| format!("Invalid HyperonPy version found: '{version_string}'.\nPlease update the package by running `python -m pip install -e ./python[dev]` from your hyperon source directory."))?;
        if req.iter().all(|specifier| specifier.contains(&version)) {
            Ok(())
        } else {
            Err(format!("MeTTa repl requires HyperonPy version matching '{req_str}'.  Found version: '{version}'"))
        }
    }

    pub fn load_python_module_from_mod_or_file(metta: &Metta, module_name: &str) -> Result<(), String> {

        // First, see if the module is already registered with Python
        match load_python_module(metta, module_name) {
            Err(_) => {
                // If that failed, try and load the module from a file

                //Check each include directory in order, until we find the module we're looking for
                let file_name = PathBuf::from(module_name).with_extension("py");
                let mut found_path = None;
                for include_path in Environment::platform_env().modules_search_paths() {
                    let path = include_path.join(&file_name);
                    if path.exists() {
                        found_path = Some(path);
                        break;
                    }
                }

                match found_path {
                    Some(path) => load_python_module_from_known_path(metta, module_name, &path),
                    None => Err(format!("Failed to load module {module_name}; could not locate file: {file_name:?}"))
                }
            }
            _ => Ok(())
        }
    }

    pub fn load_python_module_from_known_path(metta: &Metta, module_name: &str, path: &PathBuf) -> Result<(), String> {

        let code = std::fs::read_to_string(&path).or_else(|err| Err(format!("Error reading file {path:?} - {err}")))?;
        Python::with_gil(|py| -> PyResult<()> {
            let _py_mod = PyModule::from_code(py, &code, path.to_str().unwrap(), module_name)?;
            Ok(())
        }).map_err(|err| {
            format!("{err}")
        })?;

        // If we suceeded in loading the module from source, then register the MeTTa extensions
        load_python_module(metta, module_name)
    }

    pub fn load_python_module(metta: &Metta, module_name: &str) -> Result<(), String> {

        Python::with_gil(|py| -> PyResult<()> {

            // Load the module
            let py_mod = PyModule::import(py, module_name)?;

            // Clone the Rust Metta handle and turn it into a CMetta object that hyperonpy can work with
            let boxed_metta = Box::into_raw(Box::new(metta.clone()));
            let hyperonpy_mod = PyModule::import(py, "hyperonpy")?;
            let metta_class_obj = hyperonpy_mod.getattr("CMetta")?;
            let args = PyTuple::new(py, &[boxed_metta as usize]);
            let wrapped_metta = metta_class_obj.call1(args)?;

            // Init a MeTTa Python object from our CMetta
            let hyperon_mod = PyModule::import(py, "hyperon")?;
            let metta_class_obj = hyperon_mod.getattr("MeTTa")?;
            let kwargs = PyDict::new(py);
            kwargs.set_item("cmetta", wrapped_metta)?;
            let py_metta = metta_class_obj.call((), Some(kwargs))?;

            // Register all the items in the module
            for item in py_mod.dir() {
                let obj = py_mod.getattr(item.str()?)?;

                if let Ok(obj_name) = obj.getattr("__name__") {
                    if obj_name.eq("metta_register")? {
                        let args = PyTuple::new(py, &[py_metta]);
                        obj.call1(args)?;
                    }
                }
            }

            Ok(())
        }).map_err(|err| {
            format!("{err}")
        })

    }

    #[derive(Clone, PartialEq, Debug)]
    pub struct ImportPyOp {
        pub metta: Metta,
    }

    impl Display for ImportPyOp {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "extend-py!")
        }
    }

    impl Grounded for ImportPyOp {
        fn type_(&self) -> Atom {
            //TODO: The Repl std atoms should include a "RESOURCE_PATH" atom type
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = || ExecError::from("extend-py! expects a resource path argument");
            let module_path_sym_atom: &SymbolAtom = args.get(0)
                .ok_or_else(arg_error)?
                .try_into().map_err(|_| arg_error())?;

            match load_python_module_from_mod_or_file(&self.metta, module_path_sym_atom.name()) {
                Ok(()) => Ok(vec![]),
                Err(err) => Err(ExecError::from(err)),
            }
        }

        fn match_(&self, other: &Atom) -> MatchResultIter {
            match_by_equality(self, other)
        }
    }
}
