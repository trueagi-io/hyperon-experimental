
use std::fmt::Display;
use std::path::PathBuf;

use hyperon::Atom;
use hyperon::atom::{Grounded, ExecError, match_by_equality};
use hyperon::matcher::MatchResultIter;
use hyperon::space::*;
use hyperon::space::grounding::GroundingSpace;
use hyperon::metta::*;
use hyperon::metta::runner::Metta;
use hyperon::metta::runner::stdlib::register_rust_tokens;
use hyperon::metta::text::Tokenizer;
use hyperon::metta::text::SExprParser;
use hyperon::common::shared::Shared;

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
            metta: Metta::from_space_cwd(space, tokenizer, std::env::current_dir().unwrap()),
            result: vec![]
        };

        //Load the hyperonpy Python stdlib, if the repl includes Python support
        #[cfg(feature = "python")]
        py_mod_loading::load_python_module(&new_shim.metta, "hyperon.stdlib").unwrap();

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
        new_shim.metta.tokenizer().borrow_mut().register_token_with_regex_str("extend-py!", move |_| { Atom::gnd(ImportPyErr) });

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
            self.result = self.metta.run(&mut parser).unwrap();
        }}
    }

    pub fn inside_env<F: FnOnce(&mut MettaShim)>(&mut self, func: F) {
        metta_shim_env!{{
            func(self)
        }}
    }
}

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

#[cfg(feature = "python")]
mod py_mod_loading {
    use std::fmt::Display;
    use std::path::PathBuf;
    use pyo3::prelude::*;
    use pyo3::types::{PyTuple, PyDict};
    use hyperon::*;
    use hyperon::Atom;
    use hyperon::atom::{Grounded, ExecError, match_by_equality};
    use hyperon::matcher::MatchResultIter;
    use hyperon::metta::*;
    use hyperon::metta::runner::Metta;

    pub fn load_python_module_from_mod_or_file(metta: &Metta, module_name: &str) -> Result<(), String> {

        // First, see if the module is already registered with Python
        match load_python_module(metta, module_name) {
            Err(_) => {
                // If that failed, try and load the module from a file
                //TODO: Check every import dir
                let path = PathBuf::from(module_name).with_extension("py");
                if path.exists() {
                    let code = std::fs::read_to_string(&path).or_else(|err| Err(format!("Error reading file {path:?} - {err}")))?;
                    Python::with_gil(|py| -> PyResult<()> {
                        let _py_mod = PyModule::from_code(py, &code, path.to_str().unwrap(), module_name)?;
                        Ok(())
                    }).map_err(|err| {
                        format!("{err}")
                    })?;

                    // If we suceeded in loading the module from source, then register the MeTTa extensions
                    load_python_module(metta, module_name)
                } else {
                    Err(format!("Failed to load module {module_name}; could not locate file: {path:?}"))
                }
            }
            _ => Ok(())
        }
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
        pub metta: Metta
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