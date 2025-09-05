
//! MettaShim is responsible for **ALL** calls between the repl and MeTTa, and is in charge of keeping
//! Python happy (and perhaps other languages in the future).
//!
//! Because so much functionality can be extended with Python, MettaShim must handle many operations
//! you might not expect, such as rendering atoms to text or even dropping atoms
//!

pub use metta_interface_mod::MettaShim;

use hyperon::metta::text::SExprParser;
use hyperon::metta::text::SyntaxNodeType;

use crate::SIGINT_RECEIVED_COUNT;

/// Prepares to enter an interruptible exec loop
pub fn exec_state_prepare() {
    // Clear any leftover count that might have happened if the user pressed Ctrl+C just after MeTTa
    // interpreter finished processing, but before control returned to rustyline's prompt.  That signal is
    // not intended for the new execution we are about to begin.
    //See https://github.com/trueagi-io/hyperon-experimental/pull/419#discussion_r1315598220 for more details
    *SIGINT_RECEIVED_COUNT.lock().unwrap() = 0;
}

/// Check whether an exec loop should break based on an interrupt, and clear the interrupt state
pub fn exec_state_should_break() -> bool {
    let mut signal_received_cnt = SIGINT_RECEIVED_COUNT.lock().unwrap();
    if *signal_received_cnt > 0 {
        *signal_received_cnt = 0;
        true
    } else {
        false
    }
}

#[cfg(feature = "python")]
pub mod metta_interface_mod {
    use std::str::FromStr;
    use std::path::PathBuf;
    use pep440_rs::{parse_version_specifiers, Version};
    use pyo3::prelude::*;
    use pyo3::types::{PyTuple, PyString, PyBool, PyList, PyDict};
    use hyperon_common::collections::VecDisplay;
    use super::{exec_state_prepare, exec_state_should_break};
    use hyperon::metta::runner::str::unescape;
    use hyperon::metta::text::CharReader;

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

    pub struct MettaShim {
        py_mod: Py<PyModule>,
        py_metta: Py<PyAny>,
        result: Vec<Vec<Py<PyAny>>>,
    }

    impl MettaShim {
        const PY_CODE: &'static str = include_str!("py_shim.py");

        pub fn new(working_dir: PathBuf, include_paths: Vec<PathBuf>) -> Self {

            match || -> Result<_, String> {
                //Confirm the hyperonpy version is compatible
                let req_str = Self::required_hyperon_version();
                confirm_hyperonpy_version(&req_str)?;

                //Initialize the Hyperon environment
                let new_shim = MettaShim::init_common_env(working_dir, include_paths)?;

                Ok(new_shim)
            }() {
                Ok(shim) => shim,
                Err(err) => {
                    eprintln!("Fatal Error: {err}");
                    std::process::exit(-1);
                }
            }
        }

        fn required_hyperon_version() -> String {
            const PACKAGE_VERSION: &str = env!("CARGO_PKG_VERSION");
            format!("=={PACKAGE_VERSION}")
        }

        pub fn init_common_env(working_dir: PathBuf, include_paths: Vec<PathBuf>) -> Result<MettaShim, String> {
            match Python::with_gil(|py| -> PyResult<(Py<PyModule>, Py<PyAny>)> {
                let py_mod = PyModule::from_code(py, Self::PY_CODE, "", "")?;
                let init_func = py_mod.getattr("init_metta")?;
                let kwargs = PyDict::new(py);
                kwargs.set_item("working_dir", working_dir)?;
                kwargs.set_item("include_paths", include_paths)?;
                let py_metta = init_func.call((), Some(kwargs))?;
                Ok((py_mod.into(), py_metta.into()))
            }) {
                Err(err) => Err(format!("{err}")),
                Ok((py_mod, py_metta)) => Ok(Self { py_mod, py_metta, result: vec![] }),
            }
        }

        pub fn exec<R: Iterator<Item=std::io::Result<char>>, I: Into<CharReader<R>>>(&mut self, input: I) {
            // TODO: should be replaced by wrapping Rust CharReader into Python API
            let line: String = input.into().map(|r| r.expect("Error while reading input")).collect();

            //Initialize the runner state
            let runner_state = Python::with_gil(|py| -> PyResult<Py<PyAny>> {
                let line = PyString::new(py, &line);
                let py_metta = self.py_metta.as_ref(py);
                let module: &PyModule = self.py_mod.as_ref(py);
                let runner_class = module.getattr("RunnerState")?;
                let args = PyTuple::new(py, &[py_metta, line]);
                let result = runner_class.call1(args)?;
                Ok(result.into())
            }).unwrap();

            exec_state_prepare();

            loop {
                //See if we've already finished processing
                if Python::with_gil(|py| -> PyResult<bool> {
                    let module: &PyModule = self.py_mod.as_ref(py);
                    let func = module.getattr("run_is_complete")?;
                    let args = PyTuple::new(py, &[&runner_state]);
                    let result = func.call1(args)?;
                    Ok(result.downcast::<PyBool>().unwrap().is_true())
                }).unwrap() {
                    break;
                }

                //See if we should exit
                if exec_state_should_break() {
                    break;
                }

                //Run the next step
                self.result = Python::with_gil(|py| -> PyResult<Vec<Vec<Py<PyAny>>>> {
                    let module: &PyModule = self.py_mod.as_ref(py);
                    let func = module.getattr("run_step")?;
                    let args = PyTuple::new(py, &[&runner_state]);
                    let result = func.call1(args)?;
                    let results_list = result.downcast::<PyList>().unwrap();
                    let mut results: Vec<Vec<Py<PyAny>>> = vec![];
                    for result in results_list {
                        let inner_list = result.downcast::<PyList>().unwrap();
                        results.push(inner_list.iter().map(|atom| atom.into()).collect());
                    }
                    Ok(results)
                }).unwrap();
            }
        }

        pub fn print_result(&self) {
            Python::with_gil(|py| -> PyResult<()> {
                for result_vec in self.result.iter() {
                    let result_vec: Vec<&PyAny> = result_vec.iter().map(|atom| atom.as_ref(py)).collect();
                    println!("{}", VecDisplay(&result_vec));
                }
                Ok(())
            }).unwrap()
        }

        pub fn parse_line(&mut self, line: &str) -> Result<(), String> {
            Python::with_gil(|py| -> PyResult<Result<(), String>> {
                let py_line = PyString::new(py, line);
                let py_metta = self.py_metta.as_ref(py);
                let args = PyTuple::new(py, &[py_metta, py_line]);
                let module: &PyModule = self.py_mod.as_ref(py);
                let func = module.getattr("parse_line")?;
                let result = func.call1(args)?;
                Ok(if result.is_none() {
                    Ok(())
                } else {
                    Err(result.to_string())
                })
            }).unwrap()
        }

        pub fn config_dir(&self) -> Option<PathBuf> {
            Python::with_gil(|py| -> PyResult<Option<PathBuf>> {
                let module: &PyModule = self.py_mod.as_ref(py);
                let func = module.getattr("get_config_dir")?;
                let result = func.call0()?;
                Ok(if result.is_none() {
                    None
                } else {
                    Some(PathBuf::from(result.to_string()))
                })
            }).unwrap()
        }

        pub fn get_config_expr_vec(&mut self, config_name: &str) -> Option<Vec<String>> {
            Python::with_gil(|py| -> PyResult<Option<Vec<String>>> {
                let config_name = PyString::new(py, config_name);
                let py_metta = self.py_metta.as_ref(py);
                let args = PyTuple::new(py, &[py_metta, config_name]);
                let module: &PyModule = self.py_mod.as_ref(py);
                let func = module.getattr("get_config_expr_vec")?;
                let result = func.call1(args)?;

                Ok(if result.is_none() {
                    None
                } else {
                    match result.downcast::<PyList>() {
                        Ok(result_list) => {
                            Some(result_list.into_iter()
                                // String atom is expected as a value
                                .map(|atom| unescape(&atom.to_string()).unwrap())
                                .collect())
                        },
                        Err(_) => None
                    }
                })
            }).unwrap()
        }

        pub fn get_config_string(&mut self, config_name: &str) -> Option<String> {
            Python::with_gil(|py| -> PyResult<Option<String>> {
                let config_name = PyString::new(py, config_name);
                let py_metta = self.py_metta.as_ref(py);
                let args = PyTuple::new(py, &[py_metta, config_name]);
                let module: &PyModule = self.py_mod.as_ref(py);
                let func = module.getattr("get_config_string")?;
                let result = func.call1(args)?;

                Ok(if result.is_none() {
                    None
                } else {
                    // String atom is expected as a value
                    Some(unescape(&result.to_string()).unwrap())
                })
            }).unwrap()
        }

        pub fn get_config_int(&mut self, _config_name: &str) -> Option<isize> {
            None //TODO.  Make this work when I have reliable value atom bridging
        }
    }
}

/// The "no python" path involves a reimplementation of all of the MeTTa interface points calling MeTTa
/// directly instead of through Python.  Maintaining two paths is a temporary stop-gap solution because
/// we can only link the Hyperon Rust library through one pathway and the HyperonPy module is that path
/// when the Python repl is used.
///
/// When we have the ability to statically link HyperonPy, we can remove this shim and call
/// Hyperon and MeTTa from everywhere in the code.  This will likely mean we can get rid of the clumsy
/// implementations in the "python" version of metta_interface_mod.  See See https://github.com/trueagi-io/hyperon-experimental/issues/283
#[cfg(not(feature = "python"))]
pub mod metta_interface_mod {
    use std::path::{PathBuf, Path};
    use hyperon::metta::*;
    use hyperon::metta::text::{CharReader, SExprParser};
    use hyperon_atom::ExpressionAtom;
    use hyperon_atom::Atom;
    use hyperon::metta::runner::{Metta, RunnerState, Environment, EnvBuilder};
    use hyperon::metta::runner::str::Str;
    use hyperon_common::collections::VecDisplay;
    use super::{exec_state_prepare, exec_state_should_break};

    pub struct MettaShim {
        pub metta: Metta,
        pub result: Vec<Vec<Atom>>,
    }

    impl MettaShim {

        pub fn new(working_dir: PathBuf, include_paths: Vec<PathBuf>) -> Self {
            match || -> Result<_, String> {
                let new_shim = MettaShim::init_common_env(working_dir, include_paths)?;
                Ok(new_shim)
            }() {
                Ok(shim) => shim,
                Err(err) => {
                    eprintln!("Fatal Error: {err}");
                    std::process::exit(-1);
                }
            }
        }

        pub fn init_common_env(working_dir: PathBuf, include_paths: Vec<PathBuf>) -> Result<MettaShim, String> {
            let mut builder = EnvBuilder::new()
                .set_working_dir(Some(&working_dir))
                .set_default_config_dir();

            for path in include_paths.into_iter().rev() {
                builder = builder.push_include_path(path);
            }
            builder.init_common_env();

            let new_shim = MettaShim {
                metta: Metta::new(None),
                result: vec![],
            };

            Ok(new_shim)
        }

        pub fn parse_line(&mut self, line: &str) -> Result<(), String> {
            let mut parser = SExprParser::new(line);
            loop {
                let result = parser.parse(&self.metta.tokenizer().borrow());

                match result {
                    Ok(Some(_atom)) => (),
                    Ok(None) => {
                        return Ok(());
                    },
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
        }

        pub fn exec<R: Iterator<Item=std::io::Result<char>>, I: Into<CharReader<R>>>(&mut self, input: I) {
            let parser = SExprParser::new(input);
            let mut runner_state = RunnerState::new_with_parser(&self.metta, Box::new(parser));

            exec_state_prepare();

            while !runner_state.is_complete() {
                if exec_state_should_break() {
                    break;
                }

                //Run the next step
                runner_state.run_step().unwrap_or_else(|err| panic!("Unhandled MeTTa error: {}", err));
                self.result = runner_state.current_results().clone();
            }
        }

        pub fn print_result(&self) {
            for result in self.result.iter() {
                println!("{}", VecDisplay(result));
            }
        }

        pub fn config_dir(&self) -> Option<&Path> {
            Environment::common_env().config_dir()
        }

        pub fn get_config_atom(&mut self, config_name: &str) -> Option<Atom> {
            self.exec(format!("!(get-state {config_name})").as_str());
            self.result.get(0)
                .and_then(|vec| vec.get(0))
                .and_then(|atom| (!atom_is_error(atom)).then_some(atom))
                .cloned()
        }

        pub fn get_config_string(&mut self, config_name: &str) -> Option<String> {
            let atom = self.get_config_atom(config_name)?;
            Some(Str::from_atom(&atom)
                .expect(format!("String is expected as a value of {}", config_name).as_str())
                .into())
        }

        pub fn get_config_expr_vec(&mut self, config_name: &str) -> Option<Vec<String>> {
            let atom = self.get_config_atom(config_name)?;
            if let Ok(expr) = ExpressionAtom::try_from(atom) {
                Some(expr.into_children()
                    .into_iter()
                    .map(|atom| {
                        Str::from_atom(&atom)
                            .expect(format!("String is expected as a value of {}", config_name).as_str())
                            .into()
                    })
                    .collect())
            } else {
                None
            }
        }

        pub fn get_config_int(&mut self, _config_name: &str) -> Option<isize> {
            None //TODO.  Make this work when I have reliable value atom bridging
        }
    }
}

pub fn parse_and_unroll_syntax_tree(line: &str) -> Vec<(SyntaxNodeType, std::ops::Range<usize>)> {

    let mut nodes = vec![];
    let mut parser = SExprParser::new(line);
    loop {
        match parser.parse_to_syntax_tree() {
            Ok(Some(root_node)) => {
                root_node.visit_depth_first(|node| {
                    // We will only render the leaf nodes in the syntax tree
                    if !node.node_type.is_leaf() {
                        return;
                    }

                    nodes.push((node.node_type, node.src_range.clone()))
                });
            },
            Ok(None) => break,
            Err(msg) => panic!("Unexpected input error: {}", msg),
        }
    }
    nodes
}
