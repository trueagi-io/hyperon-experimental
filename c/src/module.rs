use hyperon::metta::runner::{Metta, RunContext};
use hyperon::metta::runner::modules::{MettaMod, ModuleLoader};

use crate::util::*;
use crate::metta::*;

use std::os::raw::*;

/// @brief MettaMod C API wrapper
/// @ingroup module_group
///
#[repr(C)]
pub struct metta_mod_ref_t<'a> {
    module: *const RustMettaModRef<'a>,
}

/// This structure is required to remove notice of MettaMod from C header file
#[allow(dead_code)]
struct RustMettaModRef<'a>(&'a MettaMod);

impl<'a> From<&'a MettaMod> for metta_mod_ref_t<'a> {
    fn from(module: &'a MettaMod) -> Self {
        let module = Box::into_raw(Box::new(RustMettaModRef(module)));
        Self { module }
    }
}

/// @brief Returns tokenizer of the MeTTa module
/// @ingroup module_group
/// @param[in]  mod_ref  A pointer to the `metta_mod_ref_t`
/// @return tokenizer_t instance
///
#[no_mangle]
pub extern "C" fn metta_mod_ref_tokenizer(mod_ref: *const metta_mod_ref_t) -> tokenizer_t {
    let mod_ref = unsafe{ &*(*mod_ref).module };
    mod_ref.0.tokenizer().clone().into()
}

/// @brief A C representation of the Rust [ModuleLoader] interface. User can
/// provide [ModuleLoader] methods implemented in C.
/// @ingroup module_group
///
#[repr(C)] 
pub struct module_loader_t {
    /// @brief A function to load the module my making MeTTa API calls.
    /// @param[in]  loader  The module loader self pointer
    /// @param[in]  run_context  The `run_context_t` to provide access to the MeTTa run interface
    /// @return 0 if success, non-zero otherwise; code should put the explanation text
    /// to the `err` field.
    load: Option<extern "C" fn(loader: *mut c_void, context: *mut run_context_t) -> isize>,
    /// @brief Loads module's tokens into target module. This method is used for both
    /// initial token loading and exporting module's tokens into importing
    /// module.
    /// @param[in]  loader  The module loader self pointer
    /// @param[in]  target  The module to load tokens into
    /// @param[in]  metta  The context MeTTa runner
    /// @return 0 if success, non-zero otherwise; code should put the explanation text
    /// to the `err` field.
    load_tokens: Option<extern "C" fn(loader: *mut c_void, target: metta_mod_ref_t, metta: metta_t) -> isize>,
    /// @brief Prints module loader content as a string, used for implementing
    /// [std::fmt::Debug].
    /// @param[in]  loader  The module loader self pointer
    /// @param[in]  write  Object to write the text into
    to_string: Option<extern "C" fn(loader: *mut c_void, write: write_t)>,
    /// @brief Frees module loader and all associated memory
    /// @param[in]  loader  The module loader self pointer
    free: Option<extern "C" fn(loader: *mut c_void)>,
    /// @brief Field which contains last happened error as UTF-8 string. This
    /// memory should also be cleaned up by [free] function.
    err: *const c_char,
}

/// The wrapper of the module_loader_t providing Rust API of the C implementation
pub struct CModuleLoader {
    ptr: *mut module_loader_t,
}

//FUTURE TODO.  See QUESTION around CFsModFmtLoader about whether we trust the C plugins to be reentrant
unsafe impl Send for CModuleLoader {}
unsafe impl Sync for CModuleLoader {}

impl CModuleLoader {
    /// Create new Rust API for the C module loader object
    pub fn new(cloader: *mut module_loader_t) -> Self {
        assert!(!cloader.is_null(), "ModuleLoader::load() implementation is required");
        Self{ ptr: cloader }
    }

    fn result(&self, rc: isize) -> Result<(), String> {
        if rc == 0 {
            Ok(())
        } else {
            if self.cloader().err.is_null() {
                Err("Unexpected error while loading tokens".into())
            } else {
                Err(cstr_into_string(self.cloader().err))
            }
        }
    }
    fn this(&self) -> *mut c_void {
        self.ptr.cast()
    }
    fn cloader(&self) -> &module_loader_t {
        unsafe{ &*self.ptr }
    }
}

impl Drop for CModuleLoader {
    fn drop(&mut self) {
        match self.cloader().free {
            Some(free) => free(self.this()),
            None => {},
        }
    }
}

impl std::fmt::Debug for CModuleLoader {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.cloader().to_string {
            Some(to_string) => CWrite::new(f).with(|w| to_string(self.this(), w.into())),
            None => write!(f, "CModuleLoader-{:?}", self.ptr),
        }
    }
}

impl ModuleLoader for CModuleLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let rc = (self.cloader().load.unwrap())(self.this(), &mut context.into());
        self.result(rc)
    }

    fn load_tokens(&self, target: &MettaMod, metta: Metta) -> Result<(), String> {
        match self.cloader().load_tokens {
            Some(load_tokens) => {
                let rc = load_tokens(self.this(), target.into(), metta.into());
                self.result(rc)
            },
            None => Ok(()),
        }
    }
}
