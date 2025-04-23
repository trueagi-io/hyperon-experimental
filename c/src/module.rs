use hyperon::metta::runner::{Metta, RunContext};
use hyperon::metta::runner::modules::{MettaMod, ModuleLoader, ModId, ResourceKey, Resource};
use hyperon::metta::runner::pkg_mgmt::{FsModuleFormat, ModuleDescriptor};

use crate::util::*;
use crate::metta::*;

use std::os::raw::*;
use std::path::{Path, PathBuf};

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
    /// @param[in]  payload  The module loader self pointer
    /// @param[in]  run_context  The `run_context_t` to provide access to the MeTTa run interface
    /// @return 0 if success, non-zero otherwise; code should put the explanation text
    /// to the `err` field.
    load: Option<extern "C" fn(payload: *mut c_void, context: *mut run_context_t) -> isize>,
    /// @brief Loads module's tokens into target module. This method is used for both
    /// initial token loading and exporting module's tokens into importing
    /// module.
    /// @param[in]  payload  The module loader self pointer
    /// @param[in]  target  The module to load tokens into
    /// @param[in]  metta  The context MeTTa runner
    /// @return 0 if success, non-zero otherwise; code should put the explanation text
    /// to the `err` field.
    load_tokens: Option<extern "C" fn(payload: *mut c_void, target: metta_mod_ref_t, metta: metta_t) -> isize>,
    /// @brief Prints module loader content as a string, used for implementing
    /// [std::fmt::Debug].
    /// @param[in]  payload  The module loader self pointer
    /// @param[in]  write  Object to write the text into
    to_string: Option<extern "C" fn(payload: *mut c_void, write: write_t)>,
    /// @brief Frees module loader and all associated memory
    /// @param[in]  payload  The module loader self pointer
    free: Option<extern "C" fn(payload: *mut c_void)>,
    /// @brief Field which contains last happened error as UTF-8 string. This
    /// memory should also be cleaned up by [free] function.
    err: *const c_char,
}

/// The wrapper of the module_loader_t providing Rust API of the C implementation
pub struct CModuleLoader {
    payload: *mut module_loader_t,
}

//FUTURE TODO.  See QUESTION around CFsModFmtLoader about whether we trust the C plugins to be reentrant
unsafe impl Send for CModuleLoader {}
unsafe impl Sync for CModuleLoader {}

impl CModuleLoader {
    /// Create new Rust API for the C module loader object
    pub fn new(cloader: *mut module_loader_t) -> Self {
        assert!(!cloader.is_null(), "ModuleLoader::load() implementation is required");
        Self{ payload: cloader }
    }

    fn result(&self, rc: isize) -> Result<(), String> {
        if rc == 0 {
            Ok(())
        } else {
            if self.reference().err.is_null() {
                Err("Unexpected error while loading tokens".into())
            } else {
                Err(cstr_into_string(self.reference().err))
            }
        }
    }

    fn payload(&self) -> *mut c_void {
        self.payload.cast()
    }

    fn reference(&self) -> &module_loader_t {
        unsafe{ &*self.payload }
    }
}

impl Drop for CModuleLoader {
    fn drop(&mut self) {
        match self.reference().free {
            Some(free) => free(self.payload()),
            None => {},
        }
    }
}

impl std::fmt::Debug for CModuleLoader {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.reference().to_string {
            Some(to_string) => CWrite::new(f).with(|w| to_string(self.payload(), w.into())),
            None => write!(f, "CModuleLoader-{:?}", self.payload),
        }
    }
}

impl ModuleLoader for CModuleLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let rc = (self.reference().load.unwrap())(self.payload(), &mut context.into());
        self.result(rc)
    }

    fn load_tokens(&self, target: &MettaMod, metta: Metta) -> Result<(), String> {
        match self.reference().load_tokens {
            Some(load_tokens) => {
                let rc = load_tokens(self.payload(), target.into(), metta.into());
                self.result(rc)
            },
            None => Ok(()),
        }
    }
}

/// @brief Identifies the properties on a specific module, including its name and version
/// @ingroup module_group
/// @note `module_descriptor_t` objects must be freed with `module_descriptor_free` or passed to
///    a function that assumes responsibility for freeing them
///
#[repr(C)]
pub struct module_descriptor_t {
    /// Internal.  Should not be accessed directly
    descriptor: *mut RustModuleDescriptor,
}

#[derive(Clone)]
#[allow(dead_code)] //LP-TODO-NEXT.  Currently we don't do much with module_descriptor_t, but that will change when I make C bindings for the catalog API
enum RustModuleDescriptor {
    Descriptor(ModuleDescriptor),
    Err(String)
}

impl From<ModuleDescriptor> for module_descriptor_t {
    fn from(descriptor: ModuleDescriptor) -> Self {
        Self{ descriptor: Box::into_raw(Box::new(RustModuleDescriptor::Descriptor(descriptor))) }
    }
}

//LP-TODO-NEXT.  The "interpreter" documentation group is bloating.  Should break out a separate "runner" group
//LP-TODO-NEXT.  The Error pathway of the module_descriptor_t is no longer used.  So I can simplify this type
//LP-TODO-NEXT.  Add an error pathway to run_context_t, that mirrors the error pathway in metta_t, so calls where a run_context_t is passed can propagate errors back to the caller
//LP-TODO-Next.  Add an error-status result to the callbacks.  At a conceptual level, this error type fundamentally the same exec_error_t / ExecError

impl module_descriptor_t {
    fn new_err(err: String) -> Self {
        Self{ descriptor: Box::into_raw(Box::new(RustModuleDescriptor::Err(err))) }
    }
    fn new_from_enum(rust_enum: RustModuleDescriptor) -> Self {
        Self{ descriptor: Box::into_raw(Box::new(rust_enum)) }
    }
    fn into_rust_enum(self) -> RustModuleDescriptor {
        unsafe{ *Box::from_raw(self.descriptor) }
    }
    fn borrow_rust_enum(&self) -> &RustModuleDescriptor {
        unsafe{ &*self.descriptor }
    }
    //LP-TODO-NEXT: probably dead code.  Delete soon
    // fn borrow(&self) -> &ModuleDescriptor {
    //     match unsafe{ &*self.descriptor } {
    //         RustModuleDescriptor::Descriptor(desc) => desc,
    //         RustModuleDescriptor::Err(_err) => panic!("Fatal error.  Attempt to access Error module_descriptor_t")
    //     }
    // }
}

/// @brief Identifies a loaded module inside a specific `metta_t` MeTTa runner
/// @ingroup module_group
/// @note It is not necessary to free `module_id_t` types
///
#[repr(C)]
pub struct module_id_t {
    /// Internal.  Should not be accessed directly
    id: usize,
}

impl From<ModId> for module_id_t {
    fn from(mod_id: ModId) -> Self {
        module_id_t{ id: mod_id.0 }
    }
}

impl module_id_t {
    pub fn into_inner(self) -> ModId {
        ModId(self.id)
    }
}

/// @brief Returns `true` is a module_id_t is valid, otherwise returns `false`
/// @ingroup module_group
/// @param[in]  mod_id  A pointer to the `module_id_t` to test for validity
/// @return `true` if the module_id_t is valid, otherwise returns `false`
///
#[no_mangle]
pub extern "C" fn module_id_is_valid(mod_id: *const module_id_t) -> bool {
    let mod_id = unsafe{ &*mod_id };
    ModId(mod_id.id) != ModId::INVALID
}

/// @brief Creates a new module_descriptor_t with the specified name
/// @ingroup module_group
/// @param[in]  name  A C-style string containing the name of the module
/// @return The new `module_descriptor_t`
/// @note The returned `module_descriptor_t` must be freed with `module_descriptor_free()`
///
#[no_mangle]
pub extern "C" fn module_descriptor_new(name: *const c_char) -> module_descriptor_t {
    //TODO-NEXT: We should probably take a version string, and parse it into a semver version
    ModuleDescriptor::new(cstr_as_str(name).to_string(), None, None).into()
}

/// @brief Creates a new module_descriptor_t that represents the error attempting to interpret a module
/// @ingroup module_group
/// @param[in]  err_str  A C-style string containing the error message
/// @return The new error `module_descriptor_t`
///
#[no_mangle]
pub extern "C" fn module_descriptor_error(err_str: *const c_char) -> module_descriptor_t {
    module_descriptor_t::new_err(cstr_as_str(err_str).to_string()).into()
}

/// @brief Creates a new module_descriptor_t that is a clone of the argument passed
/// @ingroup module_group
/// @param[in]  descriptor  The `module_descriptor_t` to clone
/// @return The new `module_descriptor_t`
/// @note The returned `module_descriptor_t` must be freed with `module_descriptor_free()`
///
#[no_mangle]
pub extern "C" fn module_descriptor_clone(descriptor: *const module_descriptor_t) -> module_descriptor_t {
    let rust_enum = unsafe{ &*descriptor }.borrow_rust_enum();
    module_descriptor_t::new_from_enum(rust_enum.clone())
}

/// @brief Frees a module_descriptor_t
/// @ingroup module_group
/// @param[in]  descriptor  The `module_descriptor_t` to free
///
#[no_mangle]
pub extern "C" fn module_descriptor_free(descriptor: module_descriptor_t) {
    let descriptor = descriptor.into_rust_enum();
    drop(descriptor);
}


/// @struct mod_file_fmt_api_t
/// @brief A table of functions to load MeTTa modules from an arbitrary format
/// @ingroup module_group
/// @warning All of the functions in this interface may be called from threads outside the main
///    thread, and may be called concurrently.  Therefore these functions must be fully reentrant.
///
#[repr(C)]
pub struct mod_file_fmt_api_t {

    /// @brief Constructs a path for a module with a given name that resides in a parent directory
    /// @param[in]  payload  The payload passed to `env_builder_push_fs_module_format` when the
    ///    format was initialized.  This function must not modify the payload
    /// @param[in]  parent_dir  A NULL-terminated string, representing the path to the parent directory
    ///    in the file system
    /// @param[in]  mod_name  A NULL-terminated string, representing the name of the module
    /// @param[out]  dst_buf  The buffer into which to write the output path, followed by a NULL character
    /// @param[in]  buf_size  The size of the allocated dst_buf.  This function must not overwrite the
    ///    output buffer
    /// @return the number of bytes written into the `dst_buf` by the function, including a NULL terminator
    ///    character.  If the path does not fit in the `dst_buf`, then this function should return 0
    /// @note The implementation does not need to check the validity of the returned path.  Results from
    ///    this method will be passed to `try_path` to perform validity checking
    ///
    //QUESTION: The Rust interface allows for a single module loader to check multiple path variations.
    //    That could be implemented here in the C interface but it would complicate the API functions
    //    by requiring multiple buffers to be returned, and currently it's not needed.
    path_for_name: extern "C" fn(payload: *const c_void, parent_dir: *const c_char, mod_name: *const c_char, dst_buf: *mut c_char, buf_size: usize) -> usize,

    /// @brief Tests a path in the file system to determine if a valid module resides at the path
    /// @param[in]  payload  The payload passed to `env_builder_push_fs_module_format` when the
    ///    format was initialized.  This function must not modify the payload
    /// @param[in]  path  A NULL-terminated string, representing a path in the file system to test
    /// @param[in]  mod_name  A NULL-terminated string, representing the name of the module
    /// @return any non-NULL value if the `path` contains a valid module in the format, otherwise NULL.
    ///    If a non-NULL value is returned from this function, it may be passed to the `load` function
    ///    as the `callback_context` argument.  If a non-NULL value is returned, it will eventually
    ///    trigger a call to `free_callback_context` so the returned value can be an allocated pointer.
    ///
    //TODO: This function will also be responsible for returning a module version, through an [out] arg,
    // when I add versions in the near future
    try_path: extern "C" fn(payload: *const c_void, path: *const c_char, mod_name: *const c_char) -> *mut c_void,

    /// @brief Loads the module into the runner, by making calls into the `run_context_t`
    /// @param[in]  payload  The payload passed to `env_builder_push_fs_module_format` when the
    ///    format was initialized.  This function must not modify the payload
    /// @param[in]  run_context  The `run_context_t` to provide access to the MeTTa run interface
    /// @param[in]  callback_context  The state pointer initially passed to the upstream function
    ///
    ///QUESTION: What should the error reporting pathway look like?
    ///QUESTION: Is it worth trying to unify this function prototype with the `mod_loader_callback_t` type?
    ///   The argument in favor is that they're fundamentally doing the same thing, but the function params
    ///   are different on account of having a payload, and removing the `payload` argument complicates the
    ///   API as it becomes necessary to repackage the payload inside the `callback_context`
    load: extern "C" fn(payload: *const c_void, run_context: *mut run_context_t, callback_context: *mut c_void),

    /// @brief Frees a user-defined structure that may have been allocated in `try_path`.
    /// @param[in]  callback_context  The value returned from `try_path`, if it was non-NULL
    ///
    free_callback_context: Option<extern "C" fn(callback_context: *mut c_void)>,
}

#[derive(Clone, Debug)]
pub(crate) struct CFsModFmtLoader {
    api: *const mod_file_fmt_api_t,
    payload: *const c_void,
    callback_context: *mut c_void,
    fmt_id: u64,
}

impl CFsModFmtLoader {
    pub fn new(api: *const mod_file_fmt_api_t, payload: *const c_void, fmt_id: u64) -> Self {
        Self {api, payload, callback_context: core::ptr::null_mut(), fmt_id }
    }
}

impl Drop for CFsModFmtLoader {
    fn drop(&mut self) {
        let api = unsafe{ &*self.api };

        if let Some(free_func) = api.free_callback_context {
            if !self.callback_context.is_null() {
                free_func(self.callback_context)
            }
        }
    }
}

//QUESTION: What should our multi-thread cross-language API look like?  (I am seeing this as establishing
//  a pattern we can also use for Grounded Atoms, Spaces, etc.)
//
//I see several possible approaches:
// 1.) Let the C implementations ensure their own concurrency.  This preserves the option for optimal speed,
//  but might invite problems with badly-behaved extensions.  Especially for host languages where the
//  interpreter is bound to a specific thread such as Python or JavaScript.
// 2.) Implement a single-threaded API where we transact all interactions with the host language through
//  queues to a single interface thread.  This is less rope for the user to hang themselves, but will be
//  a fundamental bottleneck, so I'd prefer not to go this route
//
//My strong opinion is we leave the C API unlimited (ie. Option 1).  Then we can implement the queueing
//  & synchronization for Python within the hyperonpy layer.  My preference would be to undertake
//  https://github.com/trueagi-io/hyperon-experimental/issues/283 before trying to implement the queueing &
//  synchronization layer.  My reasoning is that the bug-surface-area will be a lot smaller in Rust than in C++
unsafe impl Send for CFsModFmtLoader {}
unsafe impl Sync for CFsModFmtLoader {}

impl FsModuleFormat for CFsModFmtLoader {
    fn paths_for_name(&self, parent_dir: &Path, mod_name: &str) -> Vec<PathBuf> {
        let api = unsafe{ &*self.api };

        let parent_dir_c_string = str_as_cstr(parent_dir.to_str().unwrap());
        let mod_name_c_string = str_as_cstr(mod_name);
        const BUF_SIZE: usize = 512;
        let mut buffer = [0 as c_char; BUF_SIZE];

        let bytes_written = (api.path_for_name)(
            self.payload,
            parent_dir_c_string.as_ptr(),
            mod_name_c_string.as_ptr(),
            buffer.as_mut_ptr(),
            BUF_SIZE
        );
        if bytes_written > 0 {
            vec![PathBuf::from(cstr_as_str(buffer[0..=bytes_written].as_ptr()))]
        } else {
            vec![]
        }
    }
    fn try_path(&self, path: &Path, mod_name: Option<&str>) -> Option<(Box<dyn ModuleLoader>, ModuleDescriptor)> {
        let api = unsafe{ &*self.api };
        let path_c_string = str_as_cstr(path.to_str().unwrap());
        let mod_name = match mod_name {
            Some(mod_name) => mod_name,
            None => path.file_stem().unwrap().to_str().unwrap()
        };
        let mod_name_c_string = str_as_cstr(mod_name);

        let result_context = (api.try_path)(self.payload, path_c_string.as_ptr(), mod_name_c_string.as_ptr());
        if !result_context.is_null() {
            //TODO-NEXT.  We want to provide a way for the loader to support loading a PkgInfo, and also pass
            // the version from that PkgInfo when the new descriptor is created

            let descriptor = ModuleDescriptor::new_with_path_and_fmt_id(mod_name.to_string(), None, path, self.fmt_id);

            let mut new_loader = self.clone();
            new_loader.callback_context = result_context;
            Some((Box::new(new_loader), descriptor))
        } else {
            None
        }
    }
}

impl ModuleLoader for CFsModFmtLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let api = unsafe{ &*self.api };
        let mut c_context = run_context_t::from(context);

        (api.load)(self.payload, &mut c_context, self.callback_context);
        match c_context.take_err_string() {
            None => Ok(()),
            Some(err_string) => Err(err_string),
        }
    }
    fn get_resource(&self, _res_key: ResourceKey) -> Result<Resource, String> {
        //TODO, add C API for providing resources
        Err("resource not found".to_string())
    }
}

