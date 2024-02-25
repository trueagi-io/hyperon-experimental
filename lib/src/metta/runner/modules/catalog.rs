//!
//! # Module Resolution
//!
//! ## Behavior of Module Resolution
//!
//! ```ignore
//!       ┌────────────────────┐           ⎽⎼⎻⎺ ⎺⎺⎺ ⎺⎻⎼⎽                    ⎽⎼⎻⎺ ⎺⎺⎺ ⎺⎻⎼⎽
//!      ╱                    ╱       ⎽⎼⎻⎺  pkg-info in  ⎺⎻⎼⎽ Yes      ⎽⎼⎻⎺pkg-info entry ⎺⎻⎼⎽ No
//!     ╱      (import!)     ╱─────►<   &self has entry for   >─────►<   has fs_path attrib?   >───┐
//!    ╱                    ╱         ⎺⎻⎼⎽    module?    ⎽⎼⎻⎺          ⎺⎻⎼⎽               ⎽⎼⎻⎺     │
//!   └────────────────────┘               ⎺⎻⎼⎽ ⎽⎽⎽ ⎽⎼⎻⎺                    ⎺⎻⎼⎽ ⎽⎽⎽ ⎽⎼⎻⎺          │
//!                                              │ No                             │ Yes            │
//!  ┌─────────────────────────┐     ┌───────────▼─────────────┐      /───────────▼─────────────\  │
//!  │  Query ModuleCatalogs   │     │    Assume any module    │      │    Load the module at   │  │
//!  │     in order, with      │◄──┬─┤  version will satisfy   │      │   the file-system path  │  │
//!  │   version requirement   │   │ │       dependency        │      │   with first successful │  │
//!  │                         │   │ │                         │      │      FsModuleFormat     │  │
//!  └───────────┬─────────────┘   │ └───────────▲─────────────┘      \───────────▲─────────────/  │
//!              │                 │             │                                │                │
//!  /───────────▼─────────────\   │             │                    ┌───────────┴─────────────┐  │
//!  │  Load the module from   │   │             │                    │    clone module from    │  │
//!  │   the first catalog     │   │             │                    │     remote repo to      │  │
//!  │     that reports a      │   │             │                    │    local resource dir   │  │
//!  │    successful match     │   │             │                    │                         │  │
//!  \─────────────────────────/   │             │                    └───────────▲─────────────┘  │
//!                                │             │ No                             │ Yes            │
//!                                │Yes    ⎽⎼⎻⎺ ⎺⎺⎺ ⎺⎻⎼⎽                    ⎽⎼⎻⎺ ⎺⎺⎺ ⎺⎻⎼⎽          │
//!                                │  ⎽⎼⎻⎺pkg-info entry ⎺⎻⎼⎽       No ⎽⎼⎻⎺pkg-info entry ⎺⎻⎼⎽     │
//!                                └<   has version attrib?   >◄─────<     has git attrib?     >───┘
//!                                   ⎺⎻⎼⎽               ⎽⎼⎻⎺          ⎺⎻⎼⎽               ⎽⎼⎻⎺
//!                                        ⎺⎻⎼⎽ ⎽⎽⎽ ⎽⎼⎻⎺                    ⎺⎻⎼⎽ ⎽⎽⎽ ⎽⎼⎻⎺
//! ```
//!

//LP-TODO-NEXT make a test to make sure circular imports are caught and don't lead to infinite recursion
//QUESTION: Should circular imports between modules be allowed?  The current implementation (and possibly
// the MeTTa language itself) disallow circular imports because there is no concept of forward declaration.
// It *may* be possible to pre-parse the language in order to make recursive imports possible, but I have
// not yet thought in detail about this.
//

//QUESTION on shared base dependencies & sat-set solving:
//The currently implemented design resolves each module's dependencies in a straightforward depth-first
//  order.  This is possible because the module system allows multiple instances of the same module to
//  be loaded simultaneously.  So each module can pick its best dependencies based on its pkg-info and
//  the available catalogs.
//However, consider the following situation:
//  ModA depends on ModI for some interface types (for example a special String type)
//  ModB depends on ModI for the same interface types, but ModA and ModB don't know about each other
//  ModTop depends on both ModA and ModB, and uses functionality in ModA to create some objects that
//   it expects ModB to be able to use.  Therefore the system must guarantee the same version of ModI
//   gets imported by both ModA and ModB.
//This is precisely the opposite behavior from the ability of a module to carry around "private"
//  dependencies and know that those dependencies will always be loaded, and they won't be substituted
//  for another version.
//
//I see several possible solutions:
// 1.) We could disallow private dependencies altogether.  This is the approach taken by Cargo.
//  However this contravenes some of the desiderata outlined in this issue:
//  https://github.com/trueagi-io/hyperon-experimental/issues/470
// 2.) We could require explicit re-exporting of a dependency module used in a module's interface, which
//   would give the implementation an opportunity to find dependency module versions that work for
//   all other modules that use them in common.  ie. solve for the sat set.  Optionally, with this approach,
//   the module could also opt to re-export a private dependency as part of itself, making the interface
//   between ModA and ModB in the example deliberately incompatible. 
// 3.) We could require private dependencies to be explicitly specified as private in the pkg-info.  With
//  the default behavior being a single module for each module name.  This might be a reasonable compromise
//  between 1 & 2, however we would likely need some form of linting, so that a user doesn't shoot
//  themselves in the foot by exporting an interface that includes items from a private dependency
//
// I think my personal preference is for #2.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::ffi::{OsStr, OsString};

use crate::metta::text::OwnedSExprParser;
use crate::metta::runner::*;
use crate::metta::runner::modules::*;

use xxhash_rust::xxh3::xxh3_64;

/// Implemented for types capable of locating MeTTa modules
///
/// For example, `ModuleCatalog` would be an interface to a module respository, analogous to `PyPI` or
/// `crates.io` but `ModuleCatalog` is also implemented for [Path] because any file system directory may be
/// capable of storing and indexing MeTTa modules.
///
/// `ModuleCatalog` types are closely connected with [ModuleLoader] types because the `ModuleCatalog` must
/// recognize the module in whatever media it exists, and supply the `ModuleLoader` to load that module
pub trait ModuleCatalog: std::fmt::Debug + Send + Sync {
    /// Returns the [ModuleDescriptor] for every module in the `ModuleCatalog` with the specified name
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor>;

    //TODO: Add this function when I add module versioning
    // /// Returns the [ModuleDescriptor] for every module in the `ModuleCatalog` with the specified name
    // /// matching the version requirements
    // fn lookup_within_version_range(name: &str, version_range: ) -> Vec<ModuleDescriptor>;
    //TODO: provide default implementation

    //TODO: Add this function when I add module versioning
    // /// Returns the [ModuleDescriptor] for the newest module in the `ModuleCatalog`, that falls within
    // /// the specified version range, or `None` if no module exists
    // fn lookup_newest_within_version_range(name: &str, version_range: ) -> Option<ModuleDescriptor>;
    //TODO: provide default implementation

    /// Returns a [ModuleLoader] for the specified module from the `ModuleCatalog`
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String>;
}

/// The object responsible for locating and selecting dependency modules for each [MettaMod]
///
/// This structure is conceptually analogous to the a `Cargo.toml` file for a given module.
#[derive(Clone, Debug, Default)]
//TODO: Use serde to deserialize a PkgInfo from an expression atom
pub struct PkgInfo {

    /// The public name of the module.  Should be composed of alpha-numeric characters with '-' and '_'
    /// characters allowed.  Must not contain any other punctuation. 
    pub name: String,

    //TODO: version field, to indicate the version of this module

    /// If `strict == true` then a dependency must be declared in the `PkgInfo`, otherwise a permissive
    /// version requirement will be assumed for any modules that are not explicitly declared
    pub strict: bool,

    /// Entries mapping module names to requirements for each dependency sub-module
    ///
    /// A Duplicate entry for a given sub-module in the deps list is an error.
    pub deps: HashMap<String, DepEntry>,
}

/// A single entry in a [PkgInfo]'s dependencies, specifying the properties of a module that will satisfy a dependency
#[derive(Clone, Debug, Default)]
pub struct DepEntry {
    /// Indicates that the dependency module should be loaded from a specific FS path
    ///
    /// If the fs_path is specified, the other pkg_info attributes will be ignored.
    //QUESTION: We need a MeTTa "style guide" for these field names, since they are effective going
    // to be part of the API, because a PkgInfo will be deserialized from atoms
    pub fs_path: Option<PathBuf>

    //TODO: field for fetching from a specific git repo
    //TODO: field to indicate acceptable version range for dependency
}

impl PkgInfo {
    /// Resolves which module to load from which available location or catalog, and returns the [ModuleLoader] to
    /// load that module
    pub fn resolve_module<'c>(&self, context: &'c RunContext, name_path: &str) -> Result<Option<(Box<dyn ModuleLoader + 'c>, ModuleDescriptor)>, String> {
        let mod_name = mod_name_from_path(name_path);
        let normalized_mod_path = context.normalize_module_name(name_path)?;

        //Make sure the name is a legal module name
        if !module_name_is_legal(mod_name) {
            return Err(format!("Illegal module name: {mod_name}"));
        }

        //See if we have a pkg_info dep entry for the module
        if let Some(entry) = self.deps.get(mod_name) {

            //If path is explicitly specified in the dep entry, then we must load the module at the
            // specified path, and cannot search anywhere else
            if let Some(path) = &entry.fs_path {
                return loader_for_module_at_path(&context.metta, path, Some(mod_name), context.module().resource_dir());
            }

            //TODO, if git URI is specified in the dep entry, clone the repo to a location in the environment
            // dir with a unique path (based on a random uuid), and resolve it within that directory's catalog

            //TODO, If a version range is specified in the dep entry, then use that version range to specify
            // modules discovered in the catalogs

        } else {
            //If the PkgInfo doesn't have an entry for the module, it's an error if the PkgInfo is flagged as "strict"
            if self.strict {
                return Ok(None);
            }
        }

        //Search the module's resource dir before searching the environment's catalogs
        // This allows a module to import another module inside its directory or as a peer of itself for
        // single-file modules, without including an explicit PkgInfo dep entry.  On the other hand, If we
        // want to require module authors to include a dep entry to be explicit about their dependencies, we
        // can remove this catalog
        let resource_dir_catalog;
        let mut local_catalogs = vec![];
        if let Some(mod_resource_dir) = context.module().resource_dir() {
            if context.metta.environment().working_dir() != Some(mod_resource_dir) {
                resource_dir_catalog = DirCatalog::new(PathBuf::from(mod_resource_dir), context.metta().environment().fs_mod_formats.clone());
                local_catalogs.push(&resource_dir_catalog as &dyn ModuleCatalog);
            }
        }

        //Search the catalogs, starting with the resource dir, and continuing to the runner's Environment
        for catalog in local_catalogs.into_iter().chain(context.metta.environment().catalogs()) {
            log::trace!("Looking for module: \"{mod_name}\" inside {catalog:?}");
            //TODO: use lookup_newest_within_version_range, as soon as I add module versioning
            let results = catalog.lookup(mod_name);
            if results.len() > 0 {
                log::info!("Found module: \"{mod_name}\" inside {catalog:?}");
                let descriptor = results.into_iter().next().unwrap();
                log::info!("Preparing to load module: \'{}\' as \'{}\'", descriptor.name, normalized_mod_path);
                return Ok(Some((catalog.get_loader(&descriptor)?, descriptor)))
            }
        }

        Ok(None)
    }
}

/// Internal function to get a loader for a module at a specific file system path, by trying each FsModuleFormat in order
pub(crate) fn loader_for_module_at_path<P: AsRef<Path>>(metta: &Metta, path: P, name: Option<&str>, search_dir: Option<&Path>) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {

    //If the path is not an absolute path, assume it's relative to the running search_dir
    let path = if path.as_ref().is_absolute() {
        PathBuf::from(path.as_ref())
    } else {
        search_dir.ok_or_else(|| format!("Error loading {}.  Working directory or module resource dir required to load modules by relative path", path.as_ref().display()))?
            .join(path)
    };

    //If a mod name was supplied, we want to make sure it's not a full name path
    let name = match name {
        Some(name) => Some(mod_name_from_path(name)),
        None => None
    };

    //Check all module formats, to try and load the module at the path
    for fmt in metta.environment().fs_mod_formats() {
        if let Some((loader, descriptor)) = fmt.try_path(&path, name) {
            return Ok(Some((loader, descriptor)))
        }
    }

    Err(format!("No module format able to interpret module at {}", path.display()))
}

/// A loader for a MeTTa module that lives within a single `.metta` file
#[derive(Debug)]
pub(crate) struct SingleFileModule {
    path: PathBuf,
}

impl SingleFileModule {
    fn new(path: &Path) -> Self {
        Self {path: path.into() }
    }
}

impl ModuleLoader for SingleFileModule {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {

        let space = DynSpace::new(GroundingSpace::new());
        let resource_dir = self.path.parent().unwrap();
        context.init_self_module(space, Some(resource_dir.into()));

        let program_text = std::fs::read_to_string(&self.path)
            .map_err(|err| format!("Could not read file, path: {}, error: {}", self.path.display(), err))?;

        let parser = OwnedSExprParser::new(program_text);
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

/// A loader for a MeTTa module implemented as a directory
#[derive(Debug)]
pub(crate) struct DirModule {
    path: PathBuf,
}

impl DirModule {
    fn new(path: &Path) -> Self {
        Self {path: path.into() }
    }
}

impl ModuleLoader for DirModule {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {

        let space = DynSpace::new(GroundingSpace::new());
        let resource_dir = &self.path;
        context.init_self_module(space, Some(resource_dir.into()));

        let module_metta_path = self.path.join("module.metta");
        let program_text = std::fs::read_to_string(&module_metta_path)
            .map_err(|err| format!("Failed to read metta file in directory module, path: {}, error: {}", module_metta_path.display(), err))?;

        let parser = OwnedSExprParser::new(program_text);
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

/// Implemented on a type to test if a given file-system path points to a MeTTa module, and to construct
/// possible paths within a parent directory for a module of a certain name
///
/// Objects implementing this trait work with in conjunction with [DirCatalog] and [PkgInfo] to facilitate
/// loading modules from include directories, specific paths, and remote `git` repositories.
pub trait FsModuleFormat: std::fmt::Debug + Send + Sync {

    /// Returns the possible paths inside a parent directory which may point to a module
    ///
    /// NOTE: This function is allowed to return paths that may not be valid.  Paths returned
    /// from this method will be passed to [try_path] to validate them.
    fn paths_for_name(&self, parent_dir: &Path, mod_name: &str) -> Vec<PathBuf>;

    /// Checks a specific path, and returns a [ModuleLoader] and a [ModuleDescriptor] if a
    /// supported module resides at the path
    ///
    /// This method should return `None` if the path does not point to a valid module in the
    /// implemented format.
    fn try_path(&self, path: &Path, mod_name: Option<&str>) -> Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>;
}

/// Arbitrary number unlikely to be chosen by another FsModuleFormat
const SINGLE_FILE_MOD_FMT_ID: u64 = u64::MAX - 5000;

/// Arbitrary number unlikely to be chosen by another FsModuleFormat
const DIR_MOD_FMT_ID: u64 = u64::MAX - 5001;

/// An object to identify and load a single-file module (naked .metta files)
#[derive(Debug)]
pub struct SingleFileModuleFmt;

impl FsModuleFormat for SingleFileModuleFmt {
    fn paths_for_name(&self, parent_dir: &Path, mod_name: &str) -> Vec<PathBuf> {
        let base_path = parent_dir.join(mod_name);
        let extended_path = push_extension(&base_path, ".metta");
        vec![base_path, extended_path]
    }
    fn try_path(&self, path: &Path, mod_name: Option<&str>) -> Option<(Box<dyn ModuleLoader>, ModuleDescriptor)> {
        if path.is_file() {
            let mod_name = match mod_name {
                Some(mod_name) => mod_name,
                None => path.file_stem().unwrap().to_str().unwrap(),
            };

            //TODO: Add accessor for the module version here
            //In a single-file module, the discriptor information will be embedded within the MeTTa code
            // Therefore, we need to parse the whole text of the module looking for a `_pkg-info` atom,
            // that we can then convert into a PkgInfo structure

            let descriptor = ModuleDescriptor::new_with_path_and_fmt_id(mod_name.to_string(), path, SINGLE_FILE_MOD_FMT_ID);
            let loader = Box::new(SingleFileModule::new(path));
            Some((loader, descriptor))
        } else {
            None
        }
    }
}

/// An object to identify and load a MeTTa module implemented as a directory
#[derive(Debug)]
pub struct DirModuleFmt;

impl FsModuleFormat for DirModuleFmt {
    fn paths_for_name(&self, parent_dir: &Path, mod_name: &str) -> Vec<PathBuf> {
        let path = parent_dir.join(mod_name);
        vec![path]
    }
    fn try_path(&self, path: &Path, mod_name: Option<&str>) -> Option<(Box<dyn ModuleLoader>, ModuleDescriptor)> {
        if path.is_dir() {
            let mod_matta_path = path.join("module.metta");
            if mod_matta_path.exists() {
                let mod_name = match mod_name {
                    Some(mod_name) => mod_name,
                    None => path.file_stem().unwrap().to_str().unwrap(),
                };

                //LP-TODO-Next: Try and read the module version here
                //If there is a `pkg-info.metta` file, information from that file will take precedence.
                // Otherwise, try and parse a `_pkg-info` atom from the `module.metta` file

                let descriptor = ModuleDescriptor::new_with_path_and_fmt_id(mod_name.to_string(), path, DIR_MOD_FMT_ID);
                let loader = Box::new(DirModule::new(path));
                return Some((loader, descriptor));
            }
        }
        None
    }
}

/// Implements ModuleCatalog to load MeTTa modules from a file-system directory trying a number of
/// [FsModuleFormat] formats in succession
#[derive(Debug)]
pub struct DirCatalog {
    path: PathBuf,
    fmts: Arc<Vec<Box<dyn FsModuleFormat>>>,
}

impl DirCatalog {
    /// Internal function to initialize a DirCatalog for a directory in the module search path
    pub(crate) fn new(path: PathBuf, fmts: Arc<Vec<Box<dyn FsModuleFormat>>>) -> Self {
        Self {path, fmts}
    }
}

impl ModuleCatalog for DirCatalog {
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {

        //QUESTION: How should we handle modules with an internal "package-name" that differs from their
        // name in the file system?
        //
        //Cargo treats the module's internal "package-name" as authoritative, but that means it's impossible
        // to "install" a module simply by dropping it into a directory in the search path, because there
        // needs to be an index of all available modules in that directory.
        //
        //For us, I think we want a less formal approach akin to Python's, where we are allowed to drop a
        // module into a directory, and import it with a naked `import` statement (i.e. no pkg-info entry)
        // but for that to work, we need to stipulate that it's possible to infer a file name from a
        // module name.
        //
        //NOTE: This is not a limitation across all catalogs, just the `DirCatalog`  If a catalog is able
        // to maintain its own index of module names, it can store the modules any way it wants to.
        //

        let mut found_modules = vec![];

        //Inspect the directory using each FsModuleFormat, in order
        visit_modules_in_dir_using_mod_formats(&self.fmts, &self.path, name, |_loader, descriptor| {
            found_modules.push(descriptor);
            false
        });

        found_modules
    }
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {

        let mut matching_module = None;
        visit_modules_in_dir_using_mod_formats(&self.fmts, &self.path, &descriptor.name, |loader, resolved_descriptor| {
            if &resolved_descriptor == descriptor {
                matching_module = Some(loader);
                true
            } else {
                false
            }
        });

        match matching_module {
            Some(loader) => Ok(loader),
            None => Err(format!("Failed to load module {} in directory {}", &descriptor.name, self.path.display()))
        }
    }
}

/// Internal Utility Function.  Blindly appends an extension onto a path, even if the path already
/// has an extension
fn push_extension(path: &Path, extension: impl AsRef<OsStr>) -> PathBuf {
    let mut os_string: OsString = path.into();
    os_string.push(extension.as_ref());
    os_string.into()
}

/// Internal function to try FsModuleFormat formats in order.  If the closure returns `true` this function
/// will exit, otherwise it will try every path returned by every format
fn visit_modules_in_dir_using_mod_formats(fmts: &[Box<dyn FsModuleFormat>], dir_path: &Path, mod_name: &str, mut f: impl FnMut(Box<dyn ModuleLoader>, ModuleDescriptor) -> bool) {

    for fmt in fmts {
        for path in fmt.paths_for_name(dir_path, mod_name) {
            if let Some((loader, descriptor)) = fmt.try_path(&path, Some(mod_name)) {
                if f(loader, descriptor) {
                    return;
                }
            }
        }
    }
}

/// A data structure that uniquely identifies an exact version of a module with a particular provenance
///
/// If two modules have the same ModuleDescriptor, they are considered to be the same module
///
/// NOTE: It is possible for a module to have both a version and a uid.  Module version uniqueness is
/// enforced by the catalog(s), and two catalogs may disagree
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleDescriptor {
    name: String,
    uid: Option<u64>,
    //TODO: version
}

impl ModuleDescriptor {
    /// Create a new ModuleDescriptor
    pub fn new(name: String) -> Self {
        Self { name, uid: None }
    }
    /// Create a new ModuleDescriptor
    pub fn new_with_uid(name: String, uid: u64) -> Self {
        Self { name, uid: Some(uid) }
    }
    /// Create a new ModuleDescriptor using a file system path and another unique id
    ///
    /// The descriptor's uid is based on a stable-hash of the path, because a module loaded by
    /// path shouldn't be substituted for any other module unless it's from the same path.
    ///
    /// The purpose of the `fmt_id` is to ensure two different formats or catalogs don't generate
    /// the same ModuleDescriptor, but you can pass 0 if it doesn't matter
    pub fn new_with_path_and_fmt_id(name: String, path: &Path, fmt_id: u64) -> Self {
        let uid = xxh3_64(path.as_os_str().as_encoded_bytes()) ^ fmt_id;
        ModuleDescriptor::new_with_uid(name, uid)
    }
    /// Returns the name of the module represented by the ModuleDescriptor
    pub fn name(&self) -> &str {
        &self.name
    }
    /// Internal.  Use the Hash trait to get a uid for the whole ModuleDescriptor
    pub fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        std::hash::Hash::hash(self, &mut hasher);
        hasher.finish()
    }
}
