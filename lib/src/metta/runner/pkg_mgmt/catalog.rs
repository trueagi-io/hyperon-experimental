//!
//! # Module Resolution
//!
//! ## Behavior of Module Resolution
//!
//! ```text
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

use core::any::Any;
use std::path::Path;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::ffi::{OsStr, OsString};
use std::collections::HashSet;

use crate::metta::runner::modules::*;
use crate::metta::runner::{*, git_catalog::*};

use xxhash_rust::xxh3::xxh3_64;
use serde::{Deserialize, Serialize};

/// Implemented for types capable of locating MeTTa modules
///
/// For example, `ModuleCatalog` would be an interface to a module respository, analogous to `PyPI` or
/// `crates.io` but `ModuleCatalog` is also implemented for [Path] because any file system directory may be
/// capable of storing and indexing MeTTa modules.
///
/// `ModuleCatalog` types are closely connected with [ModuleLoader] types because the `ModuleCatalog` must
/// recognize the module in whatever media it exists, and supply the `ModuleLoader` to load that module
pub trait ModuleCatalog: std::fmt::Debug + Send + Sync {
    /// The name of the catalog, to be displayed to the user
    fn display_name(&self) -> String {
        std::any::type_name::<Self>().to_string()
    }

    /// Returns the [ModuleDescriptor] for every module in the `ModuleCatalog` with the specified name
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor>;

    /// Returns the [ModuleDescriptor] for every module in the `ModuleCatalog` with the specified name,
    ///   and uid match
    fn lookup_with_uid(&self, name: &str, uid: Option<u64>) -> Vec<ModuleDescriptor> {
        self.lookup(name).into_iter().filter(|desc| desc.uid == uid).collect()
    }

    /// Returns the [ModuleDescriptor] for every module in the `ModuleCatalog` with the specified name
    /// matching the version requirements
    ///
    /// NOTE: Unversioned modules will never match any version_req, so this method should never return
    /// any un-versioned ModuleDescriptors if `version_req.is_some()`
    fn lookup_with_version_req(&self, name: &str, version_req: Option<&semver::VersionReq>) -> Vec<ModuleDescriptor> {
        filter_by_version_req(self.lookup(name).into_iter(), version_req).collect()
    }

    /// Returns the [ModuleDescriptor] for the newest module in the `ModuleCatalog`, that matches the
    /// specified version requirement, or `None` if no module exists
    ///
    /// If `version_req == None`, this method should return the newest module available in the catalog
    ///
    /// NOTE: unversioned modules are considered to have the lowest possible version, and thus this method
    ///   should only return an unversioned module if no matching modules are available
    /// NOTE: Unversioned modules will never match any version_req, so this method should never return
    /// any un-versioned ModuleDescriptors if `version_req.is_some()`
    fn lookup_newest_with_version_req(&self, name: &str, version_req: Option<&semver::VersionReq>) -> Option<ModuleDescriptor> {
        find_newest_module(self.lookup_with_version_req(name, version_req).into_iter())
    }

    /// Returns the [ModuleDescriptor] for the newest module in the `ModuleCatalog`, that matches the
    /// specified name, uid, and version requirement, or `None` if no module exists
    ///
    /// See [ModuleCatalog::lookup_newest_with_version_req] for more details
    fn lookup_newest_with_uid_and_version_req(&self, name: &str, uid: Option<u64>, version_req: Option<&semver::VersionReq>) -> Option<ModuleDescriptor> {
        let result_iter = self.lookup_with_uid(name, uid).into_iter();
        find_newest_module(filter_by_version_req(result_iter, version_req))
    }

    /// Returns a [ModuleLoader] for the specified module from the `ModuleCatalog`
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String>;

    /// Returns an iterator over every module available in the catalog.  May not be supported
    /// by all catalog implementations
    fn list<'a>(&'a self) -> Option<Box<dyn Iterator<Item=ModuleDescriptor> + 'a>> {
        None
    }

    /// Returns an iterator over every unique module name in the catalog.  May not be supported
    /// by all catalog implementations
    fn list_names<'a>(&'a self) -> Option<Box<dyn Iterator<Item=String> + 'a>> {
        self.list().map(|desc_iter| {
            let mut names = HashSet::new();
            for desc in desc_iter {
                if !names.contains(desc.name()) {
                    names.insert(desc.name().to_string());
                }
            }
            Box::new(names.into_iter()) as Box<dyn Iterator<Item=String>>
        })
    }

    /// Returns an iterator over every unique (module name, uid) pair in the catalog.  May not
    /// be supported by all catalog implementations
    fn list_name_uid_pairs<'a>(&'a self) -> Option<Box<dyn Iterator<Item=(String, Option<u64>)> + 'a>> {
        self.list().map(|desc_iter| {
            let mut results = HashSet::new();
            for desc in desc_iter {
                results.insert((desc.name().to_string(), desc.uid()));
            }
            Box::new(results.into_iter()) as Box<dyn Iterator<Item=(String, Option<u64>)>>
        })
    }

    /// Returns the catalog as an [Any] in order to get back to the underlying object
    fn as_any(&self) -> Option<&dyn Any> {
        None
    }

    /// Synchronize the catalog's internal tables, so fresh upstream info is reflected
    /// locally.  Does not fetch any modules
    fn sync_toc(&self, _update_mode: UpdateMode) -> Result<(), String> {
        Ok(())
    }

    /// Returns the catalog as a [ManagedCatalog] if the catalog supports active management
    fn as_managed(&self) -> Option<&dyn ManagedCatalog> {
        None
    }
}

impl dyn ModuleCatalog {
    /// Returns the catalog as as an underlying type, if it's supported by the catalog format
    pub fn downcast<T: 'static>(&self) -> Option<&T> {
        self.as_any()?.downcast_ref()
    }
}

/// Internal function to filter a set of [ModuleDescriptor]s by a [semver::VersionReq].  See
/// [ModuleCatalog::lookup_with_version_req] for an explanation of behavior
fn filter_by_version_req<'a>(mods_iter: impl Iterator<Item=ModuleDescriptor> + 'a, version_req: Option<&'a semver::VersionReq>) -> Box<dyn Iterator<Item=ModuleDescriptor> + 'a> {
    match version_req {
        Some(req) => Box::new(mods_iter.filter(|desc| {
            match desc.version() {
                Some(ver) => req.matches(ver),
                None => false
            }
        })),
        None => Box::new(mods_iter)
    }
}

/// Internal function to find the newest module in a set.  See [ModuleCatalog::lookup_newest_with_version_req]
/// for an explanation of behavior
pub(crate) fn find_newest_module(mods_iter: impl Iterator<Item=ModuleDescriptor>) -> Option<ModuleDescriptor> {
    let mut highest_version: Option<semver::Version> = None;
    let mut ret_desc = None;
    for desc in mods_iter {
        match desc.version().cloned() {
            Some(ver) => {
                match &mut highest_version {
                    Some(highest_ver) => {
                        if ver > *highest_ver {
                            *highest_ver = ver;
                            ret_desc = Some(desc);
                        }
                    },
                    None => {
                        ret_desc = Some(desc);
                        highest_version = Some(ver)
                    }
                }
            },
            None => {
                if highest_version.is_none() {
                    if let Some(ret_desc) = ret_desc {
                        log::warn!("Multiple un-versioned {} modules in catalog; impossible to select newest", ret_desc.name());
                    }
                    ret_desc = Some(desc)
                }
            }
        }
    }
    ret_desc
}

/// The object responsible for locating and selecting dependency modules for each [MettaMod]
///
/// This structure is conceptually analogous to the a `Cargo.toml` file for a given module.
#[derive(Clone, Debug, Default, Deserialize)]
pub struct PkgInfo {

    /// The public name of the module
    ///
    /// Should be composed of alpha-numeric characters with '-' and '_' characters allowed.  Must not
    /// contain any other punctuation
    pub name: Option<String>,

    /// The version of this module
    ///
    /// A `None` or missing version is considered inferior to all other versions
    #[serde(default)]
    pub version: Option<semver::Version>,

    /// If `strict == true` then a dependency must be declared in the `PkgInfo`, otherwise a permissive
    /// version requirement will be assumed for any modules that are not explicitly declared
    #[serde(default)]
    pub strict: bool,

    /// Requirements for each dependency sub-module
    ///
    /// A Duplicate entry for a given sub-module in the deps list is an error.
    #[serde(default)]
    pub deps: HashMap<String, DepEntry>,
}

/// A single entry in a [PkgInfo]'s dependencies, specifying the properties of a module that will satisfy a dependency
#[derive(Clone, Debug, Default, Deserialize)]
pub struct DepEntry {
    /// Indicates that the dependency module should be loaded from a specific FS path
    ///
    /// If the fs_path is specified, the other pkg_info attributes will be ignored.
    #[serde(default)]
    pub fs_path: Option<PathBuf>,

    #[serde(flatten)]
    git_location: ModuleGitLocation,

    /// An acceptable version of version bounds to satisfy the dependency.  None means any version
    /// acceptable
    #[serde(default)]
    pub version_req: Option<semver::VersionReq>
}

impl PkgInfo {
    /// Returns the version of the package
    pub fn version(&self) -> Option<&semver::Version> {
        self.version.as_ref()
    }
    /// Returns the version of the package as a [semver compliant](https://semver.org) string of bytes
    pub fn version_bytes(&self) -> Result<Vec<u8>, String> {
        match self.version() {
            Some(ver) => Ok(format!("{ver}").into_bytes()),
            None => Err("no version available".to_string())
        }
    }
}

/// Resolves which module to load from which available location or catalog, and returns the [ModuleLoader] to
/// load that module
pub(crate) fn resolve_module(pkg_info: Option<&PkgInfo>, context: &RunContext, name_path: &str) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {
    let mod_name = mod_name_from_path(name_path);

    //Make sure the name is a legal module name
    if !module_name_is_legal(mod_name) {
        return Err(format!("Illegal module name: {mod_name}"));
    }

    //See if we have a pkg_info dep entry for the module
    let mut version_req = None;
    if let Some(entry) = pkg_info.as_ref().and_then(|pkg_info| pkg_info.deps.get(mod_name)) {

        //If path is explicitly specified in the dep entry, then we must load the module at the
        // specified path, and cannot search anywhere else
        if let Some(path) = &entry.fs_path {
            return loader_for_module_at_path(context.metta.environment().fs_mod_formats(), path, Some(mod_name), context.module().resource_dir());
        }

        //Get the module if it's specified with git keys
        if entry.git_location.get_url().is_some() {
            match context.metta.environment().specified_mods.as_ref() {
                Some(specified_mods) => if let Some(pair) = specified_mods.loader_for_explicit_git_module(mod_name, UpdateMode::FetchIfMissing, &entry.git_location)? {
                    return Ok(Some(pair));
                },
                None => return Err(format!("Unable to pull module \"{mod_name}\" from git; no local \"caches\" directory available"))
            }
        }

        //If `version_req` is specified in the dep entry, then use it to constrain the catalog search
        version_req = entry.version_req.as_ref();
    } else {
        //If the PkgInfo doesn't have an entry for the module and the PkgInfo is flagged as "strict"
        // then we will not attempt to resolve the module any further, and the resolution will fail.
        if let Some(pkg_info) = &pkg_info {
            if pkg_info.strict {
                return Ok(None);
            }
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
        match catalog.lookup_newest_with_version_req(mod_name, version_req) {
            Some(descriptor) => {
                log::info!("Found module: \"{mod_name}\" inside {:?}", catalog.display_name());
                log::info!("Preparing to load module: \'{}\' as \'{}\'", descriptor.name, name_path);
                return Ok(Some((catalog.get_loader(&descriptor)?, descriptor)))
            },
            None => {}
        }
    }

    Ok(None)
}

/// Internal function to get a loader for a module at a specific file system path, by trying each FsModuleFormat in order
pub(crate) fn loader_for_module_at_path<'a, P: AsRef<Path>, FmtIter: Iterator<Item=&'a dyn FsModuleFormat>>(fmts: FmtIter, path: P, name: Option<&str>, search_dir: Option<&Path>) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {

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
    for fmt in fmts {
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
    pkg_info: PkgInfo,
}

impl SingleFileModule {
    fn new(path: &Path, pkg_info: PkgInfo) -> Self {
        Self {path: path.into(), pkg_info }
    }
    fn open_file(&self) -> Result<std::fs::File, String> {
        std::fs::File::open(&self.path)
            .map_err(|err| format!("Could not read file, path: {}, error: {}", self.path.display(), err))
    }
}

impl ModuleLoader for SingleFileModule {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {

        let space = DynSpace::new(GroundingSpace::new());
        let resource_dir = self.path.parent().unwrap();
        context.init_self_module(space, Some(resource_dir.into()));

        let parser: SExprParser<_> = self.open_file()?.into();
        context.push_parser(Box::new(parser));

        Ok(())
    }
    fn get_resource(&self, res_key: ResourceKey) -> Result<Resource, String> {
        match res_key {
            ResourceKey::MainMettaSrc => self.open_file().map(Into::<Resource>::into),
            ResourceKey::Version => self.pkg_info.version_bytes().map(Into::<Resource>::into),
            _ => Err("unsupported resource key".to_string())
        }
    }
}

/// A loader for a MeTTa module implemented as a directory
///
/// A `DirModule` can contain MeTTa code in a `module.metta` file, but any directory may
/// be explicitly loaded as a module, making the directory contents available as resources.
///
/// See the "Anatomy of a Directory Module" section in the LP-TODO Finish writeup of user-level guide
#[derive(Debug)]
pub(crate) struct DirModule {
    path: PathBuf,
    pkg_info: PkgInfo,
}

impl DirModule {
    fn new(path: &Path, pkg_info: PkgInfo) -> Self {
        Self { path: path.into(), pkg_info }
    }
    fn module_metta_path(&self) -> PathBuf {
        self.path.join("module.metta")
    }
    fn open_file(&self) -> Result<std::fs::File, String> {
        std::fs::File::open(&self.module_metta_path())
            .map_err(|err| format!("Could not read file, path: {}, error: {}", self.path.display(), err))
    }
}

impl ModuleLoader for DirModule {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {

        let space = DynSpace::new(GroundingSpace::new());
        let resource_dir = &self.path;
        context.init_self_module(space, Some(resource_dir.into()));

        // A module.metta file is optional.  Without one a dir module behaves as just
        // a container for other resources and sub-modules.
        if let Some(program_file) = self.open_file().ok() {
            let parser: SExprParser<_> = program_file.into();
            context.push_parser(Box::new(parser));
        }

        Ok(())
    }
    fn get_resource(&self, res_key: ResourceKey) -> Result<Resource, String> {
        match res_key {
            ResourceKey::MainMettaSrc => self.open_file()
                .map_err(|_| format!("no module.metta file found in {} dir module", self.path.display()))
                .map(Into::<Resource>::into),
            ResourceKey::Version => self.pkg_info.version_bytes()
                .map(Into::<Resource>::into),
            _ => Err("unsupported resource key".to_string())
        }
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
    /// from this method will be passed to [Self::try_path] to validate them.
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
                None => path.file_stem().unwrap().to_str().unwrap(), //LP-TODO-NEXT: Unify the code to extract the mod-name from the file name between here and DirModuleFmt::try_path
            };

            //TODO: parse out the module version here, and pass it to new_with_path_and_fmt_id below
            //In a single-file module, the discriptor information will be embedded within the MeTTa code
            // Therefore, we need to parse the whole text of the module looking for a `_pkg-info` atom,
            // that we can then convert into a PkgInfo structure
            let pkg_info = PkgInfo::default();

            let descriptor = ModuleDescriptor::new_with_path_and_fmt_id(mod_name.to_string(), None, path, SINGLE_FILE_MOD_FMT_ID);
            let loader = Box::new(SingleFileModule::new(path, pkg_info));
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

            //First see if we can extract a [PkgInfo] from a `pkg-info.json` file
            let mut pkg_info: Option<PkgInfo> = None;
            let pkginfo_json_path = path.join("pkg-info.json");
            if pkginfo_json_path.exists() {
                let file_contents = std::fs::read_to_string(&pkginfo_json_path).unwrap();
                pkg_info = Some(serde_json::from_str(&file_contents).unwrap());
            }

            //TODO: Also check for a `pkg-info.metta` file, as soon as I have implemented Atom-Serde
            // Also try and parse a `_pkg-info` atom from the `module.metta` file if it's not in a dedicated file

            let pkg_info = pkg_info.unwrap_or_else(|| PkgInfo::default());

            //Get the module name, first use the name provided.  If none, then use the name from the
            // pkg-info, and if that's also none, construct a module name from the file name
            let full_path;
            let mod_name = match mod_name {
                Some(mod_name) => mod_name,
                None => {
                    match &pkg_info.name {
                        Some(name) => name,
                        None => {
                            //LP-TODO-Next: I need to gracefully create a legal module name from the file name
                            // if the file name happens to contain characters that are illegal in a module name
                            full_path = path.canonicalize().unwrap();
                            full_path.file_stem().unwrap().to_str().unwrap()
                        }
                    }
                },
            };

            let version = pkg_info.version.clone();
            let descriptor = ModuleDescriptor::new_with_path_and_fmt_id(mod_name.to_string(), version, path, DIR_MOD_FMT_ID);
            let loader = Box::new(DirModule::new(path, pkg_info));
            return Some((loader, descriptor));
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
    fn display_name(&self) -> String {
        format!("Dir \"{}\"", self.path.display())
    }
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
            true
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

/// A data structure that uniquely identifies an exact instance of a module
///
/// If two modules have the same ModuleDescriptor, they are considered to be the same module
///
/// The uid field encodes particulars about a module so it will never be mistaken for another copy
/// or variation of the module even if the version field is the same.  For example, a module loaded
/// from the file system will use the uid to hash the path, while a module fetched from git will
/// hash the url and branch.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct ModuleDescriptor {
    name: String,
    uid: Option<u64>,
    version: Option<semver::Version>,
}

impl core::fmt::Display for ModuleDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(version) = &self.version {
            write!(f, " @{version}")?;
        }
        if let Some(uid) = self.uid {
            write!(f, " #{uid:016x}")?;
        }
        Ok(())
    }
}

impl ModuleDescriptor {
    /// Create a new ModuleDescriptor
    pub fn new(name: String, version: Option<semver::Version>, uid: Option<u64>) -> Self {
        Self { name, uid, version }
    }
    /// Returns a new ModuleDescriptor by computing a stable hash of the `ident` bytes, and using the `fmt_id`
    pub fn new_with_ident_bytes_and_fmt_id(name: String, version: Option<semver::Version>, ident: &[u8], fmt_id: u64) -> Self {
        let uid = Self::uid_from_ident_bytes_and_fmt_id(ident, fmt_id);
        ModuleDescriptor::new(name, version, Some(uid))
    }
    /// Create a new ModuleDescriptor using a file system path and another unique id
    ///
    /// The descriptor's uid is based on a stable-hash of the path, because a module loaded by
    /// path shouldn't be substituted for any other module unless it's from the same path.
    ///
    /// The purpose of the `fmt_id` is to ensure two different formats or catalogs don't generate
    /// the same ModuleDescriptor, but you can pass 0 if it doesn't matter
    pub fn new_with_path_and_fmt_id(name: String, version: Option<semver::Version>, path: &Path, fmt_id: u64) -> Self {
        Self::new_with_ident_bytes_and_fmt_id(name, version, path.as_os_str().as_encoded_bytes(), fmt_id)
    }
    /// Returns the name of the module represented by the ModuleDescriptor
    pub fn name(&self) -> &str {
        &self.name
    }
    /// Returns the uid associated with the ModuleDescriptor
    pub fn uid(&self) -> Option<u64> {
        self.uid
    }
    /// Returns the version of the module represented by the ModuleDescriptor
    pub fn version(&self) -> Option<&semver::Version> {
        self.version.as_ref()
    }
    /// Internal.  Use the Hash trait to get a uid for the whole ModuleDescriptor
    pub fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        std::hash::Hash::hash(self, &mut hasher);
        hasher.finish()
    }
    /// Returns a uid based on a stable hash of `the ident` bytes, and the fmt_id
    pub fn uid_from_ident_bytes_and_fmt_id(ident: &[u8], fmt_id: u64) -> u64 {
        xxh3_64(ident) ^ fmt_id
    }
}

/// Extracts the module name from a `.git` URL
///
/// For example, `https://github.com/trueagi-io/hyperon-experimental.git` would be parsed
/// into "hyperon-experimental".  Returns None if the form of the URL isn't recognized
pub fn mod_name_from_url(url: &str) -> Option<String> {
    let without_ending = url.trim_end_matches("/")
        .trim_end_matches(".git");
    let without_mod_name = without_ending.trim_end_matches(|c| c != '/');
    let mod_name = &without_ending[without_mod_name.len()..];
    module_name_make_legal(mod_name)
}

//-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-
// TESTS
//-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-

#[cfg(test)]
mod tests {
    use super::*;

    /// Bogus test catalog that returns a fake module in response to any query with a single capital letter
    /// used by `recursive_submodule_import_test`
    #[derive(Debug)]
    struct TestCatalog;

    impl ModuleCatalog for TestCatalog {
        fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {
            if name.len() == 1 && name.chars().last().unwrap().is_uppercase() {
                vec![ModuleDescriptor::new(name.to_string(), None, None)]
            } else {
                vec![]
            }
        }
        fn get_loader(&self, _descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {
            Ok(Box::new(TestCatalog))
        }
    }

    impl ModuleLoader for TestCatalog {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            let space = DynSpace::new(GroundingSpace::new());
            context.init_self_module(space, None);
            Ok(())
        }
    }

    /// This tests the core recursive sub-module loading code
    #[test]
    fn recursive_submodule_import_test() {

        //Make a new runner with the TestCatalog
        let runner = Metta::new(Some(EnvBuilder::test_env().push_module_catalog(TestCatalog)));

        //Now try loading an inner-module, and make sure it can recursively load all the needed parents
        let result = runner.run(SExprParser::new("!(import! &self A:B:C)"));
        assert_eq!(result, Ok(vec![vec![expr!()]]));

        //Test that each parent sub-module is indeed loaded
        assert!(runner.get_module_by_name("A").is_ok());
        assert!(runner.get_module_by_name("A:B").is_ok());
        assert!(runner.get_module_by_name("A:B:C").is_ok());

        //Test that we fail to load a module with an invalid parent, even if the module itself resolves
        let _result = runner.run(SExprParser::new("!(import! &self a:B)"));
        assert!(runner.get_module_by_name("a:B").is_err());
    }

    //
    //LP-TODO-NEXT, Next make sure the catalogs are able to do the recursive loading from the file system,
    // using their working dirs.  Maybe make this second test a C API test to get better coverage
    //

    //LP-TODO-NEXT, Add a test for loading a module from a DirCatalog by passing a name with an extension (ie. `my_mod.metta`) to `resolve`,
    // and make sure the loaded module that comes back doesn't have the extension

    #[derive(Debug)]
    struct TestLoader {
        pkg_info: PkgInfo,
    }

    impl TestLoader {
        fn new() -> Self {
            let mut pkg_info = PkgInfo::default();

            //Set up the module [PkgInfo] so it knows to load a sub-module from git
            pkg_info.name = Some("test-mod".to_string());
            pkg_info.deps.insert("metta-morph-test".to_string(), DepEntry{
                fs_path: None,
                git_location: ModuleGitLocation {
                    //TODO: We probably want a smaller test repo
                    git_url: Some("https://github.com/trueagi-io/metta-morph/".to_string()),
                    git_branch: None, //Some("Hyperpose".to_string()),
                    git_subdir: None,
                    git_main_file: Some(PathBuf::from("mettamorph.metta")),
                },
                version_req: None,
            });
            Self { pkg_info }
        }
    }

    impl ModuleLoader for TestLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            let space = DynSpace::new(GroundingSpace::new());
            context.init_self_module(space, None);

            Ok(())
        }
        fn pkg_info(&self) -> Option<&PkgInfo> {
            Some(&self.pkg_info)
        }
    }

    /// Tests that a module can be fetched from git and loaded, when the git URL is specified in
    /// the module's PkgInfo.  This test requires a network connection
    ///
    /// NOTE.  Ignored because we may not want it fetching from the internet when running the
    /// test suite.  Invoke `cargo test --features git git_pkginfo_fetch_test -- --ignored --nocapture` to run it.
    #[ignore]
    #[test]
    fn git_pkginfo_fetch_test() {

        //Make a new runner, with the config dir in `/tmp/hyperon-test/`
        let runner = Metta::new(Some(EnvBuilder::new().set_config_dir(Path::new("/tmp/hyperon-test/"))));
        let _mod_id = runner.load_module_direct(Box::new(TestLoader::new()), "test-mod").unwrap();

        let result = runner.run(SExprParser::new("!(import! &self test-mod:metta-morph-test)"));
        assert_eq!(result, Ok(vec![vec![expr!()]]));

        //Test that we can use a function imported from the module
        let result = runner.run(SExprParser::new("!(sequential (A B))"));
        assert_eq!(result, Ok(vec![vec![sym!("A"), sym!("B")]]));

        runner.display_loaded_modules();
    }

    /// Tests that a module can be resolved in a remote cataloc, fetched from git and then
    /// loaded.  This test requires a network connection
    ///
    /// NOTE.  Ignored because we may not want it fetching from the internet when running the
    /// test suite.  Invoke `cargo test --features git git_remote_catalog_fetch_test -- --ignored --nocapture` to run it.
    #[ignore]
    #[test]
    fn git_remote_catalog_fetch_test() {

        //Make a new runner, with the config dir in `/tmp/hyperon-test/`
        let runner = Metta::new(Some(EnvBuilder::new().set_config_dir(Path::new("/tmp/hyperon-test/"))));
        let result = runner.run(SExprParser::new("!(import! &self metta-morph)"));
        assert_eq!(result, Ok(vec![vec![expr!()]]));

        //Test that we can use a function imported from the module
        let result = runner.run(SExprParser::new("!(sequential (A B))"));
        assert_eq!(result, Ok(vec![vec![sym!("A"), sym!("B")]]));

        runner.display_loaded_modules();
    }
}
