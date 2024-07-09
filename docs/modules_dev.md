
# MeTTa Modules (Rust / Python Developer Documentation)

TODO: Integrate this documentation within the larger MeTTa Book

Modules are implementations of free-standing MeTTa functionality that can be imported into other MeTTa modules or programs.  Modules may be implemented in MeTTa code itself, but they may also include functionality implemented with, or linked from host languages such as Rust, C, or Python.  Modules may include additional files and resources as well.

**_NOTE:_** Importantly, a module can have sub-module dependencies, aka "downward" dependencies, but it cannot have "upward" dependencies, ie. dependencies on the client code importing the module.

## What *is* a Module?

Fundamentally a module in a persistent encapsulation of a context within which MeTTa code can run.  Every module has a unique [Space] (and also a [Tokenizer], for now).  For MeTTa code running within the context of a module, the `&self` token will resolve to the module's space.

A loaded module is represented with the [MettaMod] struct.  In addition to a [Space] and a [Tokenizer], a module may also contain a filesystem path to the module's resources, the sub-modules imported by the module, a [ModuleDescriptor] object, and a [PkgInfo] struct, the latter of which are documented in the [Package Management](#package-management) section.

To execute code in the context of any loaded module, use the [RunnerState::new_with_module] method.

## Loading a Module

Modules are loaded into a [Metta] runner using one of the module loading methods: [Metta::load_module_direct], [Metta::load_module_at_path], or [RunContext::load_module].  Loaded modules are referred to with a [ModId].

Fundamentally, all modules are loaded via a loader object that implements the [ModuleLoader] trait.  Irrespective of the module's original format or host language, a loader object's [ModuleLoader::load] function ultimately loads the module into the runner.

### Module Names & Name Paths

Each loaded module must have a name.  A legal module name is an ascii string, containing only alpha-numeric characters plus `_` and `-`.

If module loading is initiated through the MeTTa `import!` operation or the corresponding [RunContext::load_module] API call, then the module name will be used to identify the module to load, following the logic in the [Module Name Resolution](#module-name-resolution) section.

Direct module-loading API calls such as [Metta::load_module_direct], [Metta::load_module_at_path], or [Metta::load_module_alias] all take an explicit module name or name path.

Upon loading, the module is subsequently placed into the module name path hierarchy, where `top` is always the name for the top-module in the runner and the `':'` character acts as the separator.  An example module name path looks like `top:mod1:sub_a`, and an example hierarchy is illustrated below.

```
top = 0
 ├─corelib = 1
 ├─stdlib = 2
 ├─mod1 = 3
 │  └─sub_a = 4
 └─mod2 = 5
    └─sub_b = 6
```

In addition, the `self` token may be used at the beginning of a module name path to refer to the currently running module context.  In the context of the top module, `top` and `self` should have an identical meaning.

Step-by-step, the MeTTa code:
```
!(import! &self some_module)
```
will cause the name `some_module` to be resolved into a specific module instance; if that module is not yet loaded then it will be, and finally the module will be imported, in totality, into the currently executing module (context).

![Import Op! Behavior](./assets/importop_behavior.svg "Import Op! Behavior")

**NOTE**: The same loaded module (with the same ModId) may appear multiple times in the hierarchy, sometimes with different names.  This could be the effect of an "import as" operation or making a module alias.

**NOTE**: The same module name may occur in multiple places in the hierarchy, and there is no guaranteed a name will always refer to the same module.  However, within a given node of the module name hierarchy, a module name will always be unique.

## Importing a Module

A module is imported into another module using one of the import methods:
 - [MettaMod::import_dependency_as], corresponding to `import module as name`
 - [MettaMod::import_all_from_dependency], corresponding to `import * from module`
 - [MettaMod::import_item_from_dependency_as], corresponding to `import item from module as name`

Once imported, a sub-module is accessed via an embedded [Space] atom in the destination module's space, [Tokenizer] entries for accessing the source module's space, tokens, or a combination of the two.

### Behavior WIP

TODO: The precise semantics of importing (in other words, linking) are still under discussion and development.  Specifically we may wish to provide a mechanism to explicitly declare what is exported from a module (and thus available for import).  This would be similar to the `export` key words in some languages, or the `pub` visibility qualifiers in Rust.

In addition, some changes will be needed so that [Tokenizer] entries can be imported and accessed between modules.  Currently Tokenizer entries are only imported using the [MettaMod::import_all_from_dependency] method, and the mechanism at work may lead to unreachable Tokenizer entries.

Some issues regarding this are:
[https://github.com/trueagi-io/hyperon-experimental/issues/509]
[https://github.com/trueagi-io/hyperon-experimental/issues/511]
[https://github.com/trueagi-io/hyperon-experimental/issues/510]

More discussion on these topics is in the section: "Importing / Linking" of the `modules_internal_discussion.md` file.

# Package Management

Package Management is the set of features that allow for:
- searching for modules across multiple locations (Catalogs)
- expressing version requirements and selecting compatible versions
- loading modules from files or other locations

Modularity is a fundamental and inseparable aspect of the MeTTa runner, but Package Management features could be optional.

## Module File Formats

Modules may be loaded from files and other file-system-like resources (for example, a github repo) using the objects that implement the [FsModuleFormat] trait.  This trait contains the interface to interpret a file or a directory and instantiate a [ModuleLoader] to load the module(s) contained within.

The objects [SingleFileModuleFmt] and [DirModuleFmt] are part of the default environment and are capable of loading MeTTa modules from single `.metta` files and directories containing a `module.metta` file respectively.  Additionally, the `hyperon` Python module contains a [FsModuleFormat] for loading MeTTa modules from Python modules - both stand-alone `.py` files as well as directories containing an `__init__.py` file.

More information on the individual module file formats is available in the MeTTa usage documentation and MeTTa Python documentation respectively.

## The PkgInfo Structure

Each module has an associated [PkgInfo] structure, which provides the module author a place to specify meta-data about the module and express requirements for the module's dependencies.  Additionally a [PkgInfo] can provide explicit loading instructions such as file system paths or github URLs for dependent modules.  The [PkgInfo] structure is the same concept as the Cargo.toml file used in Cargo/Rust.

The [PkgInfo] should be initialized inside the module's loader function.  If it is not initialized then default values will be used.

The fields of the [PkgInfo] struct are documented in the Rust MeTTa documentation [here](TODO link to 
docs on https://docs.rs when Rust crate is published).

TODO: PkgInfo documentation also belongs in user-facing docs.  In that section, cover how to specify the pkginfo as a MeTTa structure and/or in a `_pkg-info.metta` or `_pkg-info.json` file as opposed to as a Rust struct.

## Module Name Resolution

When MeTTa code executes the `!(import! &space some_module)` operation, the module name needs to be mapped to a loaded or loadable module.  This process occurs according to the logic described by the flowchart below.

![Name Resolution Behavior](./assets/module_name_resolution.svg "Name Resolution Behavior")

1. First the module name is checked against the modules which are already loaded within the context of the running module.  This ensures the same instance of a shared dependency will be loaded everywhere.

2. If a loaded module is not available, the [PkgInfo] will be checked for a corresponding entry.  If an entry specifies a specific location in the file system or a remote repository, then the module will be loaded from that location.  Additionally, the [PkgInfo] may specify version requirements for use by the catalog in locating and selecting an accaptable module.

3. Finally, the Catalogs from the Environment will be queried in priority order. (See the Catalogs section below)

By default, the built-in search paths / catalogs are:

1. The module's own `resource` directory, if it has one
2. The `hyperon/exts/` directory, if the Hyperon Python module is running
3. The MeTTa config `modules` directory, at an OS-specific location.

Depending on the host OS, the config directory locations will be:
* Linux: ~/.config/metta/
* Windows: ~\AppData\Roaming\TrueAGI\metta\config\
* Mac: ~/Library/Application Support/io.TrueAGI.metta/

In the future we may create a centralized module catalog along the lines of `PyPI` or `crates.io`.

## Catalogs

An object that implements the [ModuleCatalog] trait exposes an interface to locate modules based on name and version constraints, and create [ModuleLoader] objects to retrieve and load those modules.

One built-in [ModuleCatalog] object type is the [DirCatalog].  As described in the "Module File Formats" section, a [DirCatalog] uses a collection of [FsModuleFormat] objects to export a catalog of modules contained within its associated directory.

Additional catalogs may be implemented for other module repository formats or protocols - for example a central package service similar to `PyPI` or `crates.io`, as mentioned earlier.

## Implementing a [ModuleLoader]

All modules are ultimately loaded programmatically through the MeTTa API, and it's the role of a [ModuleLoader] to make the necessary API calls.

The [ModuleLoader::load] method ultimately sets up the module.  Each module has its own [Space] so the space needs to be created first.  Then the module must be initialized using the [RunContext::init_self_module] method.

After `init_self_module` has run, it is now legal to access the module data stricture using [RunContext::module] or [RunContext::module_mut], as well as enqueuing MeTTa code or additional operations to run.

An example `load` method implementation is here:
```rust
fn load(&self, context: &mut RunContext, descriptor: ModuleDescriptor) -> Result<(), String> {

    let space = DynSpace::new(GroundingSpace::new());
    let resource_dir = std::path::PathBuf::from("/tmp/test_module_resources")
    context.init_self_module(descriptor, space, Some(resource_dir.into()));

    let parser = OwnedSExprParser::new(METTA_PROGRAM_TEXT);
    context.push_parser(Box::new(parser));

    Ok(())
}
```
