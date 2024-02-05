
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

### Module Names

Each module has a name which is provided by [ModuleLoader::name] method when the module is loaded.  This name serves a secondary role to [ModId] when importing a module via the Import APIs, but MeTTa users will use this name both to load and to import a module.

For example, the MeTTa code:
```
!(import! some_module &self)
```
will cause the name `some_module` to be resolved into a specific module instance; if that module is not yet loaded then it will be, and finally the module will be imported, in totality, into the currently executing module (context).

A legal module name must be an ascii string, containing only alpha-numeric characters plus `_` and `-`.

A module name is not guaranteed to be globally unique, but it is unique within the scope of a client context.  In other words, names will never conflict between two sub-modules importable from within the same parent module.

TODO: The precise behavior of the module namespace is still under discussion.  Read more in the section: "Sub-Module Version Resolution Discussion" of the `modules_internal_discussion.md` file.

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

QUESTION: PkgInfo probably belongs in user-facing docs

LP-TODO-WRITEUP

- _pkg-info: Expresses mapping between module name and a verion requirement, as well as providing specific load instructions, such as a bespoke file system paths, git repos or remote URIs
    Duplicate entries for a given module in _pkg-info are an error.
    Duplicate _pkg-info entries in one module is also an error.


## Module Name Resolution
LP-TODO-WRITEUP.  Flowchart from Ascii Art


If an imported module is in _pkg-info, that entry takes precedence.  Otherwise a universal version will be acceptable and default search behavior will be used



# Implementing a [ModuleLoader]

LP-TODO-WRITEUP