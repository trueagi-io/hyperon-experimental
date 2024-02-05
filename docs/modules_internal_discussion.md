
# Module Design Discussion

## What does Module mean?

When I began this task, I conflated the design-space of modularity with package management, however the two are separate and distinct.  I began the task by designing and implementing a package manager, but the current MeTTa semantics lack the fundamental modularity constructs that are a prerequisite for robust package management.

It may seem like a tautology, but I feel it bears repeating that *the purpose of modules is modularity.*  In other words, isolating the implementation and incidental behaviors of some "module" code from other "client" code.

**Definition**: A module is unit of functionality that can be loaded into a MeTTa runner and used by other MeTTa code without requiring modification of the module, regardless of the location or the format from which the module was loaded.

### Desiderata

* A module should be able to be loaded and tested independently of client code that might load it
* A module should express its own sub-module dependencies
* A module should not rely on the client to import sub-dependencies or configure the environment for the module ("environment" used here in a loose sense, not the Environment object)
* A corollary to the above is that a module should not be affected by things the client does aside from the interactions through the module's interface.  For example, a client shouldn't be able to accidentally break a module's implementation by defining atoms or tokens in the client code. 

By analogy, Rust `mod`s meet these criteria, and C header files do not.  Shared libraries, on the other hand, are modules according to the criteria above.

### MettaMod struct

The [MettaMod] struct essentially takes the place of a "Loading Runner" in the prior code base.  The major difference being that the MettaMod continues to exist even after the module loader code has evaluated.  More discussion can be found in the "What *is* a Module?" section of the main module docs.

## Importing / Linking

To discuss import behavior, we need to discuss 3 distinct cases separately.  The cases are:

 - **Import As** `import module as name`
 - **Import All (aka import *)** `import * from module`
 - **Import Item** `import item from module as name`

### Import As

This is probably the most straightforward of the three import behaviors.  Currently the code registers a new Tokenizer entry in the destination module's Tokenizer with the space of sub-module.  This behavior is adequate and does the right thing for allowing the sub-module's space to be accessed within the context of the client module.

This is implemented by the [MettaMod::import_dependency_as] method.

**Outstanding Issue:** This does NOT provide a way to access tokenizer entries that are part of the sub-module.

### Import All

Conceptually this operation would overlay the space and tokens from the sub-module onto the client module.  In practice, given the behavior of the interpreter, the current import implementation is a bit convoluted.

Currently the implementation makes a deep-copy of the sub-module's space, then strips away space atoms that are associated with sub-modules of the dependency, before finally adding the cloned sub-space as a grounded atom into the client's space.

After that, the implementation imports the all of the new secondary transitive dependencies into the client, and finally it merges all tokens from the sub-module into the client.

**This current code has a number of issues:**

- Inefficient Space Clone: Deep-cloning the sub-modules's space on every import is costly, considering it should be possible to import functionality by reference.  This potentially leads to many copies of the same sub-modules in memory and defeats one of the benefits of modularity.

- Lifted Private Sub-Modules: All transitive dependency sub-modules are lifted into the client scope, including private sub-modules.  Addressing this issue may be as simple as not stripping and lifting private sub-modules, so that may be an easy fix.

- Redundant Tokenizer Entries LP-TODO-WRITEUP


LP-TODO-WRITEUP Discuss "Layered Tokenizer"

LP-TODO-WRITEUP Discuss ModuleSpace implementation that keeps sub-modules sequestered.

LP-TODO-WRITEUP Improve and integrate comments from .rs files below
    //QUESTION: How should we prevent duplication of transitive imports?  For example, consider: ModA
    // imports ModOne and ModTwo.  ModOne imports ModB.  ModTwo also imports ModB.
    // How do we make sure ModA doesn't end up with duplicate definitions for ModB?
    //
    //The solution implemented here is to elevate any transient dependencies up, and import them directly
    // into the upper module, but this has a lot of drawbacks described in the [stripped_space] method
    // comments, such as requiring a deep-copy of the dependency module's atoms.
    //
    //This is usually solved in other languages using some combination of strategies.
    // 1.) Keep loaded modules contained within their own name-spaces. And allow aliasing between a parent's
    //  space and a child module's space.  This is not really sufficient for us because we also want to
    //  import Tokenizer entries and their associated functionality, such as the ability to parse integers
    // 2.) Allow a conditional "import-unique" brute-force import which is tantamount to inlining the dependency
    //  at the point of the import statement.  e.g. `#include` in C., but with the "ifdef" guards to make sure
    //  the same header isn't inlined multiple times.  We may want to offer this functionality, but I assume
    //  we'll want to offer this in addition to a more hygenic module system structure, given that most languages
    //  (Python, JavaScript, even Perl!) that started off with brute-force imports have retrofitted more
    //  sophisticated dependency management as they have matured
    // 3.) Force a module to be explicit about what can be exported from the module.  e.g. the `pub` visibility
    //  qualifiers in Rust, etc.
    //
    //Personally, I feel like 3. (being explicit about exported items) is the cleanest solution and provides
    //  the most flexibility into the future (such as the ability to unload modules, etc.)



This is implemented by the [MettaMod::import_all_from_dependency] method.


/// Effectively adds all atom in a dependency module to the &self module, by adding the dependency
/// module's space as an atom inside the &self module
///
/// WARNING: Module import behavior is still WIP, specifically around "import *" behavior, and this
/// function may result in duplicated transitive imports.
//
//QUESTION: What do we do about tokenizer entries?  Currently they end up glomming together but this
// is highly undesireable for several reasons.  At best it leads to tokenizer entries that are
// unreachable because they're superseded by other entries, and at worst this can introduce some
// difficult to diagnose behavior
//
//QUESTION: How do we prevent duplication of transitive imports?  For example, consider: ModA imports
// ModOne and ModTwo.  ModOne imports ModB.  ModTwo also imports ModB.
// How do we make sure ModA doesn't end up with duplicate definitions for ModB?
//
//This is usually solved in other languages using some combination of strategies.
// 1.) Keep loaded modules contained within their own name-spaces. And allow aliasing between a parent's
//  space and a child module's space.  This is not really sufficient for us because we also want to
//  import Tokenizer entries and their associated functionality, such as the ability to parse integers
// 2.) Allow a conditional "import-unique" brute-force import which is tantamount to inlining the dependency
//  at the point of the import statement.  e.g. `#include` in C., but with the "ifdef" guards to make sure
//  the same header isn't inlined multiple times.  We may want to offer this functionality, but I assume
//  we'll want to offer this in addition to a more hygenic module system structure, given that most languages
//  (Python, JavaScript, even Perl!) that started off with brute-force imports have retrofitted more
//  sophisticated dependency management as they have matured
// 3.) Force a module to be explicit about what can be exported from the module.  e.g. the `pub` visibility
//  qualifiers in Rust, etc.
//
//Personally, I feel like 3. (being explicit about exported items) is the cleanest solution and provides
//  the most flexibility into the future (such as the ability to unload modules, etc.)
//


## Sub-Module Version Resolution Discussion

### Background

There are two desiderata which are at odds with each other and must be balanced:

- **Implementation sub-modules**: We want sub-dependencies not to interfere with each other as much as is possible.  For example, ModA should be able to import ModC, and ModB should be able to import a *different* version of ModC.  This should be allowed as long as both ModA and ModB use ModC within their internal implementation.  Within this document I will call this pattern an "Implementation" sub-module

- **Interface sub-modules** If ModA imports ModC and uses functionality from ModC in its interface, then ClientMod will transitively import ModC when it imports ModA.  In effect this means the dependent sub-module's version becomes part of the module's interface.  Therefore, within this document, I will call this an "Interface" sub-module.  If ClientMod imports ModA and ModB, which both import ModC as an Interface sub-module, then the version (instance) of ModC must be the same between ModA and ModB.

### Sub-Module Set Satisfiability

The versions of transitive Interface sub-modules must be reconciled such that a given client imports exactly one version of each dependent sub-module, even when that sub-module is shared between two or more other dependents.  Finding the compatible module versions is a flavor of the Satisfiability Problem.

Reasonably sophisticated package managers such as Cargo include a Satisfiability Solver, which will attempt to find a set of sub-module versions that satisfy the requirements for each client that imports each dependency.  I believe we will ultimately want to add a solver to MeTTa, however currently this doesn't exist.  MeTTa as a language is well-suited to implementing a solver.

Until we add a solver to the MeTTa Package Manager, the module resolution logic will progress sequentially with no ability to backtrack.  This means that we will need to rely on each module's author to manually determine a workable set of sub-module versions for themselves.  Then the specific (or narrower) version requirements can be added to the module's [PkgInfo].

**Current State of the Code:** Version requirements for sub-modules are not yet implemented so this is currently a non-issue.

### Module Namespace Scope

When a module is loaded, the module's name may be registered in the runner's module namespace, so that subsequent attempts to resolve the same module name elsewhere in the runner will return the already-loaded module as opposed to causing a new instance of the module to be loaded.

The question here is which behavior we want for the module namespace:

- Proposal A: Global Scope:  This means every module is effectively an Interface Module, and only one version of a given module can be loaded in the same runner.  This is the approach taken by both Cargo and Python (but for different reasons in each case), and a number of other package managers.  However, the reasons that have driven other package managers towards this design don't necessarily apply to MeTTa, and I believe we can support private Implementation sub-modules.  Therefore I do not advocate for this design.

- Proposal B: Conservative Hierarchical Scope:  Within this design, each module name only propagates upwards to its client if it needs to.  This means that Interface modules must be explicitly exported or marked some other way.  It also complicates the Sat Solver (see above) as it would need to operate over multiple sets with some partial intersections between them.  I believe this is probably the "right" design, but also the most work to implement.

- Proposal C: 2-Layer "Public vs. Private" Scope:  This is the way the code currently works.  Each imported module may be "Public", in which case it is available by name to all other modules in the runner, or it may be "Private", in which case it is loaded only for the client module that resolves it.  This is a "poor man's" version of Proposal B.  However, it has many problematic edge cases so I would prefer to implement Proposal B.

For either Proposal B or C, we probably want to add some form of linting / checking, so users don't accidentally shoot themselves in the foot by exporting items from private (Implementation) sub-modules.

