
# Module Design Discussion - Open Questions

## Overview: What does Module mean?

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

## Question Topic 1: Importing & Linking

To discuss import behavior, we need to discuss 3 distinct cases separately.  The cases are:

 - **Import As** `import module as name`
 - **Import All (aka import \*)** `import * from module`
 - **Import Item** `import item from module as name`

In parallel, a discussion on this topic exists here: https://github.com/trueagi-io/hyperon-experimental/issues/509 although this document presently represents a more thorough treatment of the topic.

### Import As

This is probably the most straightforward of the three import behaviors.  Currently the code registers a new Tokenizer entry in the destination module's Tokenizer with the space of sub-module.  This behavior is adequate and does the right thing for allowing the sub-module's space to be accessed within the context of the client module.

This is implemented by the [MettaMod::import_dependency_as] method.

**Outstanding Issue:** This implementation doesn't provide any way to access tokenizer entries that are part of the sub-module.

### Import All

Conceptually this operation imports the entire contents of a sub-module into the target module.  Another way to think about this is overlaying the space and tokens of the sub-module onto the client module.  In practice, given the behavior of the interpreter, the current implementation is a bit convoluted.

Currently the implementation leverages the behavior that a nested Space Grounded Atom will behave as an extension of the Space containing it.

The import code makes a deep-copy of the sub-module's space, then strips away space atoms that are associated with dependency's own sub-modules (2nd order sub-modules), before finally adding the cloned sub-space as a grounded atom into the client's space.

After that, the implementation imports the all of the new secondary transitive dependencies into the client, and finally it merges all tokenizer entries from the sub-module into the client.

This "import all" behavior is implemented by the [MettaMod::import_all_from_dependency] method.

#### This current implementation some issues

- Inefficient Space Clone: Deep-cloning the sub-modules's space on every import is costly, considering it should be possible to import functionality by reference.  This potentially leads to many copies of the same sub-modules in memory and defeats one of the benefits of modularity.  One potential way to fix this it to create a [Space] implementation that I'll call `ModuleSpace`, which wraps the module's `&self` space regardless of its underlying type.  `ModuleSpace` holds references to the sub-modules' individual spaces and controls which how the sub-module spaces will be combined.  I have not yet tried to implement this so I don't know what other problems may come up.

- Private Sub-Modules shouldn't be lifted: All transitive dependency sub-modules are lifted into the client scope, including private sub-modules.  Assuming we have a fix for the above, addressing this point may be as simple as not stripping and lifting private sub-modules, so that may be an easy fix.

- Redundant Tokenizer Entries: Currently Tokenizer entries imported from sub-modules may be inaccessible because they are superseded by existing entries in the module's Tokenizer.  A partial solution here might be to implement something like a "Layered Tokenizer" as described here: https://github.com/trueagi-io/hyperon-experimental/issues/408#issuecomment-1839196513

#### Explicit exports or visibility qualifiers

Most languages that support modularity allow a module to declare a subset of the objects as available for export.  Sometimes this is done with visibility qualifiers, (for example `pub` in Rust), or in other situations this is accomplished via the ABI and header files, as with shared libraries imported in C & C++.

Explicit control over exports would solve the problem of duplicated or conflicting transitive imports of sub-modules, because private sub-modules would not be exported.

### Import Item

The `import item from module [as name]` is implemented with the [MettaMod::import_item_from_dependency_as] method.  However there is no `stdlib` operation that calls it, so it's currently inaccessible from MeTTa code.

#### Tokenizer entries don't always have names to import

When we want to import a tokenizer entry from a sub-module, we need a name to refer to it.  But currently Tokenizer entries don't have names.  By itself, this problem is easy to address, but we should consider our choice of solution in light of the other choices we need to make for the MeTTa language.

This issue is discussed in more detail here: https://github.com/trueagi-io/hyperon-experimental/issues/510

One solution, as described on github, is to require Tokenizer entries to have corresponding items in the module's Space.

## Question Topic 2: Sub-Module Version Resolution Discussion

### Background

There are two desiderata which are at odds with each other and must be balanced:

- **Implementation sub-modules**: We want sub-module dependencies not to interfere with each other as much as is possible.  For example, ModA should be able to import ModC, and ModB should be able to import a *different* version of ModC.  This should be allowed as long as both ModA and ModB use ModC within their internal implementation.  Within this document I will call this pattern an "Implementation" sub-module.

- **Interface sub-modules** If ModA imports ModC and uses functionality from ModC in its interface, then ClientMod will transitively import ModC when it imports ModA.  In effect this means the dependent sub-module's version becomes part of the module's interface.  Therefore, within this document, I will call this an "Interface" sub-module.  If ClientMod imports ModA and ModB, which both import ModC as an Interface sub-module, then the version (instance) of ModC must be the same between ModA and ModB.

### Sub-Module set satisfiability

The versions of transitive Interface sub-modules must be reconciled such that a given client imports exactly one version of each dependent sub-module, even when that sub-module is shared between two or more other dependents.  Finding the compatible module versions is a flavor of the Satisfiability Problem.

Reasonably sophisticated package managers such as Cargo include a Satisfiability Solver, which will attempt to find a set of sub-module versions that satisfy the requirements for each client that imports each dependency.  I believe we will ultimately want to add a solver to MeTTa too, someday.  However this doesn't currently exist.  MeTTa as a language is well-suited to implementing a solver.

Until we add a solver to the MeTTa Package Manager, the module resolution logic will progress sequentially with no ability to backtrack.  This means that we will need to rely on each module's author to manually determine a workable set of sub-module versions for themselves.  Then the specific (or narrower) version requirements can be added to the module's [PkgInfo].

**Current State of the Code:** Version requirements for sub-modules are not yet implemented so this is a non-issue in the present implementation.

### Module Namespace Scope

When a module is loaded, the module's name may be registered in the runner's module namespace, so that subsequent attempts to resolve the same module name elsewhere in the runner will return the already-loaded module as opposed to causing a new instance of the module to be loaded.

The question here is which behavior we want for the module namespace:

- Proposal A: Global Scope:  This means every module is effectively an Interface Module, and only one version of a given module can be loaded in the same runner.  This is the approach taken by both Cargo and Python (but for different reasons in each case), and a number of other package managers.  However, the reasons that have driven other package managers towards this design don't necessarily apply to MeTTa, and I believe we can support private Implementation sub-modules.  Therefore I do not advocate for this design.

- Proposal B: Conservative Hierarchical Scope:  Within this design, each module name only propagates upwards to its client if it needs to, or is explicitly re-exported.  This means that Interface modules must be exported or marked some other way.  It also complicates the Sat Solver (see above) as it would need to operate over multiple sets with some partial intersections between them.  I believe this is probably the "right" design, but also the most work to implement.

- Proposal C: 2-Layer "Public vs. Private" Scope:  This is the way the code currently works.  Each imported module may be "Public", in which case it is available by name to all other modules in the runner, or it may be "Private", in which case it is loaded only for the client module that resolves it.  This is a "poor man's" version of Proposal B.  However, it has many problematic edge cases so I would prefer to implement Proposal B.

For either Proposal B or C, we probably want to add some form of linting / checking, so users don't accidentally shoot themselves in the foot by exporting items that depend on private (Implementation) sub-modules.  In other words, public interfaces must not include non-public objects.

#### Is importing by hierarchical module path allowed?

In other words, should we support the following MeTTa code?  `!(import! mod_a.mod_b.mod_c &self)`?

Assuming each of `mod_a`, `mod_b`, and `mod_c` can be resolved within their appropriate parent context, and each is a valid module that exports the needed sub-module, should we support this?

My feeling is yes, assuming we settle on a hierarchical module namespace (Proposal B in the *Module Namespace Scope* section) and we resolve issues with sub-module visibility / exports, then I don't see any drawbacks to supporting this.

## Remaining implementation work



LP-TODO, search code for TODO comments and collect them here so I can prioritize remaining work

### Catalog for All Python modules

Need to implement a ModuleCatalog that publishes all Python modules as MeTTa modules.  This will achieve parity with the current implementation and re-enable the practice of using `pip` to install MeTTa modules.  Additionally, we may want to support hierarchical module path imports, so that the MeTTa import operation is able to traverse the python module hierarchy to load and import nested Python modules.

