
Hyperon Bindings for C                         {#mainpage}
============

This documentation specifically covers the API for extending Hyperon with modules implemented in C as well as integrating Hyperon into a C project.  For a more complete overview of Hyperon, you should look here:  **TODO: Where?**

#### API Conventions

The HyperonC API is intended to be a light weight layer over the native Rust implementation, and is designed to impose as little runtime performance overhead as possible.  It is not necessary to be familiar with Rust to use HyperonC, but it might clarify the paterns because some of the API semantics come directly from ownership and borrowing in Rust.

#### Object Lifecycle Pattern

When a Hyperon object is created through the creation function, it is given to you, the caller, by value.  For example `atom_sym()` returns an `atom_t` struct.  You own that `atom_t` and you must free it when it's no longer needed, using the corresponding free function; `atom_free()` in the case of `atom_t`.

```C
    // Create the atom
    atom_t test_atom = atom_sym("test_atom");

    // We need to free the atom because we own it
    atom_free(test_atom);
```

However, if you pass the object by value to another function, that function takes ownership and now you are no longer responsible for freeing the object.  For example, `atom_vec_push()` takes an `atom_t` argument, passed by value.  This means the passed atom is now owned by the vec, and it is the responsibility of the vec to free the atom.

```C
    // Create an empty vec (list container) for atoms
    atom_vec_t test_vec = atom_vec_new();

    // Create a test atom and push it into the vec
    // Now the vec takes responsibility for the atom (ownership)
    atom_t test_atom = atom_sym("test_atom");
    atom_vec_push(&test_vec, test_atom);

    // We still own the vec so we must free it
    atom_vec_free(test_vec);
```

Some functions return pointers or smart pointers (structs with types ending in "ref_t") pointing to other objects.  When working with a pointer or ref_t smart pointer, you must be cognizant of the life cycle of the referenced object.  For example, `atom_vec_get()` returns an `atom_ref_t` to refer to an atom within the vec.  That reference will become invalid if the `atom_vec_t` the pointer refers into is freed or changed in any way.  The Rust borrow checker ensures these constraints are never violated, but in C you must take that responsibility yourself.

```C
    // Create a vec with an atom
    atom_vec_t test_vec = atom_vec_new();
    atom_vec_push(&test_vec, atom_sym("test_atom"));

    // Borrow the atom back from the vec as a reference
    atom_ref_t borrowed_atom = atom_vec_get(&test_vec, 0);

    // Print the atom's text description
    char buffer[64];
    atom_to_str(&borrowed_atom, buffer, 64);
    printf("atom = %s", buffer);

    // We're done with the vec for some reason, so free it
    atom_vec_free(test_vec);

    // NO!!!! The atom that `borrowed_atom` points to was freed with the vec
    //atom_t new_atom = atom_clone(&borrowed_atom);
```

[//]: # (Atom Interface)

@defgroup atom_group Atom Interface
@brief Functions for working directly with atoms

These functions are used to create, inspect, modify, and free atoms.

[//]: # (Atom Vec Interface)

@defgroup atom_vec_group Atom Vec Interface
@brief Functions for working with vectors (lists) of atoms

These functions are used to create, access, modify, and free atom vectors.

[//]: # (Matching and Binding Interface)

@defgroup matching_group Matching and Binding Interface
@brief Interface for working with atom matching and variable-to-atom binding

[//]: # (Grounded Atom Interface)

@defgroup grounded_atom_group Grounded Atom Interface
@brief Interface to define custom atoms implemented with C

Providing the functions in `gnd_api_t` allows custom atom behaviors to be implemented using C.

[//]: # (Space Client Interface)

@defgroup space_client_group Space Client Interface
@brief Functions for accessing a space directly

These functions are used to manage spaces, including to create new spaces, free spaces, manipulate the atoms in a space as well as to query a space directly.

[//]: # (Space Observer Interface)

@defgroup space_observer_group Space Observer Interface
@brief Functions for implementing a Space Observer to monitor changes to a Space from C

Space Observers permit code to monitor activity within a Space by receiving events.  Additionally, custom Space implementations must notify all observers by broadcasting events to communicate activity within the space.

[//]: # (Space Implementation Interface)

@defgroup space_impl_group Space Implementation Interface
@brief Functions for implementing a custom space in C

This interface allows a custom space to be defined with C.  This allows for different space back-ends, such as a distributed database, or different space topologies, such as the multidimensional latent space of [word2vec](https://en.wikipedia.org/wiki/Word2vec).

[//]: # (Tokenizer and Parser Interface)

@defgroup tokenizer_and_parser_group Tokenizer and Parser Interface
@brief API to parse the MeTTa language from text into Atoms

This interface facilitates parsing textual representations of MeTTa into atom representations, and can be extended to parse custom atom types with specialized syntax.

[//]: # (MeTTa Language and Types)

@defgroup metta_language_group MeTTa Language and Types
@brief Components needed to interact with the MaTTa language and type system

This interface includes Symbol atoms that have special meaning to the MeTTa interpreter, and it provides access to the MeTTa type system

More complete documentation on the MeTTa language, type system, and the MeTTa standard library can be found here: **TODO: Where?**

[//]: # (MeTTa Interpreter Interface)

@defgroup interpreter_group MeTTa Interpreter Interface
@brief Top-level API to execute MeTTa code

This Interface includes the types and functions to instantiate a MeTTa interpreter and step through MeTTa code.

[//]: # (Misc. Interfaces)

@defgroup misc_group Misc Interfaces
@brief Misc. Interfaces for integration and housekeeping

This module is the catch-all for ancillary interfaces.
