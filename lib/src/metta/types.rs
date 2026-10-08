//! Contains Rust functions working on types. All MeTTa
//! specific constants are exported as a part of [metta] module.
//!
//! To designate the atom `a` has a type `A` one need add to the space the
//! expression `(: a A)`. One can also assign a type to type, for example to
//! designate `A` is a type add `(: A Type)`. `->` is used to create a function
//! type, for example `(: foo (-> A B))`. Types can also be parameterized by
//! type or by value. For example list of numbers can be represented as
//! `(: ns (List Number))`.
//!
//! There are five special meta-types: `Atom`, `Symbol`, `Variable`, `Grounded`
//! and `Expression`. These types should not be assigned explicitly, but they
//! can be used in type expressions and will be checked. For example one can
//! define a function which accepts `Atom` as an argument: `(: bar (-> Atom A))`.
//! When such expression is interpreted the argument is accepted without
//! reduction (see [metta::interpreter] algorithm).
//!
//! When atom has no type assigned by user it has type `%Undefined%`. The value
//! of `%Undefined%` type can be matched with any type required.

use super::*;
use hyperon_atom::matcher::{Bindings, BindingsSet, apply_bindings_to_atom_move, atoms_are_equivalent};
use hyperon_space::DynSpace;

use std::fmt::{Display, Debug};
use itertools::Itertools;
use hyperon_atom::gnd::number::Number;
use std::cell::Cell;

/// Default bound on the depth of subtype derivations: 0, which means no bound. The search for
/// supertypes is bounded only when the user sets a bound, like the interpreter's stack depth.
pub const DEFAULT_TYPE_SEARCH_DEPTH: usize = 0;
thread_local! {
    static TYPE_SEARCH_DEPTH: Cell<usize> = const { Cell::new(DEFAULT_TYPE_SEARCH_DEPTH) };
}

pub(crate) struct TypeSearchScope(usize);
impl Drop for TypeSearchScope {
    fn drop(&mut self) { TYPE_SEARCH_DEPTH.with(|limit| limit.set(self.0)); }
}

pub(crate) fn type_search_depth() -> usize { TYPE_SEARCH_DEPTH.with(Cell::get) }

pub(crate) fn scoped_type_search_depth(depth: usize) -> TypeSearchScope {
    TypeSearchScope(TYPE_SEARCH_DEPTH.with(|limit| limit.replace(depth)))
}

fn type_search_limit_error(atom: Atom, depth: usize) -> Atom {
    Atom::expr([ERROR_SYMBOL, atom, Atom::expr([sym!("TypeSearchLimit"), Atom::gnd(Number::Integer(depth.min(i64::MAX as usize) as i64))])])
}

pub(crate) fn is_type_search_limit_error(error: &Atom) -> bool {
    match error {
        Atom::Expression(expr) => match expr.children() {
            [op, _, Atom::Expression(reason)] if *op == ERROR_SYMBOL =>
                reason.children().first() == Some(&sym!("TypeSearchLimit")),
            _ => false,
        },
        _ => false,
    }
}

fn typeof_query(atom: &Atom, typ: &Atom) -> Atom {
    Atom::expr(vec![HAS_TYPE_SYMBOL, atom.clone(), typ.clone()])
}

fn isa_query(sub_type: &Atom, super_type: &Atom) -> Atom {
    Atom::expr(vec![SUB_TYPE_SYMBOL, sub_type.clone(), super_type.clone()])
}

fn query_has_type(space: &DynSpace, sub_type: &Atom, super_type: &Atom) -> BindingsSet {
    space.borrow().query(&typeof_query(sub_type, super_type))
}

fn query_super_types(space: &DynSpace, sub_type: &Atom) -> Vec<Atom> {
    // TODO: query should check that sub type is a type and not another typed symbol
    let var_x = VariableAtom::new("X").make_unique();
    let super_types = space.borrow().query(&isa_query(&sub_type, &Atom::Variable(var_x.clone())));
    let atom_x = Atom::Variable(var_x);
    super_types.into_iter().map(|bindings| { apply_bindings_to_atom_move(atom_x.clone(), &bindings) }).collect()
}

/// Lazily enumerates types followed by their supertypes.
///
/// The enumeration proceeds in rounds. A round adds each supertype of the types found by the
/// previous round, unless the supertype was known before the round started. Two equal
/// supertypes found by the same round are both kept. A round is computed only when all the
/// types found before are consumed, so a check can stop as soon as it finds a matching type,
/// while a complete enumeration returns the same types in the same order as an eager search.
///
/// A supertype which repeats one of its own ancestors up to renaming of variables is not
/// added. Such a supertype comes from a declaration which introduces a fresh variable on each
/// use, for example `(:< A $x)`: adding it would only produce the same ancestors again under
/// new variable names, without end.
struct TypeClosure<'a> {
    space: &'a DynSpace,
    types: Vec<Atom>,
    /// For each type the index of the type it was derived from
    parents: Vec<Option<usize>>,
    yielded: usize,
    /// Index of the first type found by the last round
    round_start: usize,
    /// Number of the rounds which added types
    depth: usize,
    /// Maximum number of such rounds, 0 means no limit
    limit: usize,
    finished: bool,
}

impl<'a> TypeClosure<'a> {
    fn new(space: &'a DynSpace, types: Vec<Atom>) -> Self {
        let parents = vec![None; types.len()];
        Self { space, types, parents, yielded: 0, round_start: 0, depth: 0,
            limit: type_search_depth(), finished: false }
    }

    fn repeats_ancestor(&self, index: usize, typ: &Atom) -> bool {
        let mut ancestor = Some(index);
        while let Some(index) = ancestor {
            if atoms_are_equivalent(&self.types[index], typ) {
                return true;
            }
            ancestor = self.parents[index];
        }
        false
    }
}

impl Iterator for TypeClosure<'_> {
    type Item = Result<Atom, usize>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished { return None; }
        if self.yielded == self.types.len() {
            let mut found = Vec::new();
            for index in self.round_start..self.types.len() {
                for super_type in query_super_types(self.space, &self.types[index]) {
                    if !self.types.contains(&super_type) && !self.repeats_ancestor(index, &super_type) {
                        found.push((super_type, index));
                    }
                }
            }
            if found.is_empty() {
                self.finished = true;
                return None;
            }
            if self.limit > 0 && self.depth == self.limit {
                self.finished = true;
                return Some(Err(self.limit));
            }
            self.round_start = self.types.len();
            for (super_type, parent) in found {
                self.types.push(super_type);
                self.parents.push(Some(parent));
            }
            self.depth += 1;
        }
        let typ = self.types[self.yielded].clone();
        self.yielded += 1;
        Some(Ok(typ))
    }
}

fn check_arg_types(actual: &[Vec<AtomType>], meta: &[Vec<Atom>], types: &mut Vec<AtomType>, expected: &[Atom], ret_typ: &Atom, atom: &Atom, fn_type_atom: &Atom) -> () {
    if actual.len() != expected.len() {
        types.push(AtomType::error(fn_type_atom.clone(), Atom::expr([ERROR_SYMBOL, atom.clone(), INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL])));
    } else {
        check_arg_types_internal(actual, meta, expected, Bindings::new(), types, fn_type_atom, atom, ret_typ, actual.len());
    }
}

fn check_arg_types_internal(actual: &[Vec<AtomType>], meta: &[Vec<Atom>], expected: &[Atom], bindings: Bindings, types: &mut Vec<AtomType>, fn_type_atom: &Atom, atom: &Atom, ret_typ: &Atom, start_len: usize) -> () {
    log::trace!("check_arg_types: actual: {}, expected: {}",
        actual.iter().format_with(", ", |v, f| f(&format_args!("{}", v.iter().format(", ")))),
        expected.iter().format(", "));

    match (actual, meta, expected) {
        ([actual, actual_tail @ ..], [meta, meta_tail @ ..], [expected, expected_tail @ ..]) => {
            let undefined_or_meta = actual.is_empty()
                || *expected == ATOM_TYPE_UNDEFINED
                || meta.contains(expected);
            let matches: &mut dyn Iterator<Item=Bindings> = if undefined_or_meta {
                &mut std::iter::once(bindings)
            } else {
                &mut actual.into_iter()
                    .flat_map(|typ| match_reducted_types(typ.as_atom(), expected))
                    .flat_map(|b| b.merge(&bindings))
            };
            let mut matches = matches.peekable();
            if matches.peek().is_none() {
                let idx = (start_len - actual_tail.len()) as i64;
                for typ in actual {
                    let error = Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::expr([BAD_ARG_TYPE_SYMBOL, Atom::gnd(Number::Integer(idx)), expected.clone(), typ.as_atom().clone()])]);
                    types.push(AtomType::error(fn_type_atom.clone(), error));
                }
            } else {
                for b in matches {
                    check_arg_types_internal(actual_tail, meta_tail, expected_tail, b, types, fn_type_atom, atom, ret_typ, start_len);
                }
            }
        },
        ([], [], []) => {
            let typ = AtomType::application(apply_bindings_to_atom_move(ret_typ.clone(), &bindings));
            types.push(typ);
        },
        _ => unreachable!(),
    };
    log::trace!("check_arg_types: actual: {}, expected: {}",
        actual.iter().format_with(", ", |v, f| f(&format_args!("{}", v.iter().format(", ")))),
        expected.iter().format(", "));
}

/// Returns true if passed type is a type of function.
///
/// # Examples
///
/// ```
/// use hyperon_atom::expr;
/// use hyperon::metta::types::is_func;
///
/// assert!(is_func(&expr!("->" "A" "B")));
/// assert!(!is_func(&expr!("A")));
/// assert!(!is_func(&expr!(("->"))));
/// ```
#[inline]
pub fn is_func(typ: &Atom) -> bool {
    match typ {
        Atom::Expression(expr) => {
            (expr.children().first() == Some(&ARROW_SYMBOL)) && (expr.children().len() > 1)
        },
        _ => false,
    }
}

fn query_types_direct(space: &DynSpace, atom: &Atom) -> Vec<Atom> {
    let var_x = VariableAtom::new("X").make_unique();
    let types = query_has_type(space, atom, &Atom::Variable(var_x.clone()));
    let atom_x = Atom::Variable(var_x);
    types.into_iter().filter_map(|bindings| {
        let atom = apply_bindings_to_atom_move(atom_x.clone(), &bindings);
        if atom_x == atom || atom == ATOM_TYPE_UNDEFINED {
            None
        } else {
            Some(atom)
        }
    }).collect()
}

fn query_types(space: &DynSpace, atom: &Atom) -> Result<Vec<Atom>, usize> {
    TypeClosure::new(space, query_types_direct(space, atom)).collect()
}

/// Splits function type on array of argument types and return type.
fn get_arg_types<'a>(fn_typ: &'a AtomType) -> (&'a [Atom], &'a Atom) {
    match fn_typ.as_atom() {
        Atom::Expression(expr) => {
            let children = expr.children();
            match children {
                [op,  args @ .., res] if *op == ARROW_SYMBOL => (args, res),
                _ => panic!("Incorrect function type: {}", fn_typ)
            }
        },
        _ => panic!("Incorrect function type: {}", fn_typ)
    }
}

fn get_args(expr: &ExpressionAtom) -> &[Atom] {
    &expr.children()[1..]
}

#[derive(Debug, PartialEq, Clone)]
pub struct AtomType {
    typ: Atom,
    is_function: bool,
    info: TypeInfo,
}

#[derive(Debug, PartialEq, Clone)]
enum TypeInfo {
    Application,
    ApplicationError {
        error: Atom,
    },
    Value,
}

impl AtomType {

    #[inline]
    pub fn undefined() -> Self {
        Self {
            typ: ATOM_TYPE_UNDEFINED,
            is_function: false,
            info: TypeInfo::Value,
        }
    }

    #[inline]
    pub fn value(typ: Atom) -> Self {
        let is_function = is_func(&typ);
        Self {
            typ,
            is_function,
            info: TypeInfo::Value,
        }
    }

    #[inline]
    pub fn application(typ: Atom) -> Self {
        let is_function = is_func(&typ);
        Self {
            typ,
            is_function,
            info: TypeInfo::Application,
        }
    }

    #[inline]
    pub fn error(typ: Atom, error: Atom) -> Self {
        let is_function = is_func(&typ);
        Self {
            typ,
            is_function,
            info: TypeInfo::ApplicationError {
                error,
            }
        }
    }

    #[inline]
    pub fn is_error(&self) -> bool {
        matches!(self.info, TypeInfo::ApplicationError{..})
    }

    /// Whether this result reports an unfinished, depth-bounded type search.
    pub fn is_search_limit(&self) -> bool {
        self.get_error().map_or(false, is_type_search_limit_error)
    }

    fn search_limit(atom: &Atom, depth: usize) -> Self {
        Self::error(ATOM_TYPE_UNDEFINED, type_search_limit_error(atom.clone(), depth))
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        !self.is_error()
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        self.is_function
    }
    #[inline]
    pub fn is_application(&self) -> bool {
        matches!(self.info, TypeInfo::Application)
    }

    #[inline]
    pub fn as_atom(&self) -> &Atom {
        &self.typ
    }

    #[inline]
    pub fn into_atom(self) -> Atom {
        self.typ
    }

    #[inline]
    pub fn get_error(&self) -> Option<&Atom> {
        match &self.info {
            TypeInfo::ApplicationError { error } => Some(error),
            _ => None,
        }
    }

    #[inline]
    pub fn into_error(self) -> Option<Atom> {
        match self.info {
            TypeInfo::ApplicationError { error } => Some(error),
            _ => None,
        }
    }

    #[inline]
    pub fn into_error_unchecked(self) -> Atom {
        match self.info {
            TypeInfo::ApplicationError { error } => error,
            _ => panic!("Unexpected state"),
        }
    }

    #[inline]
    pub fn into_atom_or_error(self) -> Result<Atom, Atom>  {
        match self.info {
            TypeInfo::ApplicationError { error } => Err(error),
            _ => Ok(self.typ),
        }
    }
}

impl Display for AtomType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}(", self.typ)
            .and_then(|r| if self.is_application() { write!(f, "A") } else { Ok(r) })
            .and_then(|r| if self.is_error() { write!(f, "E") } else { Ok(r) })
            .and_then(|_| write!(f, ")"))
    }
}

/// Returns vector of the types for the given `atom` in context of the given
/// `space`. Returns [`%Undefined%`] if atom has no type assigned.
/// # Examples
///
/// ```
/// use hyperon_common::assert_eq_no_order;
/// use hyperon_atom::Atom;
/// use hyperon_macros::metta;
/// use hyperon_space::DynSpace;
/// use hyperon::space::grounding::GroundingSpace;
/// use hyperon::metta::types::{AtomType, get_atom_types};
///
/// let mut space = GroundingSpace::new();
/// space.add(metta!((: f (-> A B))));
/// space.add(metta!((: a A)));
/// space.add(metta!((: a B)));
/// space.add(metta!((: b B)));
///
/// let space = DynSpace::from(space);
/// assert_eq_no_order!(get_atom_types(&space, &metta!($x)), vec![AtomType::undefined()]);
/// assert_eq_no_order!(get_atom_types(&space, &metta!(1)), vec![AtomType::value(metta!(Number))]);
/// assert_eq_no_order!(get_atom_types(&space, &metta!(na)), vec![AtomType::undefined()]);
/// assert_eq_no_order!(get_atom_types(&space, &metta!(a)), vec![AtomType::value(metta!(A)), AtomType::value(metta!(B))]);
/// assert_eq_no_order!(get_atom_types(&space, &metta!((a b))), vec![AtomType::value(metta!((A B))), AtomType::value(metta!((B B)))]);
/// assert_eq_no_order!(get_atom_types(&space, &metta!((f a))), vec![AtomType::application(metta!(B))]);
/// assert_eq_no_order!(get_atom_types(&space, &metta!((f b))), vec![AtomType::error(metta!((-> A B)), metta!((Error (f b) (BadArgType 1 A B))))]);
/// ```
pub fn get_atom_types(space: &DynSpace, atom: &Atom) -> Vec<AtomType> {
    let atom_types = get_atom_types_internal(space, atom);
    if atom_types.is_empty() {
        vec![AtomType::undefined()]
    } else {
        atom_types
    }
}

/// Query complete type information with an explicit subtype-derivation bound.
/// An unfinished closure yields only a `TypeSearchLimit` error, never a partial type list.
pub fn get_atom_types_with_limit(space: &DynSpace, atom: &Atom, max_depth: usize) -> Vec<AtomType> {
    let _scope = scoped_type_search_depth(max_depth);
    get_atom_types(space, atom)
}

/// Lazy type search for existential checks. Consumers must propagate the final
/// error if they have not established a match.
pub(crate) fn iter_atom_types<'a>(space: &'a DynSpace, atom: &Atom) -> Box<dyn Iterator<Item = AtomType> + 'a> {
    iter_atom_types_internal(space, atom, true)
}

fn iter_atom_types_internal<'a>(space: &'a DynSpace, atom: &Atom, undefined_if_empty: bool) -> Box<dyn Iterator<Item = AtomType> + 'a> {
    match atom {
        Atom::Symbol(_) => {
            let seeds = query_types_direct(space, atom);
            if seeds.is_empty() {
                return if undefined_if_empty {
                    Box::new(std::iter::once(AtomType::undefined()))
                } else {
                    Box::new(std::iter::empty())
                };
            }
            let atom = atom.clone();
            Box::new(TypeClosure::new(space, seeds).map(move |typ| match typ {
                Ok(typ) => AtomType::value(typ),
                Err(depth) => AtomType::search_limit(&atom, depth),
            }))
        },
        Atom::Expression(expr) if expr.children().is_empty() =>
            if undefined_if_empty {
                Box::new(std::iter::once(AtomType::undefined()))
            } else {
                Box::new(std::iter::empty())
            },
        Atom::Expression(expr) => Box::new(ExpressionTypes::new(space, atom, expr, undefined_if_empty)),
        _ if undefined_if_empty => Box::new(get_atom_types(space, atom).into_iter()),
        _ => Box::new(get_atom_types_internal(space, atom).into_iter()),
    }
}

struct ExprTypeInfo {
    op_func_types: Vec<AtomType>,
    arg_types: Vec<Vec<AtomType>>,
}

fn get_atom_types_internal(space: &DynSpace, atom: &Atom) -> Vec<AtomType> {
    log::trace!("get_atom_types_internal: atom: {}", atom);
    let types = match atom {
        // TODO: type of the variable could be actually a type variable,
        // in this case inside each variant of type for the atom we should
        // also keep bindings for the type variables. For example,
        // we have an expression `(let $n (foo) (+ $n $n))`, where
        // `(: let (-> $t $t $r $r))`, `(: foo (-> $tt))`,
        // and `(: + (-> Num Num Num))`then type checker can find that
        // `{ $r = $t = $tt = Num }`.
        Atom::Variable(_) => vec![],
        Atom::Grounded(gnd) => {
            let typ = gnd.type_();
            if typ == ATOM_TYPE_UNDEFINED {
                vec![]
            } else {
                vec![AtomType::value(make_variables_unique(gnd.type_()))]
            }
        },
        Atom::Symbol(_) => match query_types(space, atom) {
            Ok(types) => types.into_iter().map(AtomType::value).collect(),
            Err(depth) => vec![AtomType::search_limit(atom, depth)],
        },
        // TODO: empty expression should have unit type (->), but type checking
        // code cannot handle functional type which doesn't return value
        Atom::Expression(expr) if expr.children().len() == 0 => vec![],
        Atom::Expression(expr) => ExpressionTypes::new(space, atom, expr, false).collect(),
    };
    log::debug!("get_atom_types_internal: return atom {} types {}", atom, types.iter().format(", "));
    if types.iter().any(AtomType::is_search_limit) {
        types.into_iter().filter(AtomType::is_search_limit).collect()
    } else { types }
}

/// Builds expression type information while enumerating tuple types in the
/// original Cartesian-product order. Function types are cached for the later
/// application phase, but do not prevent nearby tuple types from being yielded.
struct ExprTypeInfoBuilder<'a> {
    op_types: Box<dyn Iterator<Item = AtomType> + 'a>,
    arg_type_iters: Vec<Box<dyn Iterator<Item = AtomType> + 'a>>,
    op_value_types: Vec<AtomType>,
    op_func_types: Vec<AtomType>,
    arg_types: Vec<Vec<AtomType>>,
    index: Vec<usize>,
    exhausted: Vec<bool>,
    started: bool,
    finished: bool,
}

impl<'a> ExprTypeInfoBuilder<'a> {
    fn new(space: &'a DynSpace, expr: &ExpressionAtom) -> Self {
        let (op, args) = expr.children().split_first().unwrap();
        let arg_type_iters: Vec<_> = args.iter()
            .map(|arg| iter_atom_types_internal(space, arg, false)).collect();
        let arity = args.len() + 1;
        Self {
            op_types: iter_atom_types_internal(space, op, false),
            arg_type_iters,
            op_value_types: vec![],
            op_func_types: vec![],
            arg_types: (0..args.len()).map(|_| vec![]).collect(),
            index: vec![0; arity],
            exhausted: vec![false; arity],
            started: false,
            finished: false,
        }
    }

    fn arity(&self) -> usize { self.index.len() }

    fn next_value(&mut self, source: usize) -> Option<Result<AtomType, AtomType>> {
        loop {
            let typ = if source == 0 {
                self.op_types.next()
            } else {
                self.arg_type_iters[source - 1].next()
            };
            match typ {
                Some(typ) if typ.is_search_limit() => return Some(Err(typ)),
                Some(typ) if source == 0 && typ.is_function() => self.op_func_types.push(typ),
                Some(typ) => return Some(Ok(typ)),
                None => return None,
            }
        }
    }

    fn ensure_value(&mut self, source: usize, index: usize) -> Result<bool, AtomType> {
        let len = if source == 0 {
            self.op_value_types.len()
        } else {
            self.arg_types[source - 1].len()
        };
        if index < len { return Ok(true); }
        debug_assert_eq!(index, len);
        if self.exhausted[source] { return Ok(false); }
        match self.next_value(source) {
            Some(Ok(typ)) => {
                if source == 0 {
                    self.op_value_types.push(typ);
                } else {
                    self.arg_types[source - 1].push(typ);
                }
                Ok(true)
            },
            Some(Err(error)) => Err(error),
            None => {
                self.exhausted[source] = true;
                Ok(false)
            },
        }
    }

    fn next_tuple(&mut self) -> Option<Result<Atom, AtomType>> {
        if self.finished { return None; }
        let arity = self.arity();
        if !self.started {
            for source in 0..arity {
                match self.ensure_value(source, 0) {
                    Ok(true) => {},
                    Ok(false) => {
                        self.finished = true;
                        return None;
                    },
                    Err(error) => {
                        self.finished = true;
                        return Some(Err(error));
                    },
                }
            }
            self.started = true;
        } else {
            let mut source = arity;
            loop {
                if source == 0 {
                    self.finished = true;
                    return None;
                }
                source -= 1;
                let next = self.index[source] + 1;
                match self.ensure_value(source, next) {
                    Ok(true) => {
                        self.index[source] = next;
                        break;
                    },
                    Ok(false) => self.index[source] = 0,
                    Err(error) => {
                        self.finished = true;
                        return Some(Err(error));
                    },
                }
            }
        }
        let mut children = Vec::with_capacity(arity);
        children.push(self.op_value_types[self.index[0]].as_atom().clone());
        for source in 1..arity {
            children.push(self.arg_types[source - 1][self.index[source]].as_atom().clone());
        }
        Some(Ok(Atom::expr(children)))
    }

    fn finish(mut self) -> Result<ExprTypeInfo, Vec<AtomType>> {
        let mut errors = vec![];
        for source in 0..self.arity() {
            while let Some(typ) = self.next_value(source) {
                match typ {
                    Ok(typ) if source == 0 => self.op_value_types.push(typ),
                    Ok(typ) => self.arg_types[source - 1].push(typ),
                    Err(error) => errors.push(error),
                }
            }
        }
        if errors.is_empty() {
            Ok(ExprTypeInfo {
                op_func_types: self.op_func_types,
                arg_types: self.arg_types,
            })
        } else {
            Err(errors)
        }
    }
}

// TODO: Three cases here:
// 1. function call types are not found
// 2. function call type with correct arg types are found
// 3. only function call type with incorrect arg types are found
//
// In (1) we should return `vec![ Undefined ]` from `get_atom_types()` when no types found;
// In (2) we should return the type which function returns but types are never empty;
// In (3) we should return empty `Vec` when types are empty, because `validate_atom()` expects
// empty `Vec` when atom is incorrectly typed.
//
// Thus we divide these three cases in a return value of the function:
// - `None` corresponds to the first case
// - `Some(vec![...])` corresponds to the second case
// - `Some(vec![])` corresponds to the third case
//
// This is a tricky logic. To simplify it we could  separate tuple and
// function application using separate Atom types. Or use an embedded atom
// to designate function application.
fn get_application_types(atom: &Atom, expr: &ExpressionAtom, type_info: ExprTypeInfo) -> Vec<AtomType> {
    let args = get_args(expr);
    let meta_arg_types: Vec<Vec<Atom>> = args.iter().map(|a| vec![get_meta_type(a), ATOM_TYPE_ATOM]).collect();
    let mut types = Vec::with_capacity(type_info.op_func_types.len());
    for fn_type in type_info.op_func_types.into_iter() {
        let (expected, ret_typ) = get_arg_types(&fn_type);
        let fn_type_atom = fn_type.as_atom();
        check_arg_types(&type_info.arg_types, meta_arg_types.as_slice(), &mut types, expected, ret_typ, atom, fn_type_atom);
    }
    log::trace!("get_application_types: function application {} types {}", atom, types.iter().format(", "));
    types
}

enum ExpressionTypeStage<'a> {
    Tuples,
    Declared(TypeClosure<'a>),
    Reseeded(TypeClosure<'a>),
    Applications(std::vec::IntoIter<AtomType>),
    Done,
}

/// Incremental expression type enumeration. Tuple combinations and the first
/// declared closure precede a second closure over all those types, exactly as in
/// the original eager implementation. The second closure's seed prefix is not
/// yielded twice. Application types come last.
struct ExpressionTypes<'a> {
    space: &'a DynSpace,
    atom: Atom,
    expr: ExpressionAtom,
    type_info: Option<ExprTypeInfoBuilder<'a>>,
    prefix: Vec<Atom>,
    stage: ExpressionTypeStage<'a>,
    yielded_any: bool,
    undefined_if_empty: bool,
}

impl<'a> ExpressionTypes<'a> {
    fn new(space: &'a DynSpace, atom: &Atom, expr: &ExpressionAtom, undefined_if_empty: bool) -> Self {
        Self {
            space,
            atom: atom.clone(),
            expr: expr.clone(),
            type_info: Some(ExprTypeInfoBuilder::new(space, expr)),
            prefix: vec![],
            stage: ExpressionTypeStage::Tuples,
            yielded_any: false,
            undefined_if_empty,
        }
    }

    fn terminal(&mut self, typ: AtomType) -> AtomType {
        self.stage = ExpressionTypeStage::Done;
        self.yielded_any = true;
        typ
    }

    fn search_limit(&mut self, depth: usize) -> AtomType {
        let atom = self.atom.clone();
        self.terminal(AtomType::search_limit(&atom, depth))
    }
}

impl Iterator for ExpressionTypes<'_> {
    type Item = AtomType;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let stage = std::mem::replace(&mut self.stage, ExpressionTypeStage::Done);
            match stage {
                ExpressionTypeStage::Tuples => {
                    match self.type_info.as_mut().expect("expression type info").next_tuple() {
                        Some(Ok(typ)) => {
                            self.prefix.push(typ.clone());
                            self.stage = ExpressionTypeStage::Tuples;
                            self.yielded_any = true;
                            return Some(AtomType::value(typ));
                        },
                        Some(Err(error)) => return Some(self.terminal(error)),
                        None => {
                            let seeds = query_types_direct(self.space, &self.atom);
                            self.stage = ExpressionTypeStage::Declared(TypeClosure::new(self.space, seeds));
                        },
                    }
                },
                ExpressionTypeStage::Declared(mut closure) => {
                    match closure.next() {
                        Some(Ok(typ)) => {
                            self.stage = ExpressionTypeStage::Declared(closure);
                            self.yielded_any = true;
                            return Some(AtomType::value(typ));
                        },
                        Some(Err(depth)) => return Some(self.search_limit(depth)),
                        None => {
                            let mut seeds = std::mem::take(&mut self.prefix);
                            seeds.extend(closure.types);
                            let seed_count = seeds.len();
                            let mut reseeded = TypeClosure::new(self.space, seeds);
                            reseeded.yielded = seed_count;
                            self.stage = ExpressionTypeStage::Reseeded(reseeded);
                        },
                    }
                },
                ExpressionTypeStage::Reseeded(mut closure) => {
                    match closure.next() {
                        Some(Ok(typ)) => {
                            self.stage = ExpressionTypeStage::Reseeded(closure);
                            self.yielded_any = true;
                            return Some(AtomType::value(typ));
                        },
                        Some(Err(depth)) => return Some(self.search_limit(depth)),
                        None => {
                            let type_info = self.type_info.take().expect("expression type info");
                            match type_info.finish() {
                                Ok(type_info) => {
                                    let applications = get_application_types(&self.atom, &self.expr, type_info);
                                    self.stage = ExpressionTypeStage::Applications(applications.into_iter());
                                },
                                Err(errors) => {
                                    self.stage = ExpressionTypeStage::Applications(errors.into_iter());
                                },
                            }
                        },
                    }
                },
                ExpressionTypeStage::Applications(mut types) => {
                    if let Some(typ) = types.next() {
                        self.stage = ExpressionTypeStage::Applications(types);
                        self.yielded_any = true;
                        return Some(typ);
                    }
                    self.stage = ExpressionTypeStage::Done;
                },
                ExpressionTypeStage::Done => {
                    if self.undefined_if_empty && !self.yielded_any {
                        self.yielded_any = true;
                        return Some(AtomType::undefined());
                    }
                    return None;
                },
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct UndefinedTypeMatch { }

impl Display for UndefinedTypeMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", ATOM_TYPE_UNDEFINED)
    }
}

impl Grounded for UndefinedTypeMatch {
    fn type_(&self) -> Atom {
        ATOM_TYPE_TYPE
    }

    fn as_match(&self) -> Option<&dyn CustomMatch> {
        Some(self)
    }
}

impl CustomMatch for UndefinedTypeMatch {
    fn match_(&self, _other: &Atom) -> matcher::MatchResultIter {
        Box::new(std::iter::once(hyperon_atom::matcher::Bindings::new()))
    }
}

/// Matches two types and returns an iterator over resulting bindings.
///
/// # Examples
///
/// ```
/// use hyperon_atom::{expr, bind};
/// use hyperon_atom::matcher::Bindings;
/// use hyperon::metta::types::match_reducted_types;
///
/// let bindings: Vec<Bindings> = match_reducted_types(&expr!("List" t), &expr!("List" "A")).collect();
///
/// assert_eq!(bindings, vec![ bind!{ t: expr!("A") } ]);
/// ```
pub fn match_reducted_types(left: &Atom, right: &Atom) -> matcher::MatchResultIter {
    let left = replace_undefined_types(left);
    let right = replace_undefined_types(right);
    matcher::match_atoms(&left, &right)
}

fn replace_undefined_types(atom: &Atom) -> Atom {
    let mut atom = atom.clone();
    atom.iter_mut().filter(|atom| **atom == ATOM_TYPE_UNDEFINED)
        .for_each(|atom| *atom = Atom::gnd(UndefinedTypeMatch{}));
    atom
}

/// Checks if passed `atom` has the given `typ` in context of the given `space`.
/// This function can be used for a simple type check when there is no need
/// to know type parameters. It returns `false` when no match was established,
/// including an unfinished bounded search; use [`try_check_type`] to distinguish
/// that case from a completed negative result.
///
/// # Examples
///
/// ```
/// use hyperon_atom::expr;
/// use hyperon::metta::runner::*;
/// use hyperon::metta::text::SExprParser;
/// use hyperon::metta::types::check_type;
///
/// let metta = Metta::new(None);
/// metta.run(SExprParser::new("(: a A) (: a B)")).unwrap();
///
/// assert!(check_type(&metta.space(), &expr!("a"), &expr!("B")));
/// ```
pub fn check_type(space: &DynSpace, atom: &Atom, typ: &Atom) -> bool {
    try_check_type(space, atom, typ).unwrap_or(false)
}

/// Check a type without conflating exhausted search with a negative result.
/// The boolean `check_type` wrapper reports whether a type was established;
/// use this function when an indeterminate result needs to be distinguished.
pub fn try_check_type(space: &DynSpace, atom: &Atom, typ: &Atom) -> Result<bool, Atom> {
    if check_meta_type(atom, typ) { return Ok(true); }
    for actual in iter_atom_types(space, atom) {
        if actual.is_search_limit() { return Err(actual.into_error_unchecked()); }
        if actual.is_valid() && match_reducted_types(&make_variables_unique(actual.into_atom()), typ).next().is_some() {
            return Ok(true);
        }
    }
    Ok(false)
}

pub fn get_meta_type(atom: &Atom) -> Atom {
    match atom {
        Atom::Symbol(_) => ATOM_TYPE_SYMBOL,
        Atom::Variable(_) => ATOM_TYPE_VARIABLE,
        Atom::Grounded(_) => ATOM_TYPE_GROUNDED,
        Atom::Expression(_) => ATOM_TYPE_EXPRESSION,
    }
}

fn check_meta_type(atom: &Atom, typ: &Atom) -> bool {
    *typ == ATOM_TYPE_ATOM || *typ == get_meta_type(atom)
}

/// Returns true if atom is typed correctly. For example it can be used to
/// check if function arguments have correct types. It returns `false` when
/// validity was not established, including an unfinished bounded search; use
/// [`try_validate_atom`] to distinguish that case from a completed rejection.
///
/// # Examples
///
/// ```
/// use hyperon_atom::expr;
/// use hyperon::metta::runner::*;
/// use hyperon::metta::text::SExprParser;
/// use hyperon::metta::types::validate_atom;
///
/// let metta = Metta::new(None);
/// metta.run(SExprParser::new("(: foo (-> A B)) (: a A) (: b B)")).unwrap();
///
/// let space = metta.space();
/// assert!(validate_atom(&space, &expr!("foo" "a")));
/// assert!(!validate_atom(&space, &expr!("foo" "b")));
/// ```
pub fn validate_atom(space: &DynSpace, atom: &Atom) -> bool {
    try_validate_atom(space, atom).unwrap_or(false)
}

/// Validate without treating an unfinished search as a refutation.
pub fn try_validate_atom(space: &DynSpace, atom: &Atom) -> Result<bool, Atom> {
    let types = get_atom_types_internal(space, atom);
    if let Some(error) = types.iter().find(|typ| typ.is_search_limit()) {
        return Err(error.get_error().unwrap().clone());
    }
    Ok(types.iter().all(AtomType::is_valid))
}

#[cfg(test)]
mod tests {
    use super::*;
    use hyperon_atom::matcher::atoms_are_equivalent;
    use crate::metta::runner::*;
    use crate::metta::text::SExprParser;
    use hyperon_common::assert_eq_no_order;
    use hyperon_macros::metta;

    macro_rules! typ {
        ($($typ:tt)*) => { AtomType::value(metta!($($typ)*)) }
    }
    macro_rules! typ_app {
        ($($typ:tt)*) => { AtomType::application(metta!($($typ)*)) }
    }
    macro_rules! typ_err {
        ($typ:tt , $err:tt) => { AtomType::error(metta!($typ), metta!($err)) }
    }

    fn metta_space(text: &str) -> DynSpace {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let mut space = GroundingSpace::new();
        let mut parser = SExprParser::new(text);
        while let Some(atom) = parser.parse(&*metta.tokenizer().borrow()).unwrap() {
            space.add(atom);
        }
        space.into()
    }

    fn atom(atom_str: &str) -> Atom {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let mut parser = SExprParser::new(atom_str);
        let atom = parser.parse(&*metta.tokenizer().borrow()).unwrap().expect("Single atom is expected");
        atom
    }

    fn grammar_space() -> DynSpace {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "answer" ("->" "Sent" "Sent")));
        space.add(expr!(":<" "Quest" "Sent"));
        space.add(expr!(":<" ("Aux" "Subj" "Verb" "Obj") "Quest"));
        space.add(expr!(":<" "Pron" "Subj"));
        space.add(expr!(":<" "NG" "Subj"));
        space.add(expr!(":<" "Pron" "Obj"));
        space.add(expr!(":<" "NG" "Obj"));
        space.add(expr!(":<" ("Det" "Noun") "NG"));
        space.add(expr!(":" "you" "Pron"));
        space.add(expr!(":" "do" "Aux"));
        space.add(expr!(":" "do" "Verb"));
        space.add(expr!(":" "like" "Verb"));
        space.add(expr!(":" "a" "Det"));
        space.add(expr!(":" "pizza" "Noun"));
        space.into()
    }

    #[test]
    fn test_check_type() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "do" "Verb"));
        space.add(expr!(":" "do" "Aux"));
        let space = space.into();

        let aux = sym!("Aux");
        let verb = sym!("Verb");

        let nonsense = sym!("nonsense");
        assert!(check_type(&space, &nonsense, &ATOM_TYPE_UNDEFINED));
        assert!(check_type(&space, &nonsense, &aux));

        let _do = sym!("do");
        assert!(check_type(&space, &_do, &ATOM_TYPE_UNDEFINED));
        assert!(check_type(&space, &_do, &aux));
        assert!(check_type(&space, &_do, &verb));
        assert!(!check_type(&space, &_do, &sym!("Noun")));

    }

    #[test]
    fn test_check_expr_type() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "i" "Pron"));
        space.add(expr!(":" "like" "Verb"));
        space.add(expr!(":" "music" "Noun"));
        space.add(expr!(":" ("do" "you" "like" "music") "Quest"));
        space.add(expr!(":<" ("Pron" "Verb" "Noun") "Statement"));
        let space = space.into();

        let i_like_music = expr!("i" "like" "music");
        assert!(check_type(&space, &i_like_music, &ATOM_TYPE_UNDEFINED));
        assert!(check_type(&space, &i_like_music, &expr!("Pron" "Verb" "Noun")));
        assert!(check_type(&space, &i_like_music, &sym!("Statement")));

        assert!(check_type(&space, &expr!("do" "you" "like" "music"), &sym!("Quest")));
    }

    #[test]
    fn nested_type() {
        let space = metta_space("
            (: a A)
            (:< A B)
            (:< B C)
            (:< C D)
        ");

        assert!(check_type(&space, &atom("a"), &atom("D")));
    }

    #[test]
    fn nested_loop_type() {
        let space = metta_space("
            (:< B A)
            (: a A)
            (:< A B)
            (:< B C)
        ");

        assert!(check_type(&space, &atom("a"), &atom("C")));
    }

    #[test]
    fn unbound_supertype_variable_is_not_reexpanded_by_its_fresh_name() {
        let space = metta_space("(: a A) (:< A $z)");
        let types = get_atom_types(&space, &sym!("a"));
        assert_eq!(types.len(), 2);
        assert!(types.contains(&AtomType::value(sym!("A"))));
        assert!(types.iter().any(|typ| matches!(typ.as_atom(), Atom::Variable(_))));
        assert!(check_type(&space, &sym!("a"), &sym!("B")));
    }

    #[test]
    fn supertype_search_keeps_order_and_multiplicity_of_the_eager_search() {
        // D is found twice by the same round, once through B and once through C
        let space = metta_space("(: a A) (:< A B) (:< A C) (:< B D) (:< C D) (:< D A)");
        assert_eq!(get_atom_types(&space, &sym!("a")),
            vec![typ!(A), typ!(B), typ!(C), typ!(D), typ!(D)]);
        let space = metta_space("(: a A) (:< A C) (:< A B) (:< C D) (:< B D)");
        assert_eq!(get_atom_types(&space, &sym!("a")),
            vec![typ!(A), typ!(C), typ!(B), typ!(D), typ!(D)]);
        // Types which differ only by names of variables are kept when they are not
        // derived from one another
        let space = metta_space("(: a A) (:< A (Pair $x $x)) (:< A (Pair $x $y))");
        assert_eq!(get_atom_types(&space, &sym!("a")).len(), 3);
        let space = metta_space("(: a A) (:< A (Box $y)) (:< A B) (:< B (Box $z))");
        assert_eq!(get_atom_types(&space, &sym!("a")).len(), 4);
    }

    #[test]
    fn long_subtype_chain_is_enumerated_without_a_limit() {
        let mut program = String::from("(: a T0)");
        for i in 0..300 {
            program.push_str(&format!(" (:< T{} T{})", i, i + 1));
        }
        let space = metta_space(&program);
        assert_eq!(get_atom_types(&space, &sym!("a")).len(), 301);
        assert!(check_type(&space, &sym!("a"), &sym!("T300")));
        assert_eq!(get_atom_types_with_limit(&space, &sym!("a"), 64), vec![AtomType::search_limit(&sym!("a"), 64)]);
    }

    #[test]
    fn incomplete_type_closure_is_not_a_partial_answer() {
        let space = metta_space("(: a (F x)) (:< (F $x) (F (F $x)))");
        let types = get_atom_types_with_limit(&space, &sym!("a"), 3);
        assert_eq!(types, vec![AtomType::search_limit(&sym!("a"), 3)]);
        let _scope = scoped_type_search_depth(3);
        assert_eq!(try_check_type(&space, &sym!("a"), &expr!("F" ("F" "x"))), Ok(true));
        assert!(is_type_search_limit_error(&try_check_type(&space, &sym!("a"), &sym!("Unreachable")).unwrap_err()));
    }

    #[test]
    fn expression_type_search_is_lazy_without_changing_finite_results() {
        let space = metta_space("(: (p) (F x)) (:< (F $x) (F (F $x)))");
        let atom = metta!((p));
        assert_eq!(try_check_type(&space, &atom, &expr!("F" ("F" "x"))), Ok(true));
        {
            let _scope = scoped_type_search_depth(3);
            assert!(is_type_search_limit_error(
                &try_check_type(&space, &atom, &sym!("Unreachable")).unwrap_err()
            ));
        }

        let space = metta_space("(: g (F x)) (:< (F $x) (F (F $x)))");
        assert_eq!(try_check_type(&space, &metta!((g)), &metta!(((F (F x))))), Ok(true));

        let space = metta_space("(: (p) A) (:< A $z)");
        let types = get_atom_types(&space, &metta!((p)));
        assert_eq!(types.len(), 3);
        assert_eq!(types[0], typ!(A));
        assert!(types[1..].iter().all(|typ| matches!(typ.as_atom(), Atom::Variable(_))));
        assert_ne!(types[1], types[2]);

        let space = metta_space("(: (d) A) (:< A B) (:< A C) (:< B D) (:< C D) (:< D A)");
        assert_eq!(get_atom_types(&space, &metta!((d))),
            vec![typ!(A), typ!(B), typ!(C), typ!(D), typ!(D)]);
    }

    #[test]
    fn depth_limit_distinguishes_completion_from_frontier_exhaustion() {
        let space = metta_space("(: a A) (:< A B) (:< A C) (:< B D) (:< C D) (:< D A)");
        assert_eq!(get_atom_types_with_limit(&space, &sym!("a"), 1), vec![AtomType::search_limit(&sym!("a"), 1)]);
        assert_eq_no_order!(get_atom_types_with_limit(&space, &sym!("a"), 2),
            vec![typ!(A), typ!(B), typ!(C), typ!(D), typ!(D)]);
        let reordered = metta_space("(: a A) (:< D A) (:< C D) (:< B D) (:< A C) (:< A B)");
        assert_eq!(get_atom_types_with_limit(&reordered, &sym!("a"), 1), vec![AtomType::search_limit(&sym!("a"), 1)]);
        assert_eq_no_order!(get_atom_types_with_limit(&reordered, &sym!("a"), 2),
            vec![typ!(A), typ!(B), typ!(C), typ!(D), typ!(D)]);
        assert_eq!(get_atom_types_with_limit(&metta_space("(: a A) (:< A A)"), &sym!("a"), 0), vec![typ!(A)]);
    }

    #[test]
    fn nested_type_search_scope_restores_outer_policy() {
        let before = type_search_depth();
        let outer = scoped_type_search_depth(7);
        assert_eq!(type_search_depth(), 7);
        {
            let _inner = scoped_type_search_depth(2);
            assert_eq!(type_search_depth(), 2);
        }
        assert_eq!(type_search_depth(), 7);
        drop(outer);
        assert_eq!(type_search_depth(), before);
    }

    #[test]
    fn test_validate_atom() {
        let space = grammar_space();
        let expr = expr!("answer" ("do" "you" "like" ("a" "pizza")));

        assert!(validate_atom(&space, &expr));
    }

    #[test]
    fn validate_symbol() {
        let space = DynSpace::new(GroundingSpace::new());
        assert!(validate_atom(&space, &sym!("a")));
    }

    #[test]
    fn simple_types() {
        let space = metta_space("
            (: blue Color)
            (: balloon Object)
        ");

        assert!(check_type(&space, &atom("(blue balloon)"),
            &atom("(Color Object)")));
    }

    #[test]
    fn arrow_type() {
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(check_type(&space, &atom("a"), &atom("(-> B A)")));
    }

    #[test]
    fn arrow_allows_specific_type() {
        let space = metta_space("
            (: a (-> B A))
            (: b B)
            (: c C)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
        assert!(check_type(&space, &atom("(a b)"), &ATOM_TYPE_UNDEFINED));
        assert!(!validate_atom(&space, &atom("(a c)")));
        assert!(!check_type(&space, &atom("(a c)"), &ATOM_TYPE_UNDEFINED));
    }

    #[test]
    fn validate_basic_expr() {
        let space = DynSpace::new(GroundingSpace::new());
        assert!(validate_atom(&space, &expr!({5})));
        assert!(validate_atom(&space, &expr!("+" {3} {5})));
        assert!(validate_atom(&space, &expr!("=" ("f" x) x)));
    }

    #[test]
    fn simple_dep_types() {
        let space = metta_space("
            (: = (-> $t $t Prop))
            (: Entity Prop)
            (: Human (-> Entity Prop))
            (: Socrates Entity)
            (: (Human Socrates) Prop)
            (: Plato Entity)
            (: Mortal (-> Entity Prop))
            (: HumansAreMortal (-> (Human $t) (Mortal $t)))
            (: Time NotEntity)
            (: SocratesIsHuman (Human Socrates))
            (: SocratesIsMortal (Mortal Socrates))
        ");
        let t = &atom("Prop");
        assert!(check_type(&space, &atom("(Human Socrates)"), t));
        assert!(check_type(&space, &atom("(Human Plato)"), t));
        assert!(!check_type(&space, &atom("(Human Time)"), t));
        assert!(!validate_atom(&space, &atom("(Human Time)")));
        assert!(!check_type(&space, &atom("(Human Time)"), &atom("((-> Entity Prop) NotEntity)")));
        assert!(check_type(&space, &atom("(= Socrates Socrates)"), t));
        assert!(check_type(&space, &atom("(= Socrates Plato)"), t));
        assert!(check_type(&space, &atom("(= Socrates Untyped)"), t));
        assert!(!check_type(&space, &atom("(= Socrates Time)"), t));

        assert!(validate_atom(&space, &atom("(HumansAreMortal SocratesIsHuman)")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal (Human Socrates))")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal (Human Plato))")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal (Human Time))")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal Human)")));
        assert!(!check_type(&space, &atom("(HumansAreMortal (Human Socrates))"),
                            &atom("(Mortal Socrates)")));
        assert!(check_type(&space, &atom("(HumansAreMortal SocratesIsHuman)"),
                           &atom("(Mortal Socrates)")));

        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Socrates))")));
        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Plato))")));
        assert!(!check_type(&space, &atom("(= SocratesIsHuman (Human Socrates))"), t));
        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Time))")));
        assert!(validate_atom(&space, &atom("(= SocratesIsMortal (HumansAreMortal SocratesIsHuman))")));
        assert!(validate_atom(&space, &atom("(= (Mortal Socrates) (Mortal Plato))")));
    }

    #[test]
    fn dep_types_prop() {
        let space = metta_space("
            (: Sam Entity)
            (: Frog (-> Entity Prop))
            (: Green (-> Entity Prop))
            (: Croaks (-> Entity Prop))
            (: GreenAndCroaksIsFrog (-> (Green $t) (Croaks $t) (Frog $t)))
            (: SamIsGreen (Green Sam))
            (: SamCroaks (Croaks Sam))
        ");
        assert!(validate_atom(&space, &atom("(GreenAndCroaksIsFrog SamIsGreen SamCroaks)")));
        assert!(check_type(&space, &atom("(GreenAndCroaksIsFrog SamIsGreen SamCroaks)"),
                           &atom("(Frog Sam)")));
    }

    #[test]
    fn arrow_allows_undefined_type() {
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn arrow_has_type_of_returned_value() {
        let space = metta_space("
            (: a (-> B A))
            (: b B)
        ");

        assert!(check_type(&space, &atom("(a b)"), &atom("A")));
    }

    #[test]
    fn nested_arrow_type() {
        let space = metta_space("
            (: a (-> B A))
            (: h (-> (-> B A) C))
        ");

        assert!(validate_atom(&space, &atom("(h a)")));
    }

    #[test]
    fn nested_return_type() {
        let space = metta_space("
            (: a (-> B A))
            (: b B)
            (: h (-> A C))
        ");

        assert!(validate_atom(&space, &atom("(h (a b))")));
    }

    #[test]
    fn validate_non_functional_expression() {
        let space = metta_space("
            (: a A)
            (: b B)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn check_type_non_functional_expression() {
        let space = metta_space("
            (: a (-> C D))
            (: a A)
            (: b B)
        ");

        assert!(check_type(&space, &atom("(a b)"), &atom("(A B)")));
    }

    #[test]
    fn get_atom_types_symbol() {
        let space = metta_space("
            (: a A)
            (: a AA)
        ");
        assert_eq_no_order!(get_atom_types(&space, &atom("a")), vec![typ!(A), typ!(AA)]);
        assert_eq_no_order!(get_atom_types(&space, &atom("b")), vec![AtomType::undefined()]);
    }

    #[test]
    fn get_atom_types_variable() {
        let space = DynSpace::new(GroundingSpace::new());
        assert_eq!(get_atom_types(&space, &atom("$x")), vec![AtomType::undefined()]);
    }

    #[test]
    fn get_atom_types_grounded_atom() {
        let space = DynSpace::new(GroundingSpace::new());
        assert_eq!(get_atom_types(&space, &Atom::value(3)), vec![typ!(i32)]);
    }

    #[derive(Debug, Clone, PartialEq)]
    struct GroundedAtomWithParameterizedType(Atom);

    impl std::fmt::Display for GroundedAtomWithParameterizedType {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "GroundedAtomWithParameterizedType({})", self.0)
        }
    }

    impl Grounded for GroundedAtomWithParameterizedType {
        fn type_(&self) -> Atom {
            self.0.clone()
        }
    }

    #[test]
    fn get_atom_types_variables_are_substituted_for_grounded_atom_type() {
        let actual_type = Atom::var("t");
        let gnd = GroundedAtomWithParameterizedType(actual_type.clone());
        let resolved_type = get_atom_types(&GroundingSpace::new().into(), &Atom::gnd(gnd));
        assert_eq!(resolved_type.len(), 1);
        assert!(resolved_type[0].is_valid());
        assert_ne!(resolved_type[0].as_atom(), &actual_type);
        assert!(atoms_are_equivalent(resolved_type[0].as_atom(), &actual_type));
    }

    #[test]
    fn parameterized_atom_types_should_not_conflict() {
        let actual_type = Atom::expr([ARROW_SYMBOL, Atom::var("t"), Atom::var("t")]);
        let gnd_1 = GroundedAtomWithParameterizedType(actual_type.clone());
        let gnd_2 = GroundedAtomWithParameterizedType(actual_type.clone());
        let resolved_type_1 = get_atom_types(&GroundingSpace::new().into(), &Atom::gnd(gnd_1));
        let resolved_type_2 = get_atom_types(&GroundingSpace::new().into(), &Atom::gnd(gnd_2));

        //Types of gnd_1 and gnd_2 are different in the space
        assert_ne!(resolved_type_1, resolved_type_2);

        //But the types are still equivalent
        assert_eq!(resolved_type_1.len(), 1);
        assert_eq!(resolved_type_2.len(), 1);
        assert!(resolved_type_1[0].is_valid());
        assert!(resolved_type_2[0].is_valid());
        assert!(atoms_are_equivalent(resolved_type_1[0].as_atom(), &actual_type));
        assert!(atoms_are_equivalent(resolved_type_2[0].as_atom(), &actual_type));
        assert!(atoms_are_equivalent(resolved_type_1[0].as_atom(), resolved_type_2[0].as_atom()));
    }

    #[test]
    fn get_atom_types_tuple() {
        let space = metta_space("
            (: a A)
            (: a AA)
            (: b B)
            (: b BB)
        ");
        assert_eq_no_order!(get_atom_types(&space, &metta!((a b))),
            vec![typ!((A B)), typ!((AA B)), typ!((A BB)), typ!((AA BB))]);
        assert_eq_no_order!(get_atom_types(&space, &metta!((a c))),
            vec![AtomType::undefined()]);
        assert_eq_no_order!(get_atom_types(&space, &metta!((c d))), vec![AtomType::undefined()]);
    }

    #[test]
    fn get_atom_types_function_call_and_tuple() {
        let space = metta_space("
            (: a (-> B C))
            (: a A)
            (: b B)
        ");
        assert_eq!(get_atom_types(&space, &metta!((a b))), vec![typ!((A B)), typ_app!(C)]);
    }

    #[test]
    fn get_atom_types_empty_expression() {
        let space = DynSpace::new(GroundingSpace::new());
        assert_eq!(get_atom_types(&space, &Atom::expr([])), vec![AtomType::undefined()]);
    }

    #[test]
    fn get_atom_types_function_call_simple() {
        let space = metta_space("
            (: f (-> B C))
            (: b B)
        ");
        assert_eq!(get_atom_types(&space, &atom("(f b)")), vec![typ_app!(C)]);
        assert_eq!(get_atom_types(&space, &atom("(f a)")), vec![typ_app!(C)]);
    }

    #[test]
    fn get_atom_types_function_call_meta_types() {
        let space = metta_space("
            (: f_atom (-> Atom D))
            (: f_expr (-> Expression D))
            (: f_var (-> Variable D))
            (: f_sym (-> Symbol D))
            (: f_gnd (-> Grounded D))
            (: b B)
        ");
        assert_eq!(get_atom_types(&space, &metta!((f_atom b))), vec![typ_app!(D)]);
        assert_eq!(get_atom_types(&space, &metta!((f_sym b))), vec![typ_app!(D)]);
        assert_eq!(get_atom_types(&space, &metta!((f_expr b))), vec![typ_err!((-> Expression D) , (Error (f_expr b) (BadArgType 1 Expression B)))]);
        assert_eq!(get_atom_types(&space, &metta!((f_var b))), vec![typ_err!((-> Variable D) , (Error (f_var b) (BadArgType 1 Variable B)))]);
        assert_eq!(get_atom_types(&space, &metta!((f_gnd b))), vec![typ_err!((-> Grounded D) , (Error (f_gnd b) (BadArgType 1 Grounded B)))]);

        assert_eq!(get_atom_types(&space, &metta!((f_atom $b))), vec![typ_app!(D)]);
        //assert_eq!(get_atom_types(&space, &metta!((f_sym $b))), vec![]);
        //assert_eq!(get_atom_types(&space, &metta!((f_expr $b))), vec![]);
        assert_eq!(get_atom_types(&space, &metta!((f_var $b))), vec![typ_app!(D)]);
        //assert_eq!(get_atom_types(&space, &metta!((f_gnd $b))), vec![]);

        assert_eq!(get_atom_types(&space, &metta!((f_atom (b)))), vec![typ_app!(D)]);
        // Here and below: when interpreter cannot find a function type for
        // expression it evaluates it. Thus any argument expression without
        // a function type can potentially suit as a legal argument.
        assert_eq!(get_atom_types(&space, &metta!((f_sym (b)))), vec![typ_err!((-> Symbol D) , (Error (f_sym (b)) (BadArgType 1 Symbol (B))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_expr (b)))), vec![typ_app!(D)]);
        assert_eq!(get_atom_types(&space, &metta!((f_var (b)))), vec![typ_err!((-> Variable D) , (Error (f_var (b)) (BadArgType 1 Variable (B))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_gnd (b)))), vec![typ_err!((-> Grounded D) , (Error (f_gnd (b)) (BadArgType 1 Grounded (B))))]);

        assert_eq!(get_atom_types(&space, &metta!((f_atom 1))), vec![typ_app!(D)]);
        assert_eq!(get_atom_types(&space, &metta!((f_sym 1))), vec![typ_err!((-> Symbol D) , (Error (f_sym 1) (BadArgType 1 Symbol Number)))]);
        assert_eq!(get_atom_types(&space, &metta!((f_expr 1))), vec![typ_err!((-> Expression D) , (Error (f_expr 1) (BadArgType 1 Expression Number)))]);
        assert_eq!(get_atom_types(&space, &metta!((f_var 1))), vec![typ_err!((-> Variable D) , (Error (f_var 1) (BadArgType 1 Variable Number)))]);
        assert_eq!(get_atom_types(&space, &metta!((f_gnd 1))), vec![typ_app!(D)]);
    }

    #[test]
    fn get_atom_types_function_call_incorrect_arguments() {
        let space = metta_space("
            (: a (-> C D))
            (: b B)
        ");
        assert_eq!(get_atom_types(&space, &atom("(a b)")), vec![typ_err!((-> C D), (Error (a b) (BadArgType 1 C B)))]);
    }

    #[test]
    fn get_atom_types_function_call_parameterized_types() {
        let space = metta_space("
            (: = (-> $t $t Type))
            (: Some (-> $p Type))
            (: foo (-> (Some P)))
            (: bar (-> $p (Some $p)))
            (: p X)
            (: p P)
        ");
        assert_eq!(get_atom_types(&space, &atom("(= (foo) (bar p))")),
            vec![typ_app!(Type)]);
    }

    #[test]
    fn check_type_parameterized_type_no_variable_bindings() {
        let space = metta_space("
            (: Pair (-> $a $b Type))
            (: A (Pair $c $d))
        ");

        assert!(check_type(&space, &atom("A"), &atom("(Pair $e $f)")));
        assert!(check_type(&space, &atom("A"), &atom("(Pair $f $f)")));
    }

    #[test]
    fn check_type_dependent_type_symbol_param() {
        let space = metta_space("
            (: === (-> $a $a Type))
            (: Refl (-> $x (=== $x $x)))

            (: TermSym A)
        ");

        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A A)")));
        assert!(!check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A B)")));
        assert!(!check_type(&space, &atom("(Refl TermSym)"), &atom("(=== 42 A)")));
        assert!(!check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A 42)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== $a A)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A $a)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== $a $a)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== $a $b)")));
    }

    #[test]
    fn check_type_dependent_type_grounded_param() {
        let space = metta_space("
            (: === (-> $a $a Type))
            (: Refl (-> $x (=== $x $x)))

            (: TermGnd 42)
        ");

        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 42)")));
        assert!(!check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 24)")));
        assert!(!check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 A)")));
        assert!(!check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== A 42)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== $a 42)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 $a)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== $a $a)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== $a $b)")));
    }

    #[test]
    fn check_type_accept_meta_type() {
        let type_r = &atom("R");
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: aF (-> A R))
            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(check_type(&space, &atom("(aF a)"), type_r));
        assert!(!check_type(&space, &atom("(aF b)"), type_r));
        // TODO: (aF b) is incorrectly typed, but it is an Atom and check_type
        // returns True
        assert!(check_type(&space, &atom("(aF b)"), &ATOM_TYPE_ATOM));
        assert!(check_type(&space, &atom("(aF b)"), &ATOM_TYPE_EXPRESSION));
        assert!(!check_type(&space, &atom("(aF b)"), &ATOM_TYPE_SYMBOL));

        assert!(check_type(&space, &atom("(atomF a)"), type_r));
        assert!(check_type(&space, &atom("(atomF ())"), type_r));
        assert!(check_type(&space, &atom("(exprF ())"), type_r));
        assert!(!check_type(&space, &atom("(exprF a)"), type_r));
        assert!(check_type(&space, &atom("(gndF 1)"), type_r));
        assert!(!check_type(&space, &atom("(gndF a)"), type_r));
        assert!(check_type(&space, &atom("(symF a)"), type_r));
        assert!(!check_type(&space, &atom("(symF 1)"), type_r));
        assert!(check_type(&space, &atom("(varF $a)"), type_r));
        assert!(!check_type(&space, &atom("(varF a)"), type_r));
    }

    #[test]
    fn check_type_return_meta_type() {
        let type_r = &atom("R");
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: atomR (-> A Atom))
            (: exprR (-> A Expression))
            (: gndR (-> A Grounded))
            (: symR (-> A Symbol))
            (: varR (-> A Variable))

            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(check_type(&space, &atom("(atomF (atomR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (exprR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (gndR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (symR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (varR a))"), type_r));

        assert!(check_type(&space, &atom("(exprF (exprR a))"), type_r));
        // TODO: it is incorrectly typed, but (atomR a) is an Expression and
        // check_type returns True
        assert!(check_type(&space, &atom("(exprF (atomR a))"), type_r));

        assert!(check_type(&space, &atom("(gndF (gndR a))"), type_r));
        assert!(!check_type(&space, &atom("(gndF (atomR a))"), type_r));
        assert!(check_type(&space, &atom("(symF (symR a))"), type_r));
        assert!(!check_type(&space, &atom("(symF (atomR a))"), type_r));
        assert!(check_type(&space, &atom("(varF (varR a))"), type_r));
        assert!(!check_type(&space, &atom("(varF (atomR a))"), type_r));
    }

    #[test]
    fn validate_atom_accept_meta_type() {
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: aF (-> A R))
            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(validate_atom(&space, &atom("(aF a)")));
        assert!(!validate_atom(&space, &atom("(aF b)")));

        assert!(validate_atom(&space, &atom("(atomF a)")));
        assert!(validate_atom(&space, &atom("(atomF ())")));
        assert!(validate_atom(&space, &atom("(exprF ())")));
        assert!(!validate_atom(&space, &atom("(exprF a)")));
        assert!(validate_atom(&space, &atom("(gndF 1)")));
        assert!(!validate_atom(&space, &atom("(gndF a)")));
        assert!(validate_atom(&space, &atom("(symF a)")));
        assert!(!validate_atom(&space, &atom("(symF 1)")));
        assert!(validate_atom(&space, &atom("(varF $a)")));
        assert!(!validate_atom(&space, &atom("(varF a)")));
    }

    #[test]
    fn validate_atom_return_meta_type() {
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: atomR (-> A Atom))
            (: exprR (-> A Expression))
            (: gndR (-> A Grounded))
            (: symR (-> A Symbol))
            (: varR (-> A Variable))

            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(validate_atom(&space, &atom("(atomF (atomR a))")));
        assert!(validate_atom(&space, &atom("(atomF (exprR a))")));
        assert!(validate_atom(&space, &atom("(atomF (gndR a))")));
        assert!(validate_atom(&space, &atom("(atomF (symR a))")));
        assert!(validate_atom(&space, &atom("(atomF (varR a))")));

        assert!(validate_atom(&space, &atom("(exprF (exprR a))")));
        // TODO: (exprF (atomR a)) is incorrectly typed, but (atomR a)
        // is an Expression and validate_atom returns True
        assert!(validate_atom(&space, &atom("(exprF (atomR a))")));

        assert!(validate_atom(&space, &atom("(gndF (gndR a))")));
        assert!(!validate_atom(&space, &atom("(gndF (atomR a))")));
        assert!(validate_atom(&space, &atom("(symF (symR a))")));
        assert!(!validate_atom(&space, &atom("(symF (atomR a))")));
        assert!(validate_atom(&space, &atom("(varF (varR a))")));
        assert!(!validate_atom(&space, &atom("(varF (atomR a))")));
    }

    #[test]
    fn tuple_with_undefined_member() {
        let space = metta_space("(: F %Undefined%)");
        assert_eq!(get_atom_types_internal(&space, &atom("(F arg)")), vec![]);

        let gnd = GroundedAtomWithParameterizedType(ATOM_TYPE_UNDEFINED);
        assert_eq!(get_atom_types_internal(&space, &expr!({gnd} "a")), vec![]);
    }

}
