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
use hyperon_atom::matcher::{Bindings, BindingsSet, apply_bindings_to_atom_move};
use hyperon_space::DynSpace;

use std::fmt::{Display, Debug};
use itertools::Itertools;
use hyperon_common::collections::CowArray;
use crate::metta::runner::number::Number;

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

fn add_super_types(space: &DynSpace, sub_types: &mut Vec<Atom>, from: usize) {
    let mut types = Vec::new();
    sub_types.iter().skip(from).for_each(|typ| {
        for typ in query_super_types(space, typ) {
            if !sub_types.contains(&typ) {
                types.push(typ);
            }
        }
    });
    if !types.is_empty() {
        let from = sub_types.len();
        sub_types.append(&mut types.clone());
        add_super_types(space, sub_types, from);
    }
}

fn check_arg_types(actual: &[Vec<AtomType>], meta: &[Vec<Atom>], expected: &[Atom]) -> BindingsSet {
    if actual.len() != expected.len() {
        BindingsSet::empty()
    } else {
        check_arg_types_internal(actual, meta, expected, Bindings::new())
    }
}

fn check_arg_types_internal(actual: &[Vec<AtomType>], meta: &[Vec<Atom>], expected: &[Atom], bindings: Bindings) -> BindingsSet {
    log::trace!("check_arg_types: actual: {}, expected: {}",
        actual.iter().format_with(", ", |v, f| f(&format_args!("{}", v.iter().format(", ")))),
        expected.iter().format(", "));
    let matched = match (actual, meta, expected) {
        ([actual, actual_tail @ ..], [meta, meta_tail @ ..], [expected, expected_tail @ ..]) => {
            let undefined_or_meta = actual.is_empty()
                || *expected == ATOM_TYPE_UNDEFINED
                || meta.contains(expected);
            let matches: &mut dyn Iterator<Item=Bindings> = if undefined_or_meta {
                &mut std::iter::once(Bindings::new())
            } else {
                &mut actual.into_iter().flat_map(|typ| match_reducted_types(typ.as_atom(), expected))
            };
            matches
                .flat_map(|b| b.merge(&bindings))
                .flat_map(|b| check_arg_types_internal(actual_tail, meta_tail, expected_tail, b))
                .collect()
        },
        ([], [], []) => BindingsSet::from(bindings),
        _ => unreachable!(),
    };
    log::trace!("check_arg_types: actual: {}, expected: {}, matched: {}",
        actual.iter().format_with(", ", |v, f| f(&format_args!("{}", v.iter().format(", ")))),
        expected.iter().format(", "),
        matched);
    matched
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

fn query_types(space: &DynSpace, atom: &Atom) -> Vec<Atom> {
    let var_x = VariableAtom::new("X").make_unique();
    let types = query_has_type(space, atom, &Atom::Variable(var_x.clone()));
    let atom_x = Atom::Variable(var_x);
    let mut types = types.into_iter().filter_map(|bindings| {
        let atom = apply_bindings_to_atom_move(atom_x.clone(), &bindings);
        if atom_x == atom || atom == ATOM_TYPE_UNDEFINED {
            None
        } else {
            Some(atom)
        }
    }).collect();
    add_super_types(space, &mut types, 0);
    types
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

#[derive(Debug, PartialEq)]
pub struct AtomType {
    typ: Atom,
    is_function: bool,
    info: TypeInfo,
}

#[derive(Debug, PartialEq)]
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
/// assert_eq_no_order!(get_atom_types(&space, &metta!((f b))), vec![AtomType::error(metta!((-> A B)), metta!((Error (f b) (BadType 0 (B) (A)))))]);
/// ```
pub fn get_atom_types(space: &DynSpace, atom: &Atom) -> Vec<AtomType> {
    let atom_types = get_atom_types_internal(space, atom);
    if atom_types.is_empty() {
        vec![AtomType::undefined()]
    } else {
        atom_types
    }
}

struct ExprTypeInfo {
    op_value_types: Vec<AtomType>,
    op_func_types: Vec<AtomType>,
    arg_types: Vec<Vec<AtomType>>,
}

impl ExprTypeInfo {
    fn new(space: &DynSpace, expr: &ExpressionAtom) -> Self {
        let (op, args) = expr.children().split_first().unwrap();
        let op_types = get_atom_types_internal(space, op);
        let mut op_func_types = Vec::with_capacity(op_types.len());
        let mut op_value_types = Vec::with_capacity(op_types.len());
        op_types.into_iter().for_each(|t| {
            if t.is_function() {
                op_func_types.push(t);
            } else {
                op_value_types.push(t);
            }
        });
        let arg_types: Vec<Vec<AtomType>> = args.iter()
            .map(|a| get_atom_types_internal(space, a)).collect();
            // Code below allows returning partially defined tuples
            // for example (a c) where (: a A) has type (A %Undefined%)
            // see get_atom_types_tuple test
            //.map(|a| {
                //let mut types = get_atom_types_internal(space, a);
                //if types.is_empty() {
                    //types.push(AtomType::value(ATOM_TYPE_UNDEFINED));
                //}
                //types
            //}).collect();
        Self{ op_value_types, op_func_types, arg_types }
    }

    #[inline]
    fn arity(&self) -> usize {
        self.arg_types.len() + 1
    }
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
        Atom::Symbol(_) => query_types(space, atom).into_iter()
            .map(AtomType::value).collect(),
        // FIXME: incorrect type
        Atom::Expression(expr) if expr.children().len() == 0 => vec![],
        Atom::Expression(expr) => {
            let type_info = ExprTypeInfo::new(space, expr);
            let mut types = get_tuple_types(space, atom, &type_info);
            let applications = get_application_types(atom, expr, type_info);
            types.extend(applications.into_iter());
            types
        },
    };
    log::debug!("get_atom_types_internal: return atom {} types {}", atom, types.iter().format(", "));
    types
}

struct TupleIndex<'a> {
    type_info: &'a ExprTypeInfo,
    index: Vec<usize>,
    max: Vec<usize>,
    size: usize,
}

impl<'a> TupleIndex<'a> {
    fn new(type_info: &'a ExprTypeInfo) -> Option<Self> {
        let n_of_types = type_info.arg_types.iter()
            .fold(type_info.op_value_types.len(), |n, types| n * types.len());
        if n_of_types == 0 {
            return None
        }
        let arity = type_info.arity();
        let mut max = Vec::with_capacity(arity);
        max.push(type_info.op_value_types.len());
        max.extend(type_info.arg_types.iter().map(Vec::len));
        let mut index = vec![0; arity];
        index[arity - 1] = usize::wrapping_sub(index[arity - 1], 1);
        Some(Self{ type_info, index, max, size: n_of_types })
    }

    #[inline]
    fn inc(&mut self) -> bool {
        let mut i = self.index.len() - 1;
        loop {
            let d = usize::wrapping_add(self.index[i], 1);
            if d < self.max[i] {
                self.index[i] = d;
                return true;
            } else {
                if i == 0 {
                    return false;
                } else {
                    self.index[i] = 0;
                    i -= 1;
                }
            }
        }
    }

    #[inline]
    fn size(&self) -> usize {
        self.size
    }
}

impl<'a> Iterator for TupleIndex<'a> {
    type Item = Vec<Atom>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.inc() {
            let arity = self.index.len();
            let mut v = Vec::with_capacity(arity);
            v.push(self.type_info.op_value_types[self.index[0]].as_atom().clone());
            for i in 1..arity {
                v.push(self.type_info.arg_types[i - 1][self.index[i]].as_atom().clone());
            }
            Some(v)
        } else {
            None
        }
    }
}

fn get_tuple_types(space: &DynSpace, atom: &Atom, type_info: &ExprTypeInfo) -> Vec<AtomType> {
    let mut types = if let Some(index) = TupleIndex::new(type_info) {
        let mut types = Vec::with_capacity(index.size());
        index.for_each(|v| types.push(Atom::expr(v)));
        types
    } else {
        vec![]
    };

    types.append(&mut query_types(space, atom));
    add_super_types(space, &mut types, 0);
    // FIXME: ineffective
    let types: Vec<AtomType> = types.into_iter().map(AtomType::value).collect();
    log::trace!("get_tuple_types: tuple {} types {}", atom, types.iter().format(", "));
    types
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
        let (expected_arg_types, ret_typ) = get_arg_types(&fn_type);
        let correct = check_arg_types(&type_info.arg_types, meta_arg_types.as_slice(), expected_arg_types);
        if correct.is_empty() {
            let expected_types_expr = Atom::Expression(ExpressionAtom::new(CowArray::from(expected_arg_types.iter().map(|a| a.clone()).collect_vec())));
            let actual_types_expr = Atom::Expression(ExpressionAtom::new(CowArray::from(type_info.arg_types.iter().map(|a| a.iter().nth(0).unwrap().typ.clone()).collect_vec())));
            types.push(AtomType::error(fn_type.into_atom(), Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::expr([BAD_TYPE_SYMBOL, Atom::gnd(Number::Integer(0i64)), actual_types_expr, expected_types_expr])])));
        } else {
            for bindings in correct {
                types.push(AtomType::application(apply_bindings_to_atom_move(ret_typ.clone(), &bindings)));
            }
        }
    }
    log::trace!("get_application_types: function application {} types {}", atom, types.iter().format(", "));
    types
}

#[derive(Clone, PartialEq, Debug)]
struct UndefinedTypeMatch { }

impl std::fmt::Display for UndefinedTypeMatch {
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

fn get_matched_types(space: &DynSpace, atom: &Atom, typ: &Atom) -> Vec<(Atom, Bindings)> {
    let types = get_atom_types(space, atom);
    types.into_iter().filter(AtomType::is_valid).flat_map(|t| {
        // TODO: write a unit test
        let t = make_variables_unique(t.into_atom());
        match_reducted_types(&t, typ).map(move |bindings| (t.clone(), bindings))
    }).collect()
}

/// Checks if passed `atom` has the given `typ` in context of the given `space`.
/// This function can be used for a simple type check when there is no need
/// to know type parameters.
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
    check_meta_type(atom, typ) || !get_matched_types(space, atom, typ).is_empty()
}

/// Finds all types of the passed `atom` which matches the given `typ` in
/// context of the given `space`. Returns vector of matched types with type
/// parameter bindings.
///
/// # Examples
///
/// ```
/// use hyperon_atom::{expr, bind};
/// use hyperon::metta::runner::*;
/// use hyperon::metta::text::SExprParser;
/// use hyperon::metta::types::get_type_bindings;
///
/// let metta = Metta::new(None);
/// metta.run(SExprParser::new("(: a (List A))")).unwrap();
/// let types = get_type_bindings(&metta.space(), &expr!("a"), &expr!("List" t));
///
/// assert_eq!(types, vec![(expr!("List" "A"), bind!{ t: expr!("A") })]);
/// ```
pub fn get_type_bindings(space: &DynSpace, atom: &Atom, typ: &Atom) -> Vec<(Atom, Bindings)> {
    let mut result = Vec::new();
    if check_meta_type(atom, typ) {
        result.push((typ.clone(), Bindings::new()));
    }
    result.append(&mut get_matched_types(space, atom, typ));
    if result.len() > 1 {
        result = result.into_iter().filter(|(typ, _)| *typ != ATOM_TYPE_UNDEFINED).collect();
    }
    result
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
/// check if function arguments have correct types.
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
    get_atom_types_internal(space, atom).iter().all(AtomType::is_valid)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as hyperon;
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
        assert_eq!(get_atom_types(&space, &metta!((f_expr b))), vec![typ_err!((-> Expression D) , (Error (f_expr b) (BadType 0 (B) (Expression))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_var b))), vec![typ_err!((-> Variable D) , (Error (f_var b) (BadType 0 (B) (Variable))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_gnd b))), vec![typ_err!((-> Grounded D) , (Error (f_gnd b) (BadType 0 (B) (Grounded))))]);

        assert_eq!(get_atom_types(&space, &metta!((f_atom $b))), vec![typ_app!(D)]);
        //assert_eq!(get_atom_types(&space, &metta!((f_sym $b))), vec![]);
        //assert_eq!(get_atom_types(&space, &metta!((f_expr $b))), vec![]);
        assert_eq!(get_atom_types(&space, &metta!((f_var $b))), vec![typ_app!(D)]);
        //assert_eq!(get_atom_types(&space, &metta!((f_gnd $b))), vec![]);

        assert_eq!(get_atom_types(&space, &metta!((f_atom (b)))), vec![typ_app!(D)]);
        // Here and below: when interpreter cannot find a function type for
        // expression it evaluates it. Thus any argument expression without
        // a function type can potentially suit as a legal argument.
        assert_eq!(get_atom_types(&space, &metta!((f_sym (b)))), vec![typ_err!((-> Symbol D) , (Error (f_sym (b)) (BadType 0 ((B)) (Symbol))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_expr (b)))), vec![typ_app!(D)]);
        assert_eq!(get_atom_types(&space, &metta!((f_var (b)))), vec![typ_err!((-> Variable D) , (Error (f_var (b)) (BadType 0 ((B)) (Variable))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_gnd (b)))), vec![typ_err!((-> Grounded D) , (Error (f_gnd (b)) (BadType 0 ((B)) (Grounded))))]);

        assert_eq!(get_atom_types(&space, &metta!((f_atom 1))), vec![typ_app!(D)]);
        assert_eq!(get_atom_types(&space, &metta!((f_sym 1))), vec![typ_err!((-> Symbol D) , (Error (f_sym 1) (BadType 0 (Number) (Symbol))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_expr 1))), vec![typ_err!((-> Expression D) , (Error (f_expr 1) (BadType 0 (Number) (Expression))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_var 1))), vec![typ_err!((-> Variable D) , (Error (f_var 1) (BadType 0 (Number) (Variable))))]);
        assert_eq!(get_atom_types(&space, &metta!((f_gnd 1))), vec![typ_app!(D)]);
    }

    #[test]
    fn get_atom_types_function_call_incorrect_arguments() {
        let space = metta_space("
            (: a (-> C D))
            (: b B)
        ");
        assert_eq!(get_atom_types(&space, &atom("(a b)")), vec![typ_err!((-> C D), (Error (a b) (BadType 0 (B) (C))))]);
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
