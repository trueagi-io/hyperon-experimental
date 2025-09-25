use hyperon_atom::*;
use hyperon_space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::stdlib::{grounded_op, unit_result, regex};
use hyperon_atom::gnd::GroundedFunctionAtom;

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct NewSpaceOp {}

grounded_op!(NewSpaceOp, "new-space");

impl Grounded for NewSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SPACE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NewSpaceOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 0 {
            let space = Atom::gnd(DynSpace::new(GroundingSpace::new()));
            Ok(vec![space])
        } else {
            Err("new-space doesn't expect arguments".into())
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StateAtom {
    state: Rc<RefCell<(Atom, Atom)>>
}

impl StateAtom {
    pub fn new(atom: Atom, typ: Atom) -> Self {
        Self{ state: Rc::new(RefCell::new((atom, typ))) }
    }
}

impl Display for StateAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(State {})", self.state.borrow().0)
    }
}

impl Grounded for StateAtom {
    fn type_(&self) -> Atom {
        self.state.borrow().1.clone()
    }
}

fn new_state(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = "new-state expects atom as a first argument and non-empty list of types as a second argument";
    let atom = args.get(0).ok_or(arg_error)?;
    let typ = args.get(1)
        .and_then(|t| TryInto::<&ExpressionAtom>::try_into(t).ok())
        // TODO: when grounded atom will be able returning few types then
        // all types should be returned
        .and_then(|e| e.children().get(0))
        .ok_or(arg_error)?;
    Ok(vec![Atom::gnd(StateAtom::new(atom.clone(),
        Atom::expr([Atom::sym("StateMonad"), typ.clone()])))])
}

#[derive(Clone, Debug)]
pub struct GetStateOp { }

grounded_op!(GetStateOp, "get-state");

impl Grounded for GetStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!("StateMonad" tgso), expr!(tgso)])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetStateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "get-state expects single state atom as an argument";
        let state = args.get(0).ok_or(arg_error)?;
        let atom = Atom::as_gnd::<StateAtom>(state).ok_or(arg_error)?;
        Ok(vec![atom.state.borrow().0.clone()])
    }
}

#[derive(Clone, Debug)]
pub struct ChangeStateOp { }

grounded_op!(ChangeStateOp, "change-state!");

impl Grounded for ChangeStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!("StateMonad" tcso), expr!(tcso), expr!("StateMonad" tcso)])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for ChangeStateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "change-state! expects a state atom and its new value as arguments";
        let atom = args.get(0).ok_or(arg_error)?;
        let state = Atom::as_gnd::<StateAtom>(atom).ok_or("change-state! expects a state as the first argument")?;
        let new_value = args.get(1).ok_or(arg_error)?;
        state.state.borrow_mut().0 = new_value.clone();
        Ok(vec![atom.clone()])
    }
}

#[derive(Clone, Debug)]
pub struct GetAtomsOp {}

grounded_op!(GetAtomsOp, "get-atoms");

impl Grounded for GetAtomsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SPACE, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetAtomsOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-atoms expects one argument: space");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-atoms expects a space as its argument")?;
        let mut result = Vec::new();
        space.borrow().visit(&mut |atom: std::borrow::Cow<Atom>| {
            result.push(make_variables_unique(atom.into_owned()))
        }).map_or(Err(ExecError::Runtime("Unsupported Operation. Can't traverse atoms in this space".to_string())), |_| Ok(result))
    }
}

#[derive(Clone, Debug)]
pub struct AddAtomOp {}

grounded_op!(AddAtomOp, "add-atom");

impl Grounded for AddAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SPACE,
            ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AddAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("add-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("add-atom expects a space as the first argument")?;
        space.borrow_mut().add(atom.clone());
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct RemoveAtomOp {}

grounded_op!(RemoveAtomOp, "remove-atom");

impl Grounded for RemoveAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SPACE,
            ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RemoveAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("remove-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("remove-atom expects a space as the first argument")?;
        space.borrow_mut().remove(atom);
        // TODO? Is it necessary to distinguish whether the atom was removed or not?
        unit_result()
    }
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let new_space_op = Atom::gnd(NewSpaceOp{});
    tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
    let add_atom_op = Atom::gnd(AddAtomOp{});
    tref.register_token(regex(r"add-atom"), move |_| { add_atom_op.clone() });
    let remove_atom_op = Atom::gnd(RemoveAtomOp{});
    tref.register_token(regex(r"remove-atom"), move |_| { remove_atom_op.clone() });
    tref.register_function(GroundedFunctionAtom::new(
            r"_new-state".into(),
            expr!("->" t "Expression" ("StateMonad" t)),
            |args: &[Atom]| -> Result<Vec<Atom>, ExecError> { new_state(args) }, 
        ));
    let change_state_op = Atom::gnd(ChangeStateOp{});
    tref.register_token(regex(r"change-state!"), move |_| { change_state_op.clone() });
    let get_state_op = Atom::gnd(GetStateOp{});
    tref.register_token(regex(r"get-state"), move |_| { get_state_op.clone() });
    let get_atoms_op = Atom::gnd(GetAtomsOp{});
    tref.register_token(regex(r"get-atoms"), move |_| { get_atoms_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::space::grounding::metta_space;
    use crate::metta::runner::Metta;
    use hyperon_common::assert_eq_no_order;
    use hyperon_macros::metta;
    use crate as hyperon;

    #[test]
    fn mod_space_op() {
        let program = r#"
            !(bind! &new_space (new-space))
            !(add-atom &new_space (mod-space! stdlib))
            !(get-atoms &new_space)
        "#;
        let runner = Metta::new(Some(runner::environment::EnvBuilder::test_env()));
        let result = runner.run(SExprParser::new(program)).unwrap();

        assert_eq!(result[2], vec![Atom::expr([Atom::gnd(super::super::module::ModSpaceOp::new(runner.clone())), Atom::sym("stdlib")])]);
    }

    fn collect_atoms(space: &DynSpace) -> Vec<Atom> {
        let mut atoms = Vec::new();
        space.borrow().visit(&mut |atom: std::borrow::Cow<Atom>| atoms.push(atom.into_owned()))
            .expect("Space::visit is not implemented");
        atoms
    }

    #[test]
    fn remove_atom_op() {
        let space = metta_space("
            (foo bar)
            (bar foo)
        ");
        let satom = Atom::gnd(space.clone());
        let res = RemoveAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
        // REM: can return Bool in future
        assert_eq!(res, vec![UNIT_ATOM]);
        let space_atoms = collect_atoms(&space);
        assert_eq_no_order!(space_atoms, vec![expr!(("bar" "foo"))]);
    }

    #[test]
    fn get_atoms_op() {
        let space = metta_space("
            (foo bar)
            (bar foo)
        ");
        let satom = Atom::gnd(space.clone());
        let res = GetAtomsOp{}.execute(&mut vec![satom]).expect("No result returned");
        let space_atoms = collect_atoms(&space);
        assert_eq_no_order!(res, space_atoms);
        assert_eq_no_order!(res, vec![expr!(("foo" "bar")), expr!(("bar" "foo"))]);
    }

    #[test]
    fn new_space_op() {
        let res = NewSpaceOp{}.execute(&mut vec![]).expect("No result returned");
        let space = res.get(0).expect("Result is empty");
        let space = space.as_gnd::<DynSpace>().expect("Result is not space");
        let space_atoms = collect_atoms(&space);
        assert_eq_no_order!(space_atoms, Vec::<Atom>::new());
    }

    #[test]
    fn add_atom_op() {
        let space = DynSpace::new(GroundingSpace::new());
        let satom = Atom::gnd(space.clone());
        let res = AddAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
        assert_eq!(res, vec![UNIT_ATOM]);
        let space_atoms = collect_atoms(&space);
        assert_eq_no_order!(space_atoms, vec![expr!(("foo" "bar"))]);
    }

    #[test]
    fn state_ops() {
        let program = r#"
            (: a A)
            (: aa A)
            (: b B)
            (: F (-> $t $t))
            !(bind! &stateAB (new-state (F a)))
            !(change-state! &stateAB (F aa))
            !(get-state &stateAB)
            !(change-state! &stateAB (F b))
        "#;
        let runner = Metta::new(Some(runner::environment::EnvBuilder::test_env()));
        let result = runner.run(SExprParser::new(program));

        let faa = StateAtom::new(metta!((F aa)), metta!((StateMonad A)));
        assert_eq!(result, Ok(vec![
            vec![UNIT_ATOM],
            vec![Atom::gnd(faa.clone())],
            vec![metta!((F aa))],
            vec![metta!((Error ({ChangeStateOp{}} {faa} (F b)) (BadArgType 2 A B)))],
        ]));
    }
}
