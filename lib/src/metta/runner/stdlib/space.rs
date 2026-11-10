use hyperon_atom::*;
use hyperon_space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::stdlib::{grounded_op, unit_result, regex};
use hyperon_atom::gnd::GroundedFunctionAtom;

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Debug};
use std::collections::HashSet;

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

#[derive(Clone, PartialEq)]
pub struct StateAtom {
    state: Rc<RefCell<(Atom, Atom)>>
}

type StateIdentity = *const RefCell<(Atom, Atom)>;
thread_local! {
    // Only cells on the current formatting stack, not every cell ever seen.
    static DISPLAY_STATES: RefCell<HashSet<StateIdentity>> = RefCell::new(HashSet::new());
}

struct StateDisplayGuard(StateIdentity);
impl Drop for StateDisplayGuard {
    fn drop(&mut self) {
        DISPLAY_STATES.with(|states| { states.borrow_mut().remove(&self.0); });
    }
}

impl Debug for StateAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let identity = Rc::as_ptr(&self.state);
        if !DISPLAY_STATES.with(|states| states.borrow_mut().insert(identity)) {
            return write!(f, "(State <cycle>)");
        }
        let _guard = StateDisplayGuard(identity);
        f.debug_struct("StateAtom").field("state", &self.state).finish()
    }
}

impl StateAtom {
    pub fn new(atom: Atom, typ: Atom) -> Self {
        Self{ state: Rc::new(RefCell::new((atom, typ))) }
    }
}

impl Display for StateAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let identity = Rc::as_ptr(&self.state);
        if !DISPLAY_STATES.with(|states| states.borrow_mut().insert(identity)) {
            return write!(f, "(State <cycle>)");
        }
        let _guard = StateDisplayGuard(identity);
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

    #[test]
    fn state_display_and_debug_preserve_acyclic_formatting() {
        let state = StateAtom::new(expr!("pair" "leaf"), sym!("Type"));
        assert_eq!(state.to_string(), "(State (pair leaf))");
        assert_eq!(format!("{state:?}"), format!("StateAtom {{ state: {:?} }}", state.state));
        assert_eq!(state.to_string(), "(State (pair leaf))");
    }

    #[test]
    fn state_display_and_debug_detect_self_cycle() {
        let state = StateAtom::new(sym!("leaf"), sym!("Type"));
        state.state.borrow_mut().0 = Atom::expr([sym!("wrap"), Atom::gnd(state.clone())]);

        assert_eq!(state.to_string(), "(State (wrap (State <cycle>)))");
        let debug = format!("{state:?}");
        assert!(debug.contains("(State <cycle>)"), "{}", debug);
        assert_eq!(state.to_string(), "(State (wrap (State <cycle>)))");

        state.state.borrow_mut().0 = sym!("leaf");
        assert_eq!(state.to_string(), "(State leaf)");
    }

    #[test]
    fn state_display_and_debug_detect_mutual_cycle() {
        let first = StateAtom::new(sym!("leaf"), sym!("Type"));
        let second = StateAtom::new(sym!("leaf"), sym!("Type"));
        first.state.borrow_mut().0 = Atom::expr([sym!("first"), Atom::gnd(second.clone())]);
        second.state.borrow_mut().0 = Atom::expr([sym!("second"), Atom::gnd(first.clone())]);

        assert_eq!(first.to_string(), "(State (first (State (second (State <cycle>)))))");
        let debug = format!("{first:?}");
        assert!(debug.contains("(State <cycle>)"), "{}", debug);

        first.state.borrow_mut().0 = sym!("leaf");
        second.state.borrow_mut().0 = sym!("leaf");
    }

    #[test]
    fn state_display_allows_repeated_shared_children() {
        let child = StateAtom::new(sym!("leaf"), sym!("Type"));
        let parent = StateAtom::new(
            Atom::expr([Atom::gnd(child.clone()), Atom::gnd(child)]),
            sym!("Type")
        );

        assert_eq!(parent.to_string(), "(State ((State leaf) (State leaf)))");
        assert_eq!(parent.to_string(), "(State ((State leaf) (State leaf)))");
    }

    #[test]
    fn state_display_guard_survives_nested_formatter_failure() {
        struct FailingNestedWriter {
            written: String,
            failed_in_nested: bool,
        }

        impl std::fmt::Write for FailingNestedWriter {
            fn write_str(&mut self, text: &str) -> std::fmt::Result {
                if self.written == "(State " && text == "(State " {
                    self.failed_in_nested = true;
                    return Err(std::fmt::Error);
                }
                self.written.push_str(text);
                Ok(())
            }
        }

        let inner = StateAtom::new(sym!("leaf"), sym!("Type"));
        let outer = StateAtom::new(Atom::gnd(inner), sym!("Type"));
        let mut writer = FailingNestedWriter{ written: String::new(), failed_in_nested: false };
        assert!(std::fmt::write(&mut writer, format_args!("{outer}")).is_err());
        assert!(writer.failed_in_nested);
        assert_eq!(outer.to_string(), "(State (State leaf))");
    }

    #[test]
    fn state_display_guard_survives_unwinding() {
        #[derive(Clone, PartialEq, Debug)]
        struct PanicOnDisplay;

        impl Display for PanicOnDisplay {
            fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                panic!("intentional formatter panic")
            }
        }

        impl Grounded for PanicOnDisplay {
            fn type_(&self) -> Atom {
                ATOM_TYPE_UNDEFINED
            }
        }

        let state = StateAtom::new(Atom::gnd(PanicOnDisplay), sym!("Type"));
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| state.to_string()));
        assert!(result.is_err());
        state.state.borrow_mut().0 = sym!("leaf");
        assert_eq!(state.to_string(), "(State leaf)");
    }

    #[test]
    fn cyclic_state_ops_can_observe_and_break_cycle() {
        let state = StateAtom::new(sym!("leaf"), sym!("Type"));
        state.state.borrow_mut().0 = Atom::expr([sym!("wrap"), Atom::gnd(state.clone())]);
        let handle = Atom::gnd(state.clone());

        let observed = GetStateOp{}.execute(&[handle.clone()]).unwrap();
        assert!(observed.len() == 1);
        let changed = ChangeStateOp{}.execute(&[handle.clone(), sym!("leaf")]).unwrap();
        assert!(changed.len() == 1);
        assert_eq!(state.to_string(), "(State leaf)");

        let observed = GetStateOp{}.execute(&[handle]).unwrap();
        assert_eq!(observed[0], sym!("leaf"));
    }
}
