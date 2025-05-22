use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::common::CachingMapper;
use crate::metta::runner::Metta;
use crate::metta::runner::PragmaSettings;
use crate::metta::runner::bool::*;
use crate::atom::gnd::GroundedFunctionAtom;
use crate::matcher::{Bindings, apply_bindings_to_atom_move};

use std::convert::TryInto;

use super::{interpret_no_error, grounded_op, unit_result, regex, interpret};

#[derive(Clone, Debug)]
pub struct PragmaOp {
    settings: PragmaSettings,
}

grounded_op!(PragmaOp, "pragma!");

impl PragmaOp {
    pub fn new(settings: PragmaSettings) -> Self {
        Self{ settings }
    }
}

impl Grounded for PragmaOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PragmaOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pragma! expects key and value as arguments");
        let key = <&SymbolAtom>::try_from(args.get(0).ok_or_else(arg_error)?).map_err(|_| "pragma! expects symbol atom as a key")?.name();
        let value = args.get(1).ok_or_else(arg_error)?;
        match key {
            "max-stack-depth" => {
                value.to_string().parse::<usize>().map_err(|_| "UnsignedIntegerIsExpected")?;
            },
            _ => {},
        }
        self.settings.set(key.into(), value.clone());
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct NopOp {}

grounded_op!(NopOp, "nop");

impl Grounded for NopOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NopOp {
    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct SealedOp {}

grounded_op!(SealedOp, "sealed");

impl Grounded for SealedOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SealedOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("sealed expects two arguments: var_list and expression");

        let mut term_to_seal = args.get(1).ok_or_else(arg_error)?.clone();
        let var_list = args.get(0).ok_or_else(arg_error)?.clone();

        let mut local_var_mapper = CachingMapper::new(|var: &VariableAtom| var.clone().make_unique());

        var_list.iter().filter_type::<&VariableAtom>()
            .for_each(|var| { let _ = local_var_mapper.replace(var); });

        term_to_seal.iter_mut().filter_type::<&mut VariableAtom>()
            .for_each(|var| match local_var_mapper.mapping().get(var) {
                Some(v) => *var = v.clone(),
                None => {},
            });

        let result = vec![term_to_seal.clone()];
        log::debug!("sealed::execute: var_list: {}, term_to_seal: {}, result: {:?}", var_list, term_to_seal, result);

        Ok(result)
    }
}

#[derive(Clone, Debug)]
pub struct EqualOp {}

grounded_op!(EqualOp, "==");

impl Grounded for EqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!(t), expr!(t), ATOM_TYPE_BOOL])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for EqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from(concat!(stringify!($op), " expects two arguments"));
        let a = args.get(0).ok_or_else(arg_error)?;
        let b = args.get(1).ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(a == b))])
    }
}

#[derive(Clone, Debug)]
pub struct MatchOp {}

grounded_op!(MatchOp, "match");

impl Grounded for MatchOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SPACE, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for MatchOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("match expects three arguments: space, pattern and template");
        let space = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;
        log::debug!("MatchOp::execute: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("match expects a space as the first argument")?;
        Ok(space.borrow().subst(&pattern, &template))
    }
}

#[derive(Clone, Debug)]
pub struct IfEqualOp { }

grounded_op!(IfEqualOp, "if-equal");

impl Grounded for IfEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IfEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("if-equal expects <atom> <pattern> <then> <else> as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let then = args.get(2).ok_or_else(arg_error)?;
        let else_ = args.get(3).ok_or_else(arg_error)?;

        if crate::matcher::atoms_are_equivalent(atom, pattern) {
            Ok(vec![then.clone()])
        } else {
            Ok(vec![else_.clone()])
        }
    }
}

#[derive(Clone, Debug)]
pub struct SuperposeOp {
    space: DynSpace,
    settings: PragmaSettings,
}

grounded_op!(SuperposeOp, "superpose");

impl SuperposeOp {
    fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for SuperposeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SuperposeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("superpose expects single expression as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let expr  = TryInto::<&ExpressionAtom>::try_into(atom).map_err(|_| arg_error())?;

        if expr.children().is_empty() {
            Ok(vec![EMPTY_SYMBOL])
        } else {
            let mut superposed = Vec::new();
            for atom in expr.children() {
                match interpret_no_error(self.space.clone(), atom, self.settings.clone()) {
                    Ok(results) => { superposed.extend(results); },
                    Err(message) => { return Err(format!("Error: {}", message).into()) },
                }
            }
            Ok(superposed)
        }
    }
}

#[derive(Clone, Debug)]
pub struct CaptureOp {
    space: DynSpace,
    settings: PragmaSettings, 
}

grounded_op!(CaptureOp, "capture");

impl CaptureOp {
    pub fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for CaptureOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CaptureOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("capture expects one argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        interpret(self.space.clone(), &atom, self.settings.clone()).map_err(|e| ExecError::from(e))
    }
}

#[derive(Clone, Debug)]
pub struct CaseOp {
    space: DynSpace,
    settings: PragmaSettings,
}

grounded_op!(CaseOp, "case");

impl CaseOp {
    pub fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for CaseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CaseOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("case expects two arguments: atom and expression of cases");
        let cases = args.get(1).ok_or_else(arg_error)?;
        let atom = args.get(0).ok_or_else(arg_error)?;
        log::debug!("CaseOp::execute: atom: {}, cases: {:?}", atom, cases);

        let switch = |interpreted: Atom| -> Atom {
            Atom::expr([sym!("switch"), interpreted, cases.clone()])
        };

        // Interpreting argument inside CaseOp is required because otherwise `Empty` result
        // calculated inside interpreter cuts the whole branch of the interpretation. Also we
        // cannot use `unify` in a unit test because after interpreting `(chain... (chain
        // (metta (unify ...) Atom <space>)) ...)` `chain` executes `unify` and also gets
        // `Empty` even if we have `Atom` as a resulting type. It can be solved by different ways.
        // One way is to invent new type `EmptyType` (type of the `Empty` atom) and use this type
        // in a function to allow `Empty` atoms as an input. `EmptyType` type should not be
        // casted to the `%Undefined%` thus one cannot pass `Empty` to the function which accepts
        // `%Undefined%`. Another way is to introduce "call" level. Thus if function called
        // returned the result to the `chain` it should stop reducing it and insert it into the
        // last argument.
        let results = interpret(self.space.clone(), atom, self.settings.clone());
        log::debug!("CaseOp::execute: atom results: {:?}", results);
        let results = match results {
            Ok(results) if results.is_empty() =>
                vec![switch(EMPTY_SYMBOL)],
            Ok(results) =>
                results.into_iter().map(|atom| switch(atom)).collect(),
            Err(err) => vec![Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::sym(err)])],
        };
        Ok(results)
    }
}

fn collapse_add_next_atom_from_collapse_bind_result(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg0_error = || ExecError::from("Expression is expected as a first argument");
    let list = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg0_error)?).map_err(|_| arg0_error())?;
    let arg1_error = || ExecError::from("(Atom Bindings) pair is expected as a second argument");
    let atom_bindings = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg1_error)?).map_err(|_| arg1_error())?;
    let atom = atom_bindings.children().get(0).ok_or_else(arg1_error)?;
    let bindings = atom_bindings.children().get(1).and_then(|a| a.as_gnd::<Bindings>()).ok_or_else(arg1_error)?;

    let atom = apply_bindings_to_atom_move(atom.clone(), bindings);
    let mut list = list.clone().into_children();
    list.push(atom);
    Ok(vec![Atom::expr(list)])
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let is_equivalent = Atom::gnd(IfEqualOp{});
    tref.register_token(regex(r"if-equal"), move |_| { is_equivalent.clone() });
    let nop_op = Atom::gnd(NopOp{});
    tref.register_token(regex(r"nop"), move |_| { nop_op.clone() });
    let match_op = Atom::gnd(MatchOp{});
    tref.register_token(regex(r"match"), move |_| { match_op.clone() });
    let sealed_op = Atom::gnd(SealedOp{});
    tref.register_token(regex(r"sealed"), move |_| { sealed_op.clone() });
    let eq_op = Atom::gnd(EqualOp{});
    tref.register_token(regex(r"=="), move |_| { eq_op.clone() });
    tref.register_function(GroundedFunctionAtom::new(
            r"_collapse-add-next-atom-from-collapse-bind-result".into(),
            expr!("->" "Expression" "Expression" "Atom"),
            collapse_add_next_atom_from_collapse_bind_result, 
            ));
}

pub(super) fn register_context_dependent_tokens(tref: &mut Tokenizer, space: &DynSpace, metta: &Metta) {
    let superpose_op = Atom::gnd(SuperposeOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"superpose"), move |_| { superpose_op.clone() });
    let case_op = Atom::gnd(CaseOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"case"), move |_| { case_op.clone() });
    let capture_op = Atom::gnd(CaptureOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"capture"), move |_| { capture_op.clone() });
    let pragma_op = Atom::gnd(PragmaOp::new(metta.settings().clone()));
    tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::run_program;
    use crate::matcher::atoms_are_equivalent;
    use crate::common::test_utils::metta_space;
    use crate::metta::runner::number::Number;

    use std::convert::TryFrom;

    #[test]
    fn metta_superpose() {
        assert_eq_metta_results!(run_program("!(superpose (red yellow green))"),
            Ok(vec![vec![expr!("red"), expr!("yellow"), expr!("green")]]));
        let program = "
            (= (foo) FOO)
            (= (bar) BAR)
            !(superpose ((foo) (bar) BAZ))
        ";
        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("FOO"), expr!("BAR"), expr!("BAZ")]]));
    }

    #[test]
    fn metta_collapse() {
        let program = "
            (= (color) red)
            (= (color) green)
            (= (color) blue)
            !(collapse (color))
        ";
        let result = run_program(program).expect("Successful result is expected");
        assert_eq!(result.len(), 1);
        let result = result.get(0).unwrap();
        assert_eq!(result.len(), 1);
        let result = result.get(0).unwrap();
        let actual = <&ExpressionAtom>::try_from(result)
            .expect("Expression atom is expected").children();
        assert_eq_no_order!(actual, vec![expr!("red"), expr!("green"), expr!("blue")]);
    }

    #[test]
    fn metta_case_empty() {
        let result = run_program("!(case Empty ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
        let result = run_program("!(case (unify (C B) (C B) ok Empty) ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("ok")]]));
        let result = run_program("!(case (unify (B C) (C B) ok nok) ( (ok ok) (nok nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
        let result = run_program("!(case (unify (B C) (C B) ok Empty) ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
    }

    #[test]
    fn test_pragma_interpreter_bare_minimal() {
        let program = "
            (= (bar) baz)
            (= (foo) (bar))
            !(foo)
            !(pragma! interpreter bare-minimal)
            !(foo)
            !(eval (foo))
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![
                vec![expr!("baz")],
                vec![UNIT_ATOM],
                vec![expr!(("foo"))],
                vec![expr!(("bar"))],
            ]));
    }

    #[test]
    fn test_pragma_max_stack_depth() {
        let program = "!(assertEqual (pragma! max-stack-depth -12) (Error (pragma! max-stack-depth -12) UnsignedIntegerIsExpected))";
        assert_eq_metta_results!(run_program(program), Ok(vec![ vec![expr!()] ]));

        let program = "
            !(pragma! max-stack-depth 21)
            !(pragma! max-stack-depth 0)
            (= (fac $n) (if (== $n 0) 1 (* $n (fac (- $n 1)))))
            !(fac 6)
        ";
        assert_eq_metta_results!(run_program(program),
            Ok(vec![
                vec![UNIT_ATOM],
                vec![UNIT_ATOM],
                vec![expr!({Number::Integer(720)})],
            ]));

        let program = "
            (= (fac $n) (if (== $n 0) 1 (* $n (fac (- $n 1)))))
            !(pragma! max-stack-depth 200)
            !(fac 3)
            !(case (fac 6) (
               ((Error $a StackOverflow) ())
               ($_ (Error (fac 6) \"StackOverflow error is expected\")) ))
        ";
        assert_eq_metta_results!(run_program(program),
            Ok(vec![
                vec![UNIT_ATOM],
                vec![Atom::gnd(Number::Integer(6))],
                vec![UNIT_ATOM],
            ]));
    }

    #[test]
    fn use_sealed_to_make_scoped_variable() {
        assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![]]));
        assert_eq!(run_program("!(let $x (input $y) (output $x))"), Ok(vec![vec![expr!("output" ("input" y))]]));
        assert_eq!(run_program("!(let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
    }

    #[test]
    fn let_op_variables_visibility_pr262() {
        let program = "
            ;; Knowledge
            (→ P Q)
            (→ Q R)

            ;; Rule
            (= (rule (→ $p $q) (→ $q $r)) (→ $p $r))

            ;; Query (does not work as expected)
            (= (query $kb)
               (let* (($pq (→ $p $q))
                      ($qr (→ $q $r)))
                 (match $kb
                   ;; Premises
                   (, $pq $qr)
                   ;; Conclusion
                   (rule $pq $qr))))

            ;; Call
            !(query &self)
            ;; [(→ P R)]
        ";
        assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("→" "P" "R")]]));
    }

    #[test]
    fn sealed_op_runner() {
        let nested = run_program("!(sealed ($a $b $x) (quote (= ($a $x $c) ($b))))");
        let simple_replace = run_program("!(sealed ($x $y) (quote (= ($y $z))))");

        assert!(crate::atom::matcher::atoms_are_equivalent(&nested.unwrap()[0][0], &expr!("quote" ("=" (a b c) (z)))));
        assert!(crate::atom::matcher::atoms_are_equivalent(&simple_replace.unwrap()[0][0], &expr!("quote" ("=" (y z)))));
    }

    #[test]
    fn match_op() {
        let space = metta_space("(A B)");
        let match_op = MatchOp{};
        assert_eq!(match_op.execute(&mut vec![expr!({space}), expr!("A" "B"), expr!("B" "A")]),
                   Ok(vec![expr!("B" "A")]));
    }

    #[test]
    fn match_op_issue_530() {
        let space = metta_space("(A $a $a)");
        let match_op = MatchOp{};
        let result = match_op.execute(&mut vec![expr!({space}), expr!("A" x y), expr!("A" x y)]).unwrap();
        assert_eq!(result.len(), 1);
        assert!(atoms_are_equivalent(&result[0], &expr!("A" x x)),
                "atoms are not equivalent: expected: {}, actual: {}", expr!("A" x x), result[0]);
    }

    #[test]
    fn nop_op() {
        assert_eq!(NopOp{}.execute(&mut vec![]), unit_result());
    }

    #[test]
    fn sealed_op_execute() {
        let val = SealedOp{}.execute(&mut vec![expr!(x y), expr!("="(y z))]);
        assert!(crate::atom::matcher::atoms_are_equivalent(&val.unwrap()[0], &expr!("="(y z))));
    }
}
