use std::fmt::{Debug, Formatter};

// Generic plan infrastructure

/// Result of a single step of a plan
pub enum StepResult<'a, R: 'a, E: 'a> {
    /// New plan to be executed to get a result
    Execute(Box<dyn Plan<'a, (), R, E> + 'a>),
    /// Result returned
    Return(R),
    /// Plan execution error
    Error(E),
}

impl<'a, R: 'a, E: 'a> StepResult<'a, R, E> {
    /// New result from a value which has the Plan trait
    pub fn execute<P>(next: P) -> Self where P: 'a + Plan<'a, (), R, E> {
        Self::Execute(Box::new(next))
    }

    /// New result from a returned value
    pub fn ret(result: R) -> Self {
        Self::Return(result)
    }

    /// New error result
    pub fn err(err: E) -> Self {
        Self::Error(err)
    }

    /// Return true if plan can be executed further
    pub fn has_next(&self) -> bool {
        match self {
            StepResult::Execute(_) => true,
            StepResult::Return(_) => false,
            StepResult::Error(_) => false,
        }
    }
}

/// Plan which gets a value of T type as an input and returns a result of
/// R type as an output after execution
pub trait Plan<'a, T, R: 'a, E: 'a> : Debug {
    // `self: Box<Self>` allows moving content of the step into the next step.
    // We cannot use `self: Self` because it will be called via `dyn Plan`
    // which doesn't know anything about original type and cannot move it.
    /// Execute one step of the plan
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, R, E>;
}

// Specific plans to form calculations graph

/// Boxed plan is a plan
impl<'a, T, R: 'a, E: 'a> Plan<'a, T, R, E> for Box<dyn Plan<'a, T, R, E> + '_> {
    fn step(self: Box<Self>, arg:T) -> StepResult<'a, R, E> {
        (*self).step(arg)
    }
}

/// StepResult itself is a trivial plan which executes step of the plan or 
/// itself when executed
impl<'a, R: 'a + Debug, E: 'a + Debug> Plan<'a, (), R, E> for StepResult<'a, R, E> {
    fn step(self: Box<Self>, _:()) -> StepResult<'a, R, E> {
        match *self {
            StepResult::Execute(plan) => plan.step(()),
            _ => *self,
        }
    }
}

impl<R: Debug, E: Debug> Debug for StepResult<'_, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Execute(plan) => write!(f, "{:?}", plan),
            Self::Return(result) => write!(f, "return {:?}", result),
            Self::Error(err) => write!(f, "error {:?}", err),
        }
    }
}

/// Function from T to StepResult<R, E> is a plan which calls itself when executed
pub struct FunctionPlan<'a, T, R: 'a, E: 'a> {
    pub func: fn(T) -> StepResult<'a, R, E>,
    pub name: &'a str,
}

impl<'a, T, R: 'a, E: 'a> Plan<'a, T, R, E> for FunctionPlan<'a, T, R, E> {
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, R, E> {
        (self.func)(arg)
    }
}

impl<T, R, E> Debug for FunctionPlan<'_, T, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl<T, R, E> Clone for FunctionPlan<'_, T, R, E> {
    fn clone(&self) -> Self {
        Self{ func: self.func, name: self.name }
    }
}

impl<T, R, E> Copy for FunctionPlan<'_, T, R, E> {}

/// Operator from T to StepResult<R, E> is a plan which calls itself when executed
pub struct OperatorPlan<'a, T, R: 'a, E: 'a> {
    operator: Box<dyn 'a + FnOnce(T) -> StepResult<'a, R, E>>,
    name: String,
}

impl<'a, T, R: 'a, E: 'a> OperatorPlan<'a, T, R, E> {
    pub fn new<F: 'a + FnOnce(T) -> StepResult<'a, R, E>, N: Into<String>>(operator: F, name: N) -> Self {
        Self { operator: Box::new(operator), name: name.into() }
    }
}

impl<'a, T, R: 'a, E: 'a> Plan<'a, T, R, E> for OperatorPlan<'a, T, R, E> {
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, R, E> {
        (self.operator)(arg)
    }
}

impl<T, R, E> Debug for OperatorPlan<'_, T, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

/// The plan applies an argument of T type to the underlying plan Plan<T, R>.
/// Resulting plan has type Plan<(), R>.
pub struct ApplyPlan<'a, T: 'a, R: 'a, E: 'a> {
    arg: T,
    plan: Box<dyn Plan<'a, T, R, E> + 'a>,
}

impl<'a, T: 'a, R: 'a, E: 'a> ApplyPlan<'a, T, R, E> {
    pub fn new<P>(plan: P, arg: T) -> Self where P: 'a + Plan<'a, T, R, E> {
        ApplyPlan{ arg, plan: Box::new(plan) }
    }
}

impl<'a, T: 'a + Debug, R: 'a, E: 'a + Debug> Plan<'a, (), R, E> for ApplyPlan<'a, T, R, E> {
    fn step(self: Box<Self>, _: ()) -> StepResult<'a, R, E> {
        Plan::step(self.plan, self.arg)
    }
}

impl<T: Debug, R, E: Debug> Debug for ApplyPlan<'_, T, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "apply \"{:?}\" to \"{:?}\"", self.plan, self.arg)
    }
}

/// The plan applies value of T1 type as a first argument to the underlying
/// plan which consumes pair of (T1, T2). Resulting plan has type Plan<T2, R>
/// and to be applied to the second argument.
pub struct PartialApplyPlan<'a, T1: 'a, T2, R: 'a, E: 'a> {
    arg: T1,
    plan: Box<dyn Plan<'a, (T1, T2), R, E> + 'a>,
}

impl<'a, T1: 'a, T2, R: 'a, E: 'a> PartialApplyPlan<'a, T1, T2, R, E> {
    pub fn new<P>(plan: P, arg: T1) -> Self where P: 'a + Plan<'a, (T1, T2), R, E> {
        PartialApplyPlan{ arg, plan: Box::new(plan) }
    }
}

impl<'a, T1: 'a + Debug, T2, R: 'a, E: 'a> Plan<'a, T2, R, E> for PartialApplyPlan<'a, T1, T2, R, E> {
    fn step(self: Box<Self>, arg: T2) -> StepResult<'a, R, E> {
        self.plan.step((self.arg, arg))
    }
}

impl<T1: Debug, T2, R, E> Debug for PartialApplyPlan<'_, T1, T2, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "partially apply \"{:?}\" to \"{:?}\"", self.plan, self.arg)
    }
}

/// The plan concatenates two underlying plans via middle value of T2 type.
pub struct SequencePlan<'a, T1, T2: 'a, R: 'a, E: 'a> {
    first: Box<dyn Plan<'a, T1, T2, E> + 'a>,
    second: Box<dyn Plan<'a, T2, R, E> + 'a>,
}

impl<'a, T1, T2: 'a, R: 'a, E: 'a> SequencePlan<'a, T1, T2, R, E> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'a + Plan<'a, T1, T2, E>,
              P2: 'a + Plan<'a, T2, R, E> {
        SequencePlan{ first: Box::new(first), second: Box::new(second) }
    }
}

impl<'a, T1, T2: 'a + Debug, R: 'a, E: 'a + Debug> Plan<'a, T1, R, E> for SequencePlan<'a, T1, T2, R, E> {
    fn step(self: Box<Self>, arg: T1) -> StepResult<'a, R, E> {
        match self.first.step(arg) {
            StepResult::Execute(next) => StepResult::execute(SequencePlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(result) => StepResult::execute(ApplyPlan{
                arg: result,
                plan: self.second,
            }),
            StepResult::Error(error) => StepResult::err(error),
        }
    }
}

impl<T1, T2, R, E> Debug for SequencePlan<'_, T1, T2, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} then {:?}", self.first, self.second)
    }
}

/// The plan to execute two underlying plans and return a pair formed from
/// their results.
pub struct ParallelPlan<'a, T1: 'a, T2: 'a, E: 'a> {
    first: Box<dyn Plan<'a, (), T1, E> + 'a>,
    second: Box<dyn Plan<'a, (), T2, E> + 'a>,
}

impl<'a, T1: 'a, T2: 'a, E: 'a> ParallelPlan<'a, T1, T2, E> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'a + Plan<'a, (), T1, E>,
              P2: 'a + Plan<'a, (), T2, E> {
        Self{
            first: Box::new(first),
            second: Box::new(second)
        }
    }
}

/// Return error if any of sub-plans returned error
impl<'a, T1: 'a + Debug, T2: 'a + Debug, E: 'a + Debug> Plan<'a, (), (T1, T2), E> for ParallelPlan<'a, T1, T2, E> {
    fn step(self: Box<Self>, _: ()) -> StepResult<'a, (T1, T2), E> {
        match self.first.step(()) {
            StepResult::Execute(next) => StepResult::execute(ParallelPlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(first_result) => {
                let descr = format!("return tuple ({:?}, ?)", first_result);
                StepResult::execute(SequencePlan{
                    first: self.second,
                    second: Box::new(OperatorPlan::new(|second_result|
                        StepResult::ret((first_result, second_result)),
                        descr))
                })
            },
            StepResult::Error(err) => StepResult::Error(err),
        }
    }
}

impl<T1, T2, E> Debug for ParallelPlan<'_, T1, T2, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}\n{:?}", self.first, self.second)
    }
}

/// Trait to fold the value (typically sequence of sub-values) to the plan
/// which processes each sub-value in parallel and merges the results.
pub trait FoldIntoParallelPlan<'a, I, T, R, E>
    where I: 'a + Iterator,
          T: 'a, R: 'a, E: 'a {
    /// Method converts the `self` value into parallel plan. It starts from
    /// the `empty` return value. It applies `step` to each sub-value to get
    /// a plan to calculate result. It applies `merge` to plan result and
    /// step result to calculate the final result. Resulting plan is returned.
    fn into_parallel_plan<S, M>(self, empty: R, step: S, merge: M) -> Box<dyn Plan<'a, (), R, E> + 'a>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<'a, (), T, E> + 'a>,
          M: 'a + FnMut(R, T) -> R + Clone;
}

impl<'a, I: 'a, T: 'a, R, E> FoldIntoParallelPlan<'a, I, T, R, E> for I
    where I: Iterator,
          T: 'a + Debug,
          R: 'a + Debug,
          E: 'a + Debug {
    fn into_parallel_plan<S, M>(self, empty: R, mut step: S, merge: M) -> Box<dyn Plan<'a, (), R, E> + 'a>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<'a, (), T, E> + 'a>,
          M: 'a + FnMut(R, T) -> R + Clone {
        let plan: Box<dyn Plan<'a, (), R, E> + 'a> = self
            .fold(Box::new(StepResult::ret(empty)),
                |plan, step_result| {
                    let mut merge = merge.clone();
                    Box::new(SequencePlan::new(
                        ParallelPlan {
                            first: plan,
                            second: step(step_result),
                        },
                        OperatorPlan::new(move |(plan_res, step_res)|
                            StepResult::ret(merge(plan_res, step_res)),
                            "merge_results"),
                    ))
                }
            );
        plan
    }
}

/// Plan which ignores error and returns optional result
pub struct NoErrorPlan<'a, T, R, E> {
    delegate: Box<dyn Plan<'a, T, R, E> + 'a>,
}

impl<'a, T, R, E> NoErrorPlan<'a, T, R, E> {
    pub fn new<P>(delegate: P) -> Self
        where P: 'a + Plan<'a, T, R, E> {
        Self{ delegate: Box::new(delegate) }
    }
}

impl<'a, T, R: 'a, E: 'a> Plan<'a, T, Option<R>, E> for NoErrorPlan<'a, T, R, E> {
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, Option<R>, E> {
        match self.delegate.step(arg) {
            StepResult::Execute(next) => StepResult::execute(
                NoErrorPlan{ delegate: next }),
            StepResult::Return(result) => StepResult::Return(Some(result)),
            StepResult::Error(_) => StepResult::Return(None),
        }
    }
}

impl<T, R, E> Debug for NoErrorPlan<'_, T, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.delegate)
    }
}

/// Plan returns the first plan which returned non-error result
pub struct OrPlan<'a, R, E> {
    first: Box<dyn Plan<'a, (), R, E> + 'a>,
    second: Box<dyn Plan<'a, (), R, E> + 'a>,
}

impl<'a, R, E> OrPlan<'a, R, E> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'a + Plan<'a, (), R, E>,
              P2: 'a + Plan<'a, (), R, E> {
        Self{
            first: Box::new(first),
            second: Box::new(second)
        }
    }
}

impl<'a, R: 'a, E: 'a + Debug> Plan<'a, (), R, E> for OrPlan<'a, R, E> {
    fn step(self: Box<Self>, _: ()) -> StepResult<'a, R, E> {
        match self.first.step(()) {
            StepResult::Execute(next) => StepResult::execute(OrPlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(first_result) => StepResult::ret(first_result),
            StepResult::Error(err) => {
                log::debug!("OrPlan: returned second path: {:?} because of error: {:?}", self.second, err);
                StepResult::execute(self.second)
            },
        }
    }
}

impl<R, E> Debug for OrPlan<'_, R, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} (or {:?})", self.first, self.second)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Execute the plan using given input value and return result
    fn execute_plan<'a, T: Debug + 'a, R: 'a, P>(plan: P, arg: T) -> Result<R, String> where P: 'a + Plan<'a, T, R, String> {
        let mut step: Box<dyn Plan<'_, (), R, String>>  = Box::new(ApplyPlan::new(plan, arg));
        loop {
            log::debug!("current plan:\n{:?}", step);
            match step.step(()) {
                StepResult::Execute(next) => step = next,
                StepResult::Return(result) => return Ok(result),
                StepResult::Error(error) => return Err(error),
            }
        }
    }

    #[test]
    fn parallel_plan() {
        let mul = SequencePlan::new(
            ParallelPlan::new(
                StepResult::ret(7),
                StepResult::ret(6)),
            OperatorPlan::new(|(a, b)| StepResult::ret(a * b), "*"),
        );
        assert_eq!(execute_plan(mul, ()), Ok(42));
    }

    #[test]
    fn iterator_into_parallel_plan() {
        let step_counter = &mut 0;
        let args = vec!["1", "2", "3", "4"];
        let plan = args.iter().into_parallel_plan(Vec::new(),
            |n| {
                *step_counter += 1;
                Box::new(ApplyPlan::new(OperatorPlan::new(|n: &str| StepResult::ret(n.parse::<u32>().unwrap() + 1), format!("* {}", n)), *n))
            },
            |mut a, b| {a.push(b); a});
        assert_eq!(execute_plan(StepResult::Execute(plan), ()), Ok(vec![2, 3, 4, 5]));
        assert_eq!(*step_counter, 4);
    }

    #[test]
    fn step_result_plan() {
        let plan = Box::new(StepResult::execute(OperatorPlan::new(
                    |_| StepResult::<&str, String>::ret("Successful"), "return string")));

        let result = plan.step(());

        if let StepResult::Return(result) = result {
            assert_eq!(result, "Successful");
        } else {
            assert!(false, "Immediate result is expected");
        }

    }
}

