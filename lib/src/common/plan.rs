use std::fmt::{Debug, Formatter};

// Generic plan infrastructure

/// Result of a single step of a plan
pub enum StepResult<'a, R: 'a> {
    /// New plan to be executed to get a result
    Execute(Box<dyn Plan<'a, (), R> + 'a>),
    /// Result returned
    Return(R),
    /// Plan execution error message
    Error(String),
}

impl<'a, R: 'a> StepResult<'a, R> {
    /// New result from a value which has the Plan trait
    pub fn execute<P>(next: P) -> Self where P: 'a + Plan<'a, (), R> {
        Self::Execute(Box::new(next))
    }

    /// New result from a returned value
    pub fn ret(result: R) -> Self {
        Self::Return(result)
    }

    /// New error result
    pub fn err<M: Into<String>>(message: M) -> Self {
        Self::Error(message.into())
    }

    /// Return true if plan can be executed further
    pub fn has_next(&self) -> bool {
        match self {
            StepResult::Execute(_) => true,
            StepResult::Return(_) => false,
            StepResult::Error(_) => false,
        }
    }

    /// Get result of the execution
    pub fn get_result(self) -> Result<R, String> {
        match self {
            StepResult::Execute(_) => panic!("Plan is not finished yet"),
            StepResult::Return(result) => Ok(result),
            StepResult::Error(message) => Err(message),
        }
    }
}

/// Plan which gets a value of T type as an input and returns a result of
/// R type as an output after execution
pub trait Plan<'a, T, R: 'a> : Debug {
    // `self: Box<Self>` allows moving content of the step into the next step.
    // We cannot use `self: Self` because it will be called via `dyn Plan`
    // which doesn't know anything about original type and cannot move it.
    /// Execute one step of the plan
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, R>;
}

// Specific plans to form calculations graph

/// Boxed plan is a plan
impl<'a, T, R: 'a> Plan<'a, T, R> for Box<dyn Plan<'a, T, R> + '_> {
    fn step(self: Box<Self>, arg:T) -> StepResult<'a, R> {
        (*self).step(arg)
    }
}

/// StepResult itself is a trivial plan which executes step of the plan or 
/// itself when executed
impl<'a, R: 'a + Debug> Plan<'a, (), R> for StepResult<'a, R> {
    fn step(self: Box<Self>, _:()) -> StepResult<'a, R> {
        match *self {
            StepResult::Execute(plan) => plan.step(()),
            _ => *self,
        }
    }
}

impl<R: Debug> Debug for StepResult<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Execute(plan) => write!(f, "{:?}", plan),
            Self::Return(result) => write!(f, "return {:?}", result),
            Self::Error(message) => write!(f, "error {:?}", message),
        }
    }
}

/// Function from T to StepResult<R> is a plan which calls itself when executed
pub struct FunctionPlan<'a, T, R: 'a> {
    pub func: fn(T) -> StepResult<'a, R>,
    pub name: &'a str,
}

impl<'a, T, R: 'a> Plan<'a, T, R> for FunctionPlan<'a, T, R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, R> {
        (self.func)(arg)
    }
}

impl<T, R> Debug for FunctionPlan<'_, T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl<T, R> Clone for FunctionPlan<'_, T, R> {
    fn clone(&self) -> Self {
        Self{ func: self.func, name: self.name }
    }
}

impl<T, R> Copy for FunctionPlan<'_, T, R> {}

/// Operator from T to StepResult<R> is a plan which calls itself when executed
pub struct OperatorPlan<'a, T, R: 'a> {
    operator: Box<dyn 'a + FnOnce(T) -> StepResult<'a, R>>,
    name: String,
}

impl<'a, T, R: 'a> OperatorPlan<'a, T, R> {
    pub fn new<F: 'a + FnOnce(T) -> StepResult<'a, R>, N: Into<String>>(operator: F, name: N) -> Self {
        Self { operator: Box::new(operator), name: name.into() }
    }
}

impl<'a, T, R: 'a> Plan<'a, T, R> for OperatorPlan<'a, T, R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, R> {
        (self.operator)(arg)
    }
}

impl<T, R> Debug for OperatorPlan<'_, T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

/// The plan applies an argument of T type to the underlying plan Plan<T, R>.
/// Resulting plan has type Plan<(), R>.
pub struct ApplyPlan<'a, T: 'a, R: 'a> {
    arg: T,
    plan: Box<dyn Plan<'a, T, R> + 'a>,
}

impl<'a, T: 'a, R: 'a> ApplyPlan<'a, T, R> {
    pub fn new<P>(plan: P, arg: T) -> Self where P: 'a + Plan<'a, T, R> {
        ApplyPlan{ arg, plan: Box::new(plan) }
    }
}

impl<'a, T: 'a + Debug, R: 'a> Plan<'a, (), R> for ApplyPlan<'a, T, R> {
    fn step(self: Box<Self>, _: ()) -> StepResult<'a, R> {
        Plan::step(self.plan, self.arg)
    }
}

impl<T: Debug, R> Debug for ApplyPlan<'_, T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "apply \"{:?}\" to \"{:?}\"", self.plan, self.arg)
    }
}

/// The plan applies value of T1 type as a first argument to the underlying
/// plan which consumes pair of (T1, T2). Resulting plan has type Plan<T2, R>
/// and to be applied to the second argument.
pub struct PartialApplyPlan<'a, T1: 'a, T2, R: 'a> {
    arg: T1,
    plan: Box<dyn Plan<'a, (T1, T2), R> + 'a>,
}

impl<'a, T1: 'a, T2, R: 'a> PartialApplyPlan<'a, T1, T2, R> {
    pub fn new<P>(plan: P, arg: T1) -> Self where P: 'a + Plan<'a, (T1, T2), R> {
        PartialApplyPlan{ arg, plan: Box::new(plan) }
    }
}

impl<'a, T1: 'a + Debug, T2, R: 'a> Plan<'a, T2, R> for PartialApplyPlan<'a, T1, T2, R> {
    fn step(self: Box<Self>, arg: T2) -> StepResult<'a, R> {
        self.plan.step((self.arg, arg))
    }
}

impl<T1: Debug, T2, R> Debug for PartialApplyPlan<'_, T1, T2, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "partially apply \"{:?}\" to \"{:?}\"", self.plan, self.arg)
    }
}

/// The plan concatenates two underlying plans via middle value of T2 type.
pub struct SequencePlan<'a, T1, T2: 'a, R: 'a> {
    first: Box<dyn Plan<'a, T1, T2> + 'a>,
    second: Box<dyn Plan<'a, T2, R> + 'a>,
}

impl<'a, T1, T2: 'a, R: 'a> SequencePlan<'a, T1, T2, R> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'a + Plan<'a, T1, T2>,
              P2: 'a + Plan<'a, T2, R> {
        SequencePlan{ first: Box::new(first), second: Box::new(second) }
    }
}

impl<'a, T1, T2: 'a + Debug, R: 'a> Plan<'a, T1, R> for SequencePlan<'a, T1, T2, R> {
    fn step(self: Box<Self>, arg: T1) -> StepResult<'a, R> {
        match self.first.step(arg) {
            StepResult::Execute(next) => StepResult::execute(SequencePlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(result) => StepResult::execute(ApplyPlan{
                arg: result,
                plan: self.second,
            }),
            StepResult::Error(message) => StepResult::err(message),
        }
    }
}

impl<T1, T2, R> Debug for SequencePlan<'_, T1, T2, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} then {:?}", self.first, self.second)
    }
}

/// The plan to execute two underlying plans and return a pair formed from
/// their results.
pub struct ParallelPlan<'a, T1: 'a, T2: 'a> {
    first: Box<dyn Plan<'a, (), T1> + 'a>,
    second: Box<dyn Plan<'a, (), T2> + 'a>,
}

impl<'a, T1: 'a, T2: 'a> ParallelPlan<'a, T1, T2> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'a + Plan<'a, (), T1>,
              P2: 'a + Plan<'a, (), T2> {
        Self{
            first: Box::new(first),
            second: Box::new(second)
        }
    }
}

/// Return error if any of sub-plans returned error
impl<'a, T1: 'a + Debug, T2: 'a + Debug> Plan<'a, (), (T1, T2)> for ParallelPlan<'a, T1, T2> {
    fn step(self: Box<Self>, _: ()) -> StepResult<'a, (T1, T2)> {
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
            StepResult::Error(message) => StepResult::Error(message),
        }
    }
}

impl<T1, T2> Debug for ParallelPlan<'_, T1, T2> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}\n{:?}", self.first, self.second)
    }
}

/// Trait to fold the value (typically sequence of sub-values) to the plan
/// which processes each sub-value in parallel and merges the results.
pub trait FoldIntoParallelPlan<'a, I, T, R>
    where I: 'a + Iterator,
          T: 'a,
          R: 'a {
    /// Method converts the `self` value into parallel plan. It starts from
    /// the `empty` return value. It applies `step` to each sub-value to get
    /// a plan to calculate result. It applies `merge` to plan result and
    /// step result to calculate the final result. Resulting plan is returned.
    fn into_parallel_plan<S, M>(self, empty: R, step: S, merge: M) -> Box<dyn Plan<'a, (), R> + 'a>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<'a, (), T> + 'a>,
          M: 'a + FnMut(R, T) -> R + Clone;
}

impl<'a, I: 'a, T: 'a, R> FoldIntoParallelPlan<'a, I, T, R> for I
    where I: Iterator,
          T: 'a + Debug,
          R: 'a + Debug {
    fn into_parallel_plan<S, M>(self, empty: R, mut step: S, merge: M) -> Box<dyn Plan<'a, (), R> + 'a>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<'a, (), T> + 'a>,
          M: 'a + FnMut(R, T) -> R + Clone {
        let plan: Box<dyn Plan<'a, (), R> + 'a> = self
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
pub struct NoErrorPlan<'a, T, R> {
    delegate: Box<dyn Plan<'a, T, R> + 'a>,
}

impl<'a, T, R> NoErrorPlan<'a, T, R> {
    pub fn new<P>(delegate: P) -> Self
        where P: 'a + Plan<'a, T, R> {
        Self{ delegate: Box::new(delegate) }
    }
}

impl<'a, T, R: 'a> Plan<'a, T, Option<R>> for NoErrorPlan<'a, T, R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<'a, Option<R>> {
        match self.delegate.step(arg) {
            StepResult::Execute(next) => StepResult::execute(
                NoErrorPlan{ delegate: next }),
            StepResult::Return(result) => StepResult::Return(Some(result)),
            StepResult::Error(_) => StepResult::Return(None),
        }
    }
}

impl<T, R> Debug for NoErrorPlan<'_, T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.delegate)
    }
}

/// Plan returns the first plan which returned non-error result
pub struct OrPlan<'a, R> {
    first: Box<dyn Plan<'a, (), R> + 'a>,
    second: Box<dyn Plan<'a, (), R> + 'a>,
}

impl<'a, R> OrPlan<'a, R> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'a + Plan<'a, (), R>,
              P2: 'a + Plan<'a, (), R> {
        Self{
            first: Box::new(first),
            second: Box::new(second)
        }
    }
}

impl<'a, R: 'a> Plan<'a, (), R> for OrPlan<'a, R> {
    fn step(self: Box<Self>, _: ()) -> StepResult<'a, R> {
        match self.first.step(()) {
            StepResult::Execute(next) => StepResult::execute(OrPlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(first_result) => StepResult::ret(first_result),
            StepResult::Error(err) => {
                log::debug!("OrPlan: returned second path: {:?} because of error: {}", self.second, err);
                StepResult::execute(self.second)
            },
        }
    }
}

impl<R> Debug for OrPlan<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} (or {:?})", self.first, self.second)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Execute the plan using given input value and return result
    fn execute_plan<'a, T: Debug + 'a, R: 'a, P>(plan: P, arg: T) -> Result<R, String> where P: 'a + Plan<'a, T, R> {
        let mut step: Box<dyn Plan<'_, (), R>>  = Box::new(ApplyPlan::new(plan, arg));
        loop {
            log::debug!("current plan:\n{:?}", step);
            match step.step(()) {
                StepResult::Execute(next) => step = next,
                StepResult::Return(result) => return Ok(result),
                StepResult::Error(message) => return Err(message),
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
                    |_| StepResult::ret("Successful"), "return string")));

        let result = plan.step(());

        if let StepResult::Return(result) = result {
            assert_eq!(result, "Successful");
        } else {
            assert!(false, "Immediate result is expected");
        }

    }
}

