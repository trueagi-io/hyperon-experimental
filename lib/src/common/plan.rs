use std::fmt::{Debug, Formatter};

// Generic plan infrastructure

/// Result of a single step of a plan
pub enum StepResult<R> {
    /// New plan to be executed to get a result
    Execute(Box<dyn Plan<(), R>>),
    /// Result returned
    Return(R),
    /// Plan execution error message
    Error(String),
}

impl<R> StepResult<R> {
    /// New result from a value which has the Plan trait
    pub fn execute<P>(next: P) -> Self where P: 'static + Plan<(), R> {
        Self::Execute(Box::new(next))
    }

    /// New result from a returned value
    pub fn ret(result: R) -> Self {
        Self::Return(result)
    }

    /// New error result
    pub fn err(message: String) -> Self {
        Self::Error(message)
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
pub trait Plan<T, R> : Debug {
    // `self: Box<Self>` allows moving content of the step into the next step.
    // We cannot use `self: Self` because it will be called via `dyn Plan`
    // which doesn't know anything about original type and cannot move it.
    /// Execute one step of the plan
    fn step(self: Box<Self>, arg: T) -> StepResult<R>;
}

// Specific plans to form calculations graph

/// Boxed plan is a plan
impl<T, R> Plan<T, R> for Box<dyn Plan<T, R>> {
    fn step(self: Box<Self>, arg:T) -> StepResult<R> {
        (*self).step(arg)
    }
}

/// StepResult itself is a trivial plan which returns itself when executed
impl<R: Debug> Plan<(), R> for StepResult<R> {
    fn step(self: Box<Self>, _:()) -> StepResult<R> {
        *self
    }
}

impl<R: Debug> Debug for StepResult<R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Execute(plan) => write!(f, "{:?}", plan),
            Self::Return(result) => write!(f, "{:?}", result),
            Self::Error(message) => write!(f, "{:?}", message),
        }
    }
}

/// Function from T to StepResult<R> is a plan which calls itself when executed
pub struct FunctionPlan<T, R> {
    pub func: fn(T) -> StepResult<R>,
    pub name: &'static str,
}

impl<T, R> Plan<T, R> for FunctionPlan<T, R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<R> {
        (self.func)(arg)
    }
}

impl<T, R> Debug for FunctionPlan<T, R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl<T, R> Clone for FunctionPlan<T, R> {
    fn clone(&self) -> Self {
        Self{ func: self.func, name: self.name }
    }
}

impl<T, R> Copy for FunctionPlan<T, R> {}

/// Operator from T to StepResult<R> is a plan which calls itself when executed
pub struct OperatorPlan<T, R> {
    operator: Box<dyn FnOnce(T) -> StepResult<R>>,
    name: String,
}

impl<T, R> OperatorPlan<T, R> {
    pub fn new<F: 'static + FnOnce(T) -> StepResult<R>, N: Into<String>>(operator: F, name: N) -> Self {
        Self { operator: Box::new(operator), name: name.into() }
    }
}

impl<T, R> Plan<T, R> for OperatorPlan<T, R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<R> {
        (self.operator)(arg)
    }
}

impl<T, R> Debug for OperatorPlan<T, R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

/// The plan applies an argument of T type to the underlying plan Plan<T, R>.
/// Resulting plan has type Plan<(), R>.
pub struct ApplyPlan<T, R> {
    arg: T,
    plan: Box<dyn Plan<T, R>>,
}

impl<T, R> ApplyPlan<T, R> {
    pub fn new<P>(plan: P, arg: T) -> Self where P: 'static + Plan<T, R> {
        ApplyPlan{ arg, plan: Box::new(plan) }
    }
}

impl<T: Debug, R> Plan<(), R> for ApplyPlan<T, R> {
    fn step(self: Box<Self>, _: ()) -> StepResult<R> {
        Plan::step(self.plan, self.arg)
    }
}

impl<T: Debug, R> Debug for ApplyPlan<T, R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}({:?})", self.plan, self.arg)
    }
}

/// The plan applies value of T1 type as a first argument to the underlying
/// plan which consumes pair of (T1, T2). Resulting plan has type Plan<T2, R>
/// and to be applied to the second argument.
pub struct PartialApplyPlan<T1, T2, R> {
    arg: T1,
    plan: Box<dyn Plan<(T1, T2), R>>,
}

impl<T1, T2, R> PartialApplyPlan<T1, T2, R> {
    pub fn new<P>(plan: P, arg: T1) -> Self where P: 'static + Plan<(T1, T2), R> {
        PartialApplyPlan{ arg, plan: Box::new(plan) }
    }
}

impl<T1: Debug, T2, R> Plan<T2, R> for PartialApplyPlan<T1, T2, R> {
    fn step(self: Box<Self>, arg: T2) -> StepResult<R> {
        self.plan.step((self.arg, arg))
    }
}

impl<T1: Debug, T2, R> Debug for PartialApplyPlan<T1, T2, R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}({:?}, ...)", self.plan, self.arg)
    }
}

/// The plan concatenates two underlying plans via middle value of T2 type.
pub struct SequencePlan<T1, T2, R> {
    first: Box<dyn Plan<T1, T2>>,
    second: Box<dyn Plan<T2, R>>,
}

impl<T1, T2, R> SequencePlan<T1, T2, R> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'static + Plan<T1, T2>,
              P2: 'static + Plan<T2, R> {
        SequencePlan{ first: Box::new(first), second: Box::new(second) }
    }
}

impl<T1: 'static, T2: 'static + Debug, R: 'static> Plan<T1, R> for SequencePlan<T1, T2, R> {
    fn step(self: Box<Self>, arg: T1) -> StepResult<R> {
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

impl<T1, T2, R> Debug for SequencePlan<T1, T2, R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} -> {:?}", self.first, self.second)
    }
}

/// The plan to execute two underlying plans and return the pair formed from
/// their their results.
pub struct ParallelPlan<T1, T2> {
    first: Box<dyn Plan<(), T1>>,
    second: Box<dyn Plan<(), T2>>,
}

impl<T1, T2> ParallelPlan<T1, T2> {
    pub fn new<P1, P2>(first: P1, second: P2) -> Self
        where P1: 'static + Plan<(), T1>,
              P2: 'static + Plan<(), T2> {
        ParallelPlan{
            first: Box::new(first),
            second: Box::new(second)
        }
    }
}

/// Return error if any of sub-plans returned error
impl<T1, T2> Plan<(), (T1, T2)> for ParallelPlan<T1, T2> 
    where T1: 'static + Debug,
          T2: 'static + Debug {
    fn step(self: Box<Self>, _: ()) -> StepResult<(T1, T2)> {
        match self.first.step(()) {
            StepResult::Execute(next) => StepResult::execute(ParallelPlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(first_result) => {
                let descr = format!("({:?}, ...)", first_result);
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

impl<T1, T2> Debug for ParallelPlan<T1, T2> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}\n{:?}", self.first, self.second)
    }
}

/// Plan which ignores error and returns optional result
struct NoErrorPlan<T, R> {
    delegate: Box<dyn Plan<T, R>>,
}

impl<T, R: 'static> Plan<T, Option<R>> for NoErrorPlan<T, R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<Option<R>> {
        match self.delegate.step(arg) {
            StepResult::Execute(next) => StepResult::execute(
                NoErrorPlan{ delegate: next }),
            StepResult::Return(result) => StepResult::Return(Some(result)),
            StepResult::Error(_) => StepResult::Return(None),
        }
    }
}

impl<T, R> Debug for NoErrorPlan<T, R> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.delegate)
    }
}

/// Trait to fold the value (typically sequence of sub-values) to the plan
/// which processes each sub-value in parallel and merges the results.
pub trait FoldIntoParallelPlan<I, T, R>
    where I: Iterator,
          T: 'static,
          R: 'static {
    /// Method converts the `self` value into parallel plan. It starts from
    /// the `empty` return value. It applies `step` to each sub-value to get
    /// a plan to calculate result. It applies `merge` to plan result and
    /// step result to calculate the final result. Resulting plan is returned.
    fn into_parallel_plan<S, M>(self, empty: R, step: S, merge: M) -> Box<dyn Plan<(), R>>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<(), T>>,
          M: 'static + FnMut(R, T) -> R + Clone;
}

impl<I, T, R> FoldIntoParallelPlan<I, T, R> for I
    where I: Iterator,
          T: 'static + Debug,
          R: 'static + Debug {
    fn into_parallel_plan<S, M>(self, empty: R, mut step: S, merge: M) -> Box<dyn Plan<(), R>>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<(), T>>,
          M: 'static + FnMut(R, T) -> R + Clone {
        let plan: Box<dyn Plan<(), R>> = self
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Execute the plan using given input value and return result
    fn execute_plan<T: Debug, R, P>(plan: P, arg: T) -> Result<R, String> where P: 'static + Plan<T, R> {
        let mut step: Box<dyn Plan<(), R>>  = Box::new(ApplyPlan::new(plan, arg));
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
}

