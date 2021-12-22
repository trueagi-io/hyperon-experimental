// Generic plan infrastructure

/// Result of a single step of a plan
pub enum StepResult<R> {
    /// New plan to be executed to get a result
    Execute(Box<dyn Plan<(), R>>),
    /// Result returned
    Return(R),
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
}

/// Plan which gets a value of T type as an input and returns a result of
/// R type as an output after execution
pub trait Plan<T, R> {
    // `self: Box<Self>` allows moving content of the step into the next step.
    // We cannot use `self: Self` because it will be called via `dyn Plan`
    // which doesn't know anything about original type and cannot move it.
    /// Execute one step of the plan
    fn step(self: Box<Self>, arg: T) -> StepResult<R>;
}

/// Execute the plan using given input value and return result
pub fn execute_plan<T, R, P>(plan: P, arg: T) -> R where P: 'static + Plan<T, R> {
    let mut step: Box<dyn Plan<(), R>>  = Box::new(ApplyPlan::new(plan, arg));
    loop {
        match step.step(()) {
            StepResult::Execute(next) => step = next,
            StepResult::Return(result) => return result,
        }
    }
}

// Specific plans to form calculations graph

/// Operator from T to StepResult<R> is a plan which calls itself when executed
// It is not possible to have Plan implementation for both:
// `FnOnce(T) -> R` and `FnOnce(T) -> StepResult<R>`
// so latter is used as more universal
impl<T, R, F> Plan<T, R> for F where F: FnOnce(T) -> StepResult<R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<R> {
        self(arg)
    }
}

/// StepResult itself is a trivial plan which returns itself when executed
impl<R> Plan<(), R> for StepResult<R> {
    fn step(self: Box<Self>, _:()) -> StepResult<R> {
        *self
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

impl<T, R> Plan<(), R> for ApplyPlan<T, R> {
    fn step(self: Box<Self>, _: ()) -> StepResult<R> {
        Plan::step(self.plan, self.arg)
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

impl<T1, T2, R> Plan<T2, R> for PartialApplyPlan<T1, T2, R>
    where T1: 'static + Clone,
          T2: 'static + Clone,
          R: 'static {
    fn step(self: Box<Self>, arg: T2) -> StepResult<R> {
        self.plan.step((self.arg.clone(), arg.clone()))
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

impl<T1: 'static, T2: 'static, R: 'static> Plan<T1, R> for SequencePlan<T1, T2, R> {
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
        }
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

impl<T1, T2> Plan<(), (T1, T2)> for ParallelPlan<T1, T2> 
    where T1: 'static + Clone,
          T2: 'static + Clone {
    fn step(self: Box<Self>, _: ()) -> StepResult<(T1, T2)> {
        match self.first.step(()) {
            StepResult::Execute(next) => StepResult::execute(ParallelPlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(first_result) => StepResult::execute(SequencePlan{
                first: self.second,
                second: Box::new(|second_result|
                    StepResult::ret((first_result, second_result)))
            }),
        }
    }
}

/// Trait to fold the value (typically sequence of sub-values) to the plan
/// which processes each sub-value in parallel and merges the results.
pub trait FoldIntoParallelPlan<I, T, R>
    where I: Iterator,
          T: 'static + Clone,
          R: 'static + Clone {
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
          T: 'static + Clone,
          R: 'static + Clone {
    fn into_parallel_plan<S, M>(self, empty: R, mut step: S, merge: M) -> Box<dyn Plan<(), R>>
        where
          S: FnMut(I::Item) -> Box<dyn Plan<(), T>>,
          M: 'static + FnMut(R, T) -> R + Clone {
        let plan: Box<dyn Plan<(), R>> = self
            .fold(Box::new(|_: ()| StepResult::ret(empty)),
                |plan, step_result| {
                    let mut merge = merge.clone();
                    Box::new(SequencePlan::new(
                        ParallelPlan {
                            first: plan,
                            second: step(step_result),
                        },
                        Box::new(move |(plan_res, step_res)|
                            StepResult::ret(merge(plan_res, step_res))),
                    ))
                }
            );
        plan
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parallel_plan() {
        let mul = SequencePlan::new(
            ParallelPlan::new(
                |_| StepResult::ret(7),
                |_| StepResult::ret(6)),
            |(a, b)| StepResult::ret(a * b),
        );
        assert_eq!(execute_plan(mul, ()), 42);
    }

    #[test]
    fn iterator_into_parallel_plan() {
        let step_counter = &mut 0;
        let args = vec!["1", "2", "3", "4"];
        let plan = args.iter().into_parallel_plan(Vec::new(),
            |n| {
                *step_counter += 1;
                Box::new(ApplyPlan::new(|n: &str| StepResult::ret(n.parse::<u32>().unwrap() + 1), *n))
            },
            |mut a, b| {a.push(b); a});
        assert_eq!(execute_plan(StepResult::Execute(plan), ()), vec![2, 3, 4, 5]);
        assert_eq!(*step_counter, 4);
    }
}

