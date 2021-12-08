// Generic plan infrastructure

pub enum StepResult<T, R> {
    Call(Box<dyn Plan<T, R>>),
    Return(R),
}

impl<T, R> StepResult<T, R> {
    pub fn call<P>(next: P) -> Self where P: 'static + Plan<T, R> {
        Self::Call(Box::new(next))
    }

    pub fn ret(result: R) -> Self {
        Self::Return(result)
    }
}

pub trait Plan<T, R> {
    // `self: Box<Self>` allows moving content of the step into the next step.
    // We cannot use `self: Self` because it will be called via `dyn Plan`
    // which doesn't know anything about original type and cannot move it.
    fn step(self: Box<Self>, arg: T) -> StepResult<(), R>;
}

pub fn interpret<T, R, P>(plan: P, arg: T) -> R where P: 'static + Plan<T, R> {
    let mut step: Box<dyn Plan<(), R>>  = Box::new(ApplyPlan::new(plan, arg));
    loop {
        match step.step(()) {
            StepResult::Call(next) => step = next,
            StepResult::Return(result) => return result,
        }
    }
}

// Specific plans to form calculations graph

// It is not possible to have Plan implementation for both:
// `Fn(&I) -> O` and `Fn(&I) -> StepResult<(), O>`
// so latter is used as more universal
impl<T, R, F> Plan<T, R> for F where F: FnOnce(T) -> StepResult<(), R> {
    fn step(self: Box<Self>, arg: T) -> StepResult<(), R> {
        self(arg)
    }
}
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
    fn step(self: Box<Self>, _: ()) -> StepResult<(), R> {
        Plan::step(self.plan, self.arg)
    }
}

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
    fn step(self: Box<Self>, arg: T2) -> StepResult<(), R> {
        self.plan.step((self.arg.clone(), arg.clone()))
    }
}

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
    fn step(self: Box<Self>, arg: T1) -> StepResult<(), R> {
        match self.first.step(arg) {
            StepResult::Call(next) => StepResult::call(SequencePlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(result) => StepResult::call(ApplyPlan{
                arg: result,
                plan: self.second,
            }),
        }
    }
}

pub struct ParallelPlan<T1, T2> {
    first: Box<dyn Plan<(), T1>>,
    second: Box<dyn Plan<(), T2>>,
}

impl<T1, T2> Plan<(), (T1, T2)> for ParallelPlan<T1, T2> 
    where T1: 'static + Clone,
          T2: 'static + Clone {
    fn step(self: Box<Self>, _: ()) -> StepResult<(), (T1, T2)> {
        match self.first.step(()) {
            StepResult::Call(next) => StepResult::call(ParallelPlan{
                first: next,
                second: self.second,
            }),
            StepResult::Return(first_result) => StepResult::call(SequencePlan{
                first: self.second,
                second: Box::new(|second_result|
                    StepResult::ret((first_result, second_result)))
            }),
        }
    }
}

pub trait FoldIntoParallelPlan<I, R>
    where I: Iterator,
          R: 'static + Clone {
    fn into_parallel_plan<S, M>(self, empty: R, step: S, merge: M) -> StepResult<(), R>
        where
          S: Fn(I::Item) -> Box<dyn Plan<(), R>>,
          M: 'static + Fn(R, R) -> R + Clone;
}

impl<I, R> FoldIntoParallelPlan<I, R> for I
    where I: Iterator,
          R: 'static + Clone {
    fn into_parallel_plan<S, M>(self, empty: R, step: S, merge: M) -> StepResult<(), R>
        where
          S: Fn(I::Item) -> Box<dyn Plan<(), R>>,
          M: 'static + Fn(R, R) -> R + Clone {
        let plan: Box<dyn Plan<(), R>> = self
            .fold(Box::new(|_: ()| StepResult::ret(empty)),
                |plan, step_result| {
                    let merge = merge.clone();
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
        StepResult::Call(plan)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<T1, T2> ParallelPlan<T1, T2> {
        fn new<P1, P2>(first: P1, second: P2) -> Self
            where P1: 'static + Plan<(), T1>,
                  P2: 'static + Plan<(), T2> {
            ParallelPlan{
                first: Box::new(first),
                second: Box::new(second)
            }
        }
    }

    #[test]
    fn parallel_plan() {
        let mul = SequencePlan::new(
            ParallelPlan::new(
                |_| StepResult::ret(7),
                |_| StepResult::ret(6)),
            |(a, b)| StepResult::ret(a * b),
        );
        assert_eq!(interpret(mul, ()), 42);
    }

}

