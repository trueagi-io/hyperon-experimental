use super::collections::{ListMap, Equality, DefaultEquality};

use std::fmt::{Debug, Display, Formatter};
use itertools::Itertools;

pub fn compare_vec_no_order<T, A, B, E>(actual: A, expected: B, _cmp: E) -> VecDiff<T, E>
where
    A: Iterator<Item=T>,
    B: Iterator<Item=T>,
    E: Equality<T>,
{
    let mut diff: ListMap<T, Count, E> = ListMap::new();
    for i in actual {
        diff.entry(i).or_default().actual += 1;
    }
    for i in expected {
        diff.entry(i).or_default().expected += 1;
    }
    diff = diff.into_iter().filter(|(_v, c)| c.actual != c.expected).collect();
    VecDiff{ diff }
}

#[derive(Default)]
struct Count {
    actual: usize,
    expected: usize,
}

pub struct VecDiff<T, E: Equality<T>> {
    diff: ListMap<T, Count, E>,
}

struct FormatAsDebug<T: Debug>(T);
impl<T: Debug> Display for FormatAsDebug<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

struct FormatAsDisplay<T: Display>(T);
impl<T: Display> Display for FormatAsDisplay<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<T, E: Equality<T>> VecDiff<T, E> {
    pub fn has_diff(&self) -> bool {
        !self.diff.is_empty()
    }

    pub fn as_display(&self) -> Option<String> where T: Display {
        self.as_string(FormatAsDisplay)
    }

    pub fn as_debug(&self) -> Option<String> where T: Debug {
        self.as_string(FormatAsDebug)
    }
    
    fn as_string<'a, F, I: 'a + Display>(&'a self, f: F) -> Option<String>
        where F: Copy + Fn(&'a T) -> I
    {
        let mut diff = String::new();
        if self.has_diff() {
            let mut missed = self.diff.iter()
                .filter(|(_v, c)| c.actual < c.expected)
                .flat_map(|(v, c)| std::iter::repeat_n(v, c.expected - c.actual))
                .map(f)
                .peekable();
            let mut excessive = self.diff.iter()
                .filter(|(_v, c)| c.actual > c.expected)
                .flat_map(|(v, c)| std::iter::repeat_n(v, c.actual - c.expected))
                .map(f)
                .peekable();
            if missed.peek().is_some() {
                diff.push_str(format!("Missed results: {}", missed.format(", ")).as_str());
            }
            if excessive.peek().is_some() {
                if !diff.is_empty() {
                    diff.push_str("\n");
                }
                diff.push_str(format!("Excessive results: {}", excessive.format(", ")).as_str());
            }
            Some(diff)
        } else {
            None
        }
    }
}

#[macro_export]
macro_rules! assert_eq_no_order {
    ($actual:expr, $expected:expr) => {
        {
            let diff = $crate::assert::compare_vec_no_order($actual.iter(), $expected.iter(),
                $crate::collections::DefaultEquality{}).as_debug();
            assert!(diff.is_none(),
                "(actual != expected)\nActual: {:?}\nExpected: {:?}\n{}",
                    $actual, $expected, diff.unwrap());
        }
    }
}

pub fn metta_results_eq<T: PartialEq>(
    actual: &Result<Vec<Vec<T>>, String>, expected: &Result<Vec<Vec<T>>, String>) -> bool
{
    match (actual, expected) {
        (Ok(actual), Ok(expected)) if actual.len() == expected.len() => {
            for (actual, expected) in actual.iter().zip(expected.iter()) {
                let diff = compare_vec_no_order(actual.iter(), expected.iter(), DefaultEquality{});
                if diff.has_diff() {
                    return false;
                }
            }
            true
        },
        _ => false,
    }
}

#[macro_export]
macro_rules! assert_eq_metta_results {
    ($actual:expr, $expected:expr) => {
        {
            let actual = &$actual;
            let expected = &$expected;
            assert!($crate::assert::metta_results_eq(actual, expected),
                "(actual == expected)\n  actual: {:?}\n expected: {:?}", actual, expected);
        }
    }
}
