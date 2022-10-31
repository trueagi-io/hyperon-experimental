#[macro_use]
extern crate mopa;

pub mod atom;
pub mod common;
pub mod space;
pub mod metta;

pub use atom::*;
pub mod child_ai;

use ctor::ctor;

#[ctor]
fn on_load() {
    common::init_logger(false);
}
