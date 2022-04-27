#![allow(non_camel_case_types)]

pub mod util;
pub mod atom;
pub mod space;
pub mod metta;

#[no_mangle]
pub extern "C" fn init_logger() {
   hyperon::common::init_logger(false);
}

