#![allow(non_camel_case_types)]

pub mod util;
pub mod atom;
pub mod space;
pub mod metta;

/// @brief Initializes the logger
/// @ingroup misc_group
/// @note This function should be called once, prior to any other calls to Hyperon C functions
///
//TODO: Is there a way we can get rid of this function in the external API?
//  Discussion about alternative ways to init the logger here: https://github.com/trueagi-io/hyperon-experimental/pull/314
//  and here: https://github.com/trueagi-io/hyperon-experimental/issues/146
#[no_mangle]
pub extern "C" fn init_logger() {
   hyperon::common::init_logger(false);
}

