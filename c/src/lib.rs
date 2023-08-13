#![allow(non_camel_case_types)]

pub mod util;
pub mod atom;
pub mod space;
pub mod metta;

/// @brief Initializes the logger
/// @ingroup misc_group
/// @note This function should be called once, prior to any other calls to Hyperon C functions
///
//TODO: Is there a way we can get rid of this function?  Look up the github issue related to this function to remind myself of the latest state
#[no_mangle]
pub extern "C" fn init_logger() {
   hyperon::common::init_logger(false);
}

