pub mod util;
pub mod atom;
pub mod space;
pub mod metta;

#[no_mangle]
pub extern "C" fn init_logger() {
   let _ = env_logger::builder().is_test(true).try_init();
}

