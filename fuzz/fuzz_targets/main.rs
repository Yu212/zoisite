#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        let opts = zoisite::LifecycleOptions {
            base_name: "".to_string(),
            debug: false,
            run_jit: false,
            optimize: false,
            output_syntax: false,
            output_ir: false,
            output_submission: false,
            output_executable: false,
        };
        zoisite::run_lifecycle(text, opts);
    }
});
