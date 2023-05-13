use crate::Detector;
use once_cell::sync::Lazy;

static CC: Lazy<cc::Build> = Lazy::new(|| {
    // Set TARGET env var if not already set
    if std::env::var_os("TARGET").is_none() {
        #[cfg(windows)]
        std::env::set_var("TARGET", "x86_64-pc-windows-msvc");
        #[cfg(target_os = "linux")]
        std::env::set_var("TARGET", "x86_64-linux-unknown-gnu");
        #[cfg(target_os = "mac_os")]
        std::env::set_var("TARGET", "x86_64-apple-darwin");
        #[cfg(target_os = "freebsd")]
        std::env::set_var("TARGET", "x86_64-unknown-freebsd");
    }
    if std::env::var_os("OPT_LEVEL").is_none() {
        std::env::set_var("OPT_LEVEL", "0");
    }
    if std::env::var_os("HOST").is_none() {
        std::env::set_var("HOST", std::env::var_os("TARGET").unwrap());
    }
    cc::Build::new()
});

/// Returns a [`Detector`] instance initialized with the default [`cc::Build`] instance returned by
/// [`CC`](self::CC).
static DETECTOR: Lazy<Detector> = Lazy::new(|| {
    let mut detector = Detector::new(CC.clone()).unwrap();
    detector.set_verbose(true);
    detector
});

pub fn detector() -> &'static Detector {
    &DETECTOR
}

#[test]
fn symbol_defined() {
    let detector = detector();
    assert_eq!(detector.has_definition("stdio.h", "struct FILE"), true);
}

#[test]
#[cfg(unix)]
fn dir_defined() {
    let detector = detector();
    assert_eq!(detector.has_definition("dirent.h", "struct DIR"), true);
}

#[test]
fn symbol_not_defined() {
    let detector = detector();
    assert_eq!(detector.has_definition("stdio.h", "DIR"), false);
}

#[test]
fn valid_u32_value() {
    let detector = detector();
    let result = detector.get_u32_value("limits.h", "INT_MAX");
    assert_eq!(result.unwrap(), 2147483647);
}

#[test]
#[cfg(target_os = "linux")]
fn dirent_value() {
    let detector = detector();
    let result = detector.get_u32_value("dirent.h", "DT_FIFO");
    assert_eq!(result.unwrap(), 1);
}

#[test]
fn valid_u64_value() {
    let detector = detector();
    let result = detector.get_u64_value("limits.h", "LLONG_MAX");
    assert_eq!(result.unwrap(), 9223372036854775807);
}

#[test]
fn valid_i32_value() {
    let detector = detector();
    let result = detector.get_i32_value("limits.h", "INT_MIN");
    assert_eq!(result.unwrap(), i32::MIN);
}

#[test]
fn invalid_i32_value() {
    let detector = detector();
    let result = detector.get_i32_value("limits.h", "LLONG_MAX");
    assert!(matches!(result, Err(_)));
}

#[test]
fn has_header() {
    let detector = detector();
    let result = detector.has_header("stdint.h");
    assert_eq!(result, true);
}

#[test]
fn not_has_header() {
    let detector = detector();
    let result = detector.has_header("f_oobar77.h");
    assert_eq!(result, false);
}

#[test]
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn glibc_greater_than_1_1() {
    let detector = detector();
    let result = detector.r#if(None, "__GLIBC_PREREQ(1, 1)");
    assert_eq!(result, true);
}

#[test]
#[cfg(all(unix, target_env = "gnu"))]
fn glibc_less_than_10_3() {
    let detector = detector();
    let result = detector.r#if(None, "!__GLIBC_PREREQ(10, 3)");
    assert_eq!(result, true);
}

#[test]
fn not_if() {
    let detector = detector();
    let result = detector.r#if(None, "__FOOO_BAR_12_");
    assert_eq!(result, false);
}

#[test]
fn if_true() {
    let detector = detector();
    let result = detector.r#if(None, "1");
    assert_eq!(result, true);
}

#[test]
#[cfg(target_os = "linux")]
fn has_library() {
    let detector = detector();
    let result = detector.has_library("pthread");
    assert_eq!(result, true);
}

#[test]
fn not_has_library() {
    let detector = detector();
    let result = detector.has_library("foo17_bar");
    assert_eq!(result, false);
}

#[test]
#[cfg(target_os = "linux")]
fn has_symbol() {
    let detector = detector();
    let result = detector.has_symbol("pthread", "pthread_create");
    assert_eq!(result, true);
}

#[test]
#[cfg(target_os = "linux")]
fn valid_library_invalid_symbol() {
    let detector = detector();
    let result = detector.has_symbol("pthread", "exhilarate");
    assert_eq!(result, false);
}

#[test]
#[cfg(target_os = "linux")]
fn invalid_library_no_symbol() {
    let detector = detector();
    let result = detector.has_symbol("zoonotico", "exhilarate");
    assert_eq!(result, false);
}
