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
    let mut detector = Detector::from(CC.clone());
    detector.set_verbose(true);
    detector
});

pub fn detector() -> &'static Detector {
    &DETECTOR
}

#[test]
fn symbol_defined() {
    let detector = detector();
    assert_eq!(detector.has_definition("struct FILE", "stdio.h"), true);
}

#[test]
#[cfg(unix)]
fn dir_defined() {
    let detector = detector();
    assert_eq!(detector.has_definition("struct DIR", "dirent.h"), true);
}

#[test]
fn symbol_not_defined() {
    let detector = detector();
    assert_eq!(detector.has_definition("DIR", "stdio.h"), false);
}

#[test]
fn valid_i32_value() {
    let detector = detector();
    let result = detector.get_i32_value("INT_MIN", "limits.h");
    assert_eq!(result.unwrap(), i32::MIN);
}

#[test]
fn invalid_i32_value_borrowed_str() {
    let detector = detector();
    let header = "limits.h".to_owned();
    let result = detector.get_i32_value("LLONG_MAX", header.as_str());
    assert!(matches!(result, Err(_)));
}

#[test]
fn valid_u32_value_string_ref() {
    let detector = detector();
    let header = "limits.h".to_string();
    let result = detector.get_u32_value("INT_MAX", &header);
    assert_eq!(result.unwrap(), 2147483647);
}

#[test]
#[cfg(unix)]
fn dirent_value() {
    let detector = detector();
    let result = detector.get_u32_value("DT_FIFO", "dirent.h");
    assert_eq!(result.unwrap(), 1);
}

#[test]
#[cfg(windows)]
fn generic_read_value() {
    let detector = detector();
    let result = detector.get_u32_value("GENERIC_READ", ["windows.h", "fileapi.h"]);
    assert_eq!(result.unwrap(), 0x80000000);
}

#[test]
fn valid_u64_value() {
    let detector = detector();
    let result = detector.get_u64_value("LLONG_MAX", "limits.h");
    assert_eq!(result.unwrap(), 9223372036854775807);
}

#[test]
fn has_headers() {
    let detector = detector();
    let result = detector.has_headers(&["stdint.h", "stdio.h"]);
    assert_eq!(result, true);
}

#[test]
fn not_has_header_string_ref() {
    let detector = detector();
    let header = "f_oobar77.h".to_owned();
    let result = detector.has_header(&header);
    assert_eq!(result, false);
}

#[test]
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn if_none() {
    let detector = detector();
    let result = detector.r#if("__GLIBC_PREREQ(1, 1)", None);
    assert_eq!(result, true);
}

#[test]
fn custom_define() {
    let mut build = CC.clone();
    build.define("FOO", "42");
    let detector = Detector::from(build);
    let result = detector.r#if("FOO == 42", None);
    assert!(result);
    let result = detector.r#if("FOO != 42", None);
    assert!(result == false);
}

#[test]
#[cfg(all(unix, target_env = "gnu"))]
fn if_false() {
    let detector = detector();
    let result = detector.r#if("!__GLIBC_PREREQ(10, 3)", "stdio.h");
    assert_eq!(result, true);
}

#[test]
fn not_if() {
    let detector = detector();
    let result = detector.r#if("__FOOO_BAR_12_", None);
    assert_eq!(result, false);
}

#[test]
fn if_true() {
    let detector = detector();
    let result = detector.r#if("1", None);
    assert_eq!(result, true);
}

#[test]
#[cfg(unix)]
fn has_pthread() {
    let detector = detector();
    let result = detector.has_library("pthread");
    assert_eq!(result, true);
}

#[test]
#[cfg(windows)]
fn has_user32() {
    let detector = detector();
    assert!(detector.has_library("user32"));
}

#[test]
#[cfg(windows)]
fn has_user32_lib() {
    let detector = detector();
    assert!(detector.has_library("user32.lib"));
}

#[test]
fn not_has_library() {
    let detector = detector();
    let result = detector.has_library("foo17_bar");
    assert_eq!(result, false);
}

#[test]
#[cfg(unix)]
fn has_symbol_pthread_create() {
    let detector = detector();
    let result = detector.has_symbol("pthread_create", "pthread");
    assert_eq!(result, true);
}

#[test]
#[cfg(windows)]
fn has_symbol_createfilew() {
    let detector = detector();
    let result = detector.has_symbol("CreateFileW", "kernel32.lib");
    assert_eq!(result, true);
}

#[test]
#[cfg(unix)]
fn valid_library_invalid_symbol() {
    let detector = detector();
    let result = detector.has_symbol("exhilarate", "pthread");
    assert_eq!(result, false);
}

#[test]
#[cfg(windows)]
fn valid_library_invalid_symbol() {
    let detector = detector();
    let result = detector.has_symbol("createfilew", "kernel32.lib");
    assert_eq!(result, false);
}

#[test]
fn invalid_library_no_symbol() {
    let detector = detector();
    let result = detector.has_symbol("zoonotico", "exhilarate");
    assert_eq!(result, false);
}

#[test]
#[cfg(target_os = "linux")]
fn has_libraries() {
    let detector = detector();
    assert!(detector.has_libraries(&["pthread", "dl"]));
}

#[test]
#[cfg(windows)]
fn has_libraries() {
    let detector = detector();
    assert!(detector.has_libraries(&["user32", "kernel32.lib"]));
}
