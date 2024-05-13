use crate as rsconf;
use crate::Target;
use once_cell::sync::Lazy;

static CC: Lazy<cc::Build> = Lazy::new(|| {
    // Set TARGET env var if not already set (set by default for build.rs scripts)
    if std::env::var_os("TARGET").is_none() {
        std::env::set_var("TARGET", env!("RSCONF_TARGET"));
    }
    if std::env::var_os("OPT_LEVEL").is_none() {
        std::env::set_var("OPT_LEVEL", "0");
    }
    if std::env::var_os("HOST").is_none() {
        std::env::set_var("HOST", env!("RSCONF_HOST"));
    }
    cc::Build::new()
});

/// Returns a [`Target`] instance initialized with the default [`cc::Build`] instance returned by
/// [`CC`](self::CC).
static DETECTOR: Lazy<Target> = Lazy::new(|| {
    let mut target = Target::from(CC.clone());
    target.set_verbose(true);
    target
});

pub fn target() -> &'static Target {
    &DETECTOR
}

#[test]
fn struct_defined() {
    let target = target();
    assert_eq!(target.has_type_in("struct FILE", &["stdio.h"]), true);
}

#[test]
fn long_long_defined() {
    let target = target();
    assert_eq!(target.has_type("long long"), true);
    assert_eq!(target.has_type_in("long long", &[]), true);
}

#[test]
#[cfg(unix)]
fn dir_defined_no_struct() {
    let target = target();
    assert_eq!(target.has_type_in("DIR", &["dirent.h"]), true);
}

#[test]
#[cfg(unix)]
fn dir_defined() {
    let target = target();
    assert_eq!(target.has_type_in("struct DIR", &["dirent.h"]), true);
}

#[test]
fn struct_not_defined() {
    let target = target();
    assert_eq!(target.has_type_in("DIR", &["stdio.h"]), false);
}

#[test]
fn valid_i32_value() {
    let target = target();
    let result = target.get_i32_value("INT_MIN", &["limits.h"]);
    assert_eq!(result.unwrap(), i32::MIN);
}

#[test]
fn invalid_i32_value() {
    let target = target();
    let result = target.get_i32_value("LLONG_MAX", &["limits.h"]);
    assert!(matches!(result, Err(_)));
}

#[test]
fn valid_u32_value() {
    let target = target();
    let result = target.get_u32_value("INT_MAX", &["limits.h"]);
    assert_eq!(result.unwrap(), 2147483647);
}

#[test]
#[cfg(unix)]
fn dirent_value() {
    let target = target();
    let result = target.get_u32_value("DT_FIFO", &["dirent.h"]);
    assert_eq!(result.unwrap(), 1);
}

#[test]
#[cfg(windows)]
fn generic_read_value() {
    let target = target();
    let result = target.get_u32_value("GENERIC_READ", &["windows.h", "fileapi.h"]);
    assert_eq!(result.unwrap(), 0x80000000);
}

#[test]
fn valid_u64_value() {
    let target = target();
    let result = target.get_u64_value("LLONG_MAX", &["limits.h"]);
    assert_eq!(result.unwrap(), 9223372036854775807);
}

#[test]
fn has_headers() {
    let target = target();
    let result = target.has_headers(&["stdint.h", "stdio.h"]);
    assert_eq!(result, true);
}

#[test]
fn not_has_header() {
    let target = target();
    let result = target.has_header("f_oobar77.h");
    assert_eq!(result, false);
}

#[test]
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn if_none() {
    let target = target();
    let result = target.r#if("__GLIBC_PREREQ(1, 1)", &[]);
    assert_eq!(result, true);
}

#[test]
fn custom_define() {
    let mut build = CC.clone();
    build.define("FOO", "42");
    let target = Target::from(build);
    let result = target.r#if("FOO == 42", &[]);
    assert!(result);
    let result = target.r#if("FOO != 42", &[]);
    assert!(result == false);
}

#[test]
#[cfg(all(unix, target_env = "gnu"))]
fn if_false() {
    let target = target();
    let result = target.r#if("!__GLIBC_PREREQ(10, 3)", &["stdio.h"]);
    assert_eq!(result, true);
}

#[test]
fn not_if() {
    let target = target();
    let result = target.r#if("__FOOO_BAR_12_", &[]);
    assert_eq!(result, false);
}

#[test]
fn if_true() {
    let target = target();
    let result = target.r#if("1", &[]);
    assert_eq!(result, true);
}

#[test]
#[cfg(unix)]
fn has_pthread() {
    let target = target();
    let result = target.has_library("pthread");
    assert_eq!(result, true);
}

#[test]
#[cfg(windows)]
fn has_user32() {
    let target = target();
    assert!(target.has_library("user32"));
}

#[test]
#[cfg(windows)]
fn has_user32_lib() {
    let target = target();
    assert!(target.has_library("user32.lib"));
}

#[test]
fn not_has_library() {
    let target = target();
    let result = target.has_library("foo17_bar");
    assert_eq!(result, false);
}

#[test]
#[cfg(unix)]
fn has_symbol_in_libc() {
    let target = target();
    let result = target.has_symbol("pipe");
    assert_eq!(result, true);
}

#[test]
#[cfg(unix)]
fn has_symbol_pthread_create() {
    let target = target();
    let result = target.has_symbol_in("pthread_create", &["pthread"]);
    assert_eq!(result, true);
}

#[test]
#[cfg(windows)]
fn has_symbol_createfilew() {
    let target = target();
    let result = target.has_symbol_in("CreateFileW", &["kernel32.lib"]);
    assert_eq!(result, true);
}

#[test]
#[cfg(unix)]
fn valid_library_invalid_symbol() {
    let target = target();
    let result = target.has_symbol_in("exhilarate", &["pthread"]);
    assert_eq!(result, false);
}

#[test]
#[cfg(windows)]
fn valid_library_invalid_symbol() {
    let target = target();
    let result = target.has_symbol_in("createfilew", &["kernel32.lib"]);
    assert_eq!(result, false);
}

#[test]
fn invalid_library_no_symbol() {
    let target = target();
    let result = target.has_symbol_in("zoonotico", &["exhilarate"]);
    assert_eq!(result, false);
}

#[test]
#[cfg(target_os = "linux")]
fn has_libraries() {
    let target = target();
    assert!(target.has_libraries(&["pthread", "dl"]));
}

#[test]
#[cfg(windows)]
fn has_libraries() {
    let target = target();
    assert!(target.has_libraries(&["user32", "kernel32.lib"]));
}

#[test]
fn warn_macro() {
    rsconf::warn!("hi alone");
    rsconf::warn!("hello {}", "world");
    rsconf::warn!("hello {} {}", "happy", "friend");
}

#[test]
fn test_paths_invalidation() {
    // with an array ref
    rsconf::rebuild_if_paths_changed(&["foo", "bar"]);
    // with a ref to a vector of strings
    let paths = vec!["foo", "bar"];
    rsconf::rebuild_if_paths_changed(&paths);
}

#[test]
#[cfg(windows)]
fn get_macro_value_win_max() {
    let target = target();
    let definition = target
        .get_macro_value("max(x,y)", &["windows.h"])
        .expect("Error compiling get_macro_value with parameters!")
        .expect("max macro should be defined since we didn't define NOMINMAX!");
    assert_eq!(&definition, "(((x) > (y)) ? (x) : (y))");
}

#[test]
#[cfg(windows)]
/// Make sure the recursive version isn't broken for values that don't use recursion.
fn get_macro_value_recursive_win_max() {
    let target = target();
    let definition = target
        .get_macro_value_recursive("max(x,y)", &["windows.h"])
        .expect("Error compiling get_macro_value with parameters!")
        .expect("max macro should be defined since we didn't define NOMINMAX!");
    assert_eq!(&definition, "(((x) > (y)) ? (x) : (y))");
}

#[test]
#[cfg(target_env = "gnu")]
fn get_macro_value_glibc_version() {
    let target = target();
    let definition = target
        .get_macro_value("__GLIBC_MINOR__", &["features.h"])
        .expect("Error compiling get_macro_value with parameters!")
        .expect("__GLIBC_MINOR__ should be defined in features.h under glibc 6.0+");
    assert!((definition.parse::<i32>()).is_ok());
}

#[test]
fn get_macro_value_recursive() {
    use std::io::Write;

    let temp_dir = crate::TempDir::new().unwrap();
    let header = temp_dir.with_file_name("sample_recursive_macro.h");
    std::fs::File::create(&header)
        .unwrap()
        .write_all(
            b"
    #define INNER(X) BAR
    #define FOO INNER(?)
    ",
        )
        .unwrap();
    let target = target();
    let definition = target
        .get_macro_value_recursive("FOO", &[header.to_str().unwrap()])
        .expect("Error compiling test header!")
        .expect("Failed to get initial macro value");
    assert_eq!(&definition, "BAR");
}

/// Verify that we get the RUSTC version ok as part of the declare_cfg() checks
#[test]
fn declare_cfg() {
    rsconf::declare_cfg("foo", true);
}
