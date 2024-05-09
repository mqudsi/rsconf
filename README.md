# rsconf

[![crates.io](https://img.shields.io/crates/v/rsconf.svg)](https://crates.io/crates/rsconf)
[![docs.rs](https://docs.rs/rsconf/badge.svg)](https://docs.rs/rsconf/latest/rsconf/)

`rsconf` is a minimal, cross-platform build-time helper for testing a target system from a `build.rs` to unlock conditional compilation (via rustc's features or cfg flags) and link against the right system libraries, particularly for ffi purposes. Think of it as an autoconf replacement, but sane and intelligible.

## Usage

`rsconf` is currently architectured as two separate components with [`rsconf::Target`](https://docs.rs/rsconf/latest/rsconf/struct.Target.html) being the primary means of testing the targeted system for the presence of libraries, symbols, system headers, types, and extracting compile-time constants; and the freestanding top-level functions in the `rsconf` crate's root namespace facilitating easier manipulation of Cargo and rustc (typically by wrapping the `println!("cargo:{...}")` messages [that influence how crates are built](https://rustwiki.org/en/cargo/reference/build-scripts.html)).

[`rsconf::Target`](https://docs.rs/rsconf/latest/rsconf/struct.Target.html) is built on top of [the `cc` crate](https://docs.rs/cc/latest/cc/) and uses it to obtain a working toolchain for the target platform. `Target` can be initialized with `Target::new()` (which internally initializes a `cc::Build` instance with the default configuration) or you can pass in a configured/customized `cc::Build` for `Target` to use for all subsequent tests via [`Target::new_from()`](https://docs.rs/rsconf/latest/rsconf/struct.Target.html#method.new_from).

## Design notes

Special care is taken to distinguish features that are compatible with cross-compilation from those that aren't. Currently the bulk of `rsconf::Target` tests are designed with cross-compilation in mind, but the functions to extract compile-time constants from the C standard library or system header files (the [`Target::get_xxx_value()`](https://docs.rs/rsconf/latest/rsconf/struct.Target.html#method.get_i32_value)] family of functions) are currently not compatible with cross-compilation as they require the ability to execute an test executable compiled for the target system. There is a possibility that these may become cross-compilation-safe in the future as a different approach is explored.

`rsconf` intentionally does not expose a facility to extract values that are *not* compile-time constants (e.g. `Target::has_symbol()` can be used to check for the presence of a symbol in an external library but the `get_xxx_value()` functions do not provide a facility for linking against system libraries) because anything only defined at run-time cannot be assumed to not change its value during the course of execution or to have the same value across different systems - the correct approach here is to test for the presence of a symbol (in a library or in the standard C library) and then declare it in your code either as an `extern "C" fn` or as a `#[no_mangle]` static variable and obtain the value at run-time.

At this time, `rsconf` does not expose any functionality for package discovery (as opposed to searching for headers and libraries in either the default system paths or those search paths that the `cc::Build` instance was configured with then passed to `Target::new_from()`. If you need that functionality you are encouraged to use a crate such as [`pkg_config`](https://docs.rs/pkg-config/latest/pkg_config/) to find the path to the library or header files and then configure the `cc::Build` instance with those paths before passing it in to `Target::new_from()`.

## Usage example

Here's an example of how to check for a symbol in multiple libraries in your `build.rs` build script with `rsconf`, then use that information from your crate to conditionally compile code. Here we'll test for a low-level curses library and verify that the one we found has the symbols we need before using those from rust.

In `build.rs`:

```rust
use rsconf::{LinkType, Target};

fn find_curses(system: &Target) -> bool {
    // We need to try different library names depending on the platform
    for lib in [ "tinfo", "terminfo" ] {
        if !system.has_library(lib) {
            continue;
        }
        if system.has_symbol("cur_term", lib)
            && system.has_symbol("setupterm", lib)
        {
            // We found what we need, so make sure we link against it.
            rsconf::link_library(lib, LinkType::Default);
            return true;
        }
    }
    return false;
}

fn main() {
    let system = Target::new();
    if find_curses(&system) {
        rsconf::enable_cfg("curses");
    } else {
        rsconf::warn!("Unable to find a curses library!");
    }
}
```

then in `src/main.rs`:

```rust
#[cfg(curses)]
extern "C" {
    static mut cur_term: *const libc::c_void;
    fn setupterm(term: *const libc::c_void, fd: libc::c_int, err: &mut libc::c_int);
}

/// A safe wrapper around the curses `setupterm()` API.
#[cfg(not(curses)]
fn setup_term() -> bool { return false; }

/// A safe wrapper around the curses `setupterm()` API.
#[cfg(curses)]
fn setup_term() -> bool {
    let mut error: i32 = 0;
    let result = unsafe {
        setupterm(std::ptr::null(), libc::STDOUT_FILENO, &mut error)
    };

    result == 0
}
```

Note that there are actually [convenience methods](https://docs.rs/rsconf/latest/rsconf/) that significantly reduce the boilerplate above, but the more verbose api has been used for illustration purposes.

## License

`rsconf` is released under a dual MIT and Apache 2.0 license. All rights are otherwise reserved, copyright Mahmoud Al-Qudsi 2024.
