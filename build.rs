fn main() {
    println!(
        "cargo:rustc-env=RSCONF_TARGET={}",
        std::env::var("TARGET").unwrap()
    );
    println!(
        "cargo:rustc-env=RSCONF_HOST={}",
        std::env::var("HOST").unwrap()
    );
}
