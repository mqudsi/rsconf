{0}

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

// Pass the macro itself, it will expand to its value
#define EXFILTRATE_MACRO_VALUE(macro) \
    _Pragma(STRINGIFY(message("EXFIL:::" TOSTRING(macro) ":::EXFIL")))

// Use our new macro to generate a pragma message
#ifdef {1}
EXFILTRATE_MACRO_VALUE({2});
#endif

int main() {{
    return 0;
}}
