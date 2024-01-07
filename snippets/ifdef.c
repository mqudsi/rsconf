#include <stdio.h>
{}

int main() {{
#if defined({1})
	return 0;
#else
	// INVALID@INVALID
	// It turns out INVALID is a reserved keyword under MSVC, even when it's #if'd out!
	FOO17@BAR24
#endif
}}
