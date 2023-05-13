#include <stddef.h>

extern int {0};

int main() {{
	// Try to get the address of the symbol to make sure linking is required.
	void *addr = &{0};
	return (int)(size_t)(addr);
}}
