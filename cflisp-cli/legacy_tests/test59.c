// test59.c

#include <flisp_imports.h>

bool f() {
	if (2 == 6) {}
	return true;
}

int main() {
	bool x = f();
}
