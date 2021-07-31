// test10.c
int foo(int x, int y) {
	return x + y;
}

int main() {
	int a   = 2 + 43 ^ 23;
	int b   = 324 + (34 & 237);
	int *fc = 0xFC;
	int *fb = 0xFB;
	*fc     = foo(5, 2);
	*fb     = foo(2 + 43 ^ 23, 324 + (34 & 237));
}
