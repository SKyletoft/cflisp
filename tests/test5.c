int foo() {
	return 2;
}

int main() {
	int z = 1;
	int x = 2;
	while (x < 5) {
		int a = 2;
		x     = x + 1;
	}
	for (int i = 0; i < 12; i = i + 1) {
		int y = foo();
		z     = z + y;
	}

	// printf("%X %X\n", z, x);
	*0x0 = z;
	*0x1 = x;
}
