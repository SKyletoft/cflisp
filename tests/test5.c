int foo() {
	return 2;
}

int main() {
	int z = 1;
	int x = 2;
	while (x < 5) { x = x + 1; }
	for (int i = 0; 12 > i; i = i + 1) {
		int y = foo();
		z     = z + y;
		//	printf("i: %d\n", i);
	}

	printf("x: %X z: %X\n", x, z);
	//*0x0 = z;
	//*0x1 = x;
}
