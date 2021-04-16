int foo() {
	int x = 4;
	return x;
}

int bar() {
	int x;
	x     = 4;
	int z = foo();
	int y = 23;
	return y + z;
}