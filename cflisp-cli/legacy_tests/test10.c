int foo(int x, int y) {
	return x + y;
}

int main() {
	int a = 2 + 43 ^ 23;
	int b = 324 + (34 & 237);
	*0xFC = foo(5, 2);
	*0xFB = foo(2 + 43 ^ 23, 324 + (34 & 237));
}
