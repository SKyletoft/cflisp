int foo() {
	return 2;
}

int main() {
	int x = foo();
	int y = 4 + foo();
	int z = x + 4 * foo();
}