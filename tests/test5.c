int main() {
	int x = 0;
	while (x > 5) { x = x + 1; }
	for (int i = 0; i < 12; i = i + 1) { int y = foo(); }
	return 0;
}

int foo() {
	return 42;
}