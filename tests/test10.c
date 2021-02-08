int foo(int x, int y) {
	return x + y;
}

int main() {
	*0xFC = foo(5, 2);
}