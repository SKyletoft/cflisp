int foo(int x, int y) {
	return x + y;
}

void bar() {}
void bar2(int x, int y, int z) {}
int bar3() {
	return 2;
}

int main() {
	int a = 5;
	int b = bar3();
	*0xFC = foo(5, 2);
	*0xFB = foo(2 + 43 ^ 23, 324 + (34 & 237));
	bar();
	bar2(5, 4, 4);
	345 + 34;
}