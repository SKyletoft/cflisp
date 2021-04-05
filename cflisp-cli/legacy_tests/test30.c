typedef struct Foo {
	int a;
	int b;
	int c;
} Foo;

typedef struct Bar {
	int x;
	int y;
} Bar;

int f(Foo foo) {
	return foo.a;
}

int g(Bar bar) {
	return bar.y;
}

int main() {
	Bar bar = {1, 2};
	// Foo foo = {1, 2, 3};
	int b = g(bar);
	// int a   = g(foo);
}
