typedef struct Foo {
	int a;
	int b;
} Foo;

Foo create_foo() {
	Foo f = {1, 2};
	return f;
}

Foo transform_foo(Foo f) {
	return f;
}

int main() {
	Foo f = create_foo();
	Foo g = transform_foo(f);
	// Foo h = transform_foo(create_foo());
}
