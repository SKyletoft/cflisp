struct Foo {
	int a;
	int b;
};

Foo ret_struct() {
	const Foo f = {1, 2};
	return f;
}