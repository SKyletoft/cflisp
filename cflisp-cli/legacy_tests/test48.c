typedef struct Foo {
	int a;
	int b;
} Foo;

void write_char(char in, char out) {
	out = in;
}

void write_foo(Foo in, Foo out) {
	out = in;
}

void write_char_ptr(char in, char *out) {
	*out = in;
}

void write_foo_ptr(Foo in, Foo *out) {
	*out = in;
}
