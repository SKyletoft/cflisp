typedef struct myStruct {
	int a;
	int b;
	int c;
} myStruct;

int main() {
	myStruct foo     = {2 + 5, 4 + 2, 1 + 7};
	myStruct *fooPtr = &foo;
	// printf("a: %d, b: %d, c: %d\n", foo.a, foo.b, foo.c);
	fooPtr->a = 3;
	foo.b     = 2;
	// printf("a: %d, b: %d, c: %d\n", foo.a, foo.b, foo.c);
}