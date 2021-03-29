typedef struct myStruct {
	int a;
	int b;
	int c;
} myStruct;

myStruct bar(myStruct ms, int x, int y, int z) {
	ms.a = x;
	ms.b = y;
	ms.c = z;
	return ms;
}

int *get_ptr() {
	return 0;
}

static myStruct static_struct = {1, 2, 3};

int main() {
	myStruct foo     = {2 + 5, 4 + 2, 1 + 7};
	myStruct fooCopy = bar(foo, 1, 2, 3);
	myStruct *fooPtr = 0; //&foo;
	// printf("a: %d, b: %d, c: %d\n", foo.a, foo.b, foo.c);
	fooPtr->a = 3;
	foo.b     = 2;
	int a     = foo.a;
	// (*fooPtr).c = 86;
	// printf("a: %d, b: %d, c: %d\n", foo.a, foo.b, foo.c);
}
