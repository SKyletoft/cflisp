typedef struct myStruct {
	int a;
	int b;
	int c;
} myStruct;

// No nested structs yet

// typedef struct innerStruct {
//	myStruct abc;
//	myStruct def;
//} innerStruct;

void bar(myStruct ms, int x, int y, int z, myStruct *out) {
	ms.a = x;
	ms.b = y;
	ms.c = z;
	*out = ms;
}

int *get_ptr() {
	return 0;
}

int foobar(int x) {
	return x + 2;
}

static myStruct static_struct = {1, 2, 3};

int main() {
	int x        = foobar(3);
	myStruct foo = {2 + 5, 4 + 2, 1 + 7};
	myStruct fooCopy;
	bar(foo, 1, 2, 3, &fooCopy);
	myStruct *fooPtr = 0; //&foo;
	// printf("a: %d, b: %d, c: %d\n", foo.a, foo.b, foo.c);
	fooPtr->a = 3;
	foo.b     = 2;
	int a     = foo.a;
	int b     = fooPtr->b;
	// (*fooPtr).c = 86;
	// printf("a: %d, b: %d, c: %d\n", foo.a, foo.b, foo.c);
}
