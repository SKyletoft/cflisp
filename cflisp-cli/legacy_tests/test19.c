static int foo = 5;

static int *array = {1, 2, 3, 4, 5};

int main() {
	int *array_local = {9, 8, 7, 6, 5};
	int bar          = array[1];
	// array[5]         = 4;
	int baz = &foo;
}