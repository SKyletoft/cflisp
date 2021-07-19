static int foo = 5;

static int *array = {1, 2, 3, 4, 5};

static int array_with_len[5] = {6, 7, 8, 9, 0};

int main() {
	int *array_local = {9, 8, 7, 6, 5};
	int bar          = array[1];
	array[5]         = 4;
	int baz          = &foo;
	int bar_2        = array_with_len[1];
}
