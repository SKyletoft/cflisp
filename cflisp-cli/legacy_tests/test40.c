void foo() {
	static int bar[2] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
	for (int i = 0; i < 5; i = i + 1) {
		bar[i] = bar[i] + 1;
	}
}
