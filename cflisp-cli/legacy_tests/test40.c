void foo() {
	static int bar[5];
	for (int i = 0; i < 5; i = i + 1) {
		bar[i] = bar[i] + 1;
	}
}