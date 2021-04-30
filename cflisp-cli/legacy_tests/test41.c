static int LEN = 5;

void foo(int *a, int *b) {
	for (int i = 0; i < LEN; i = i + 1) {
		a[i] = b[i];
	}
}

void bar(int *a, int *b) {
	a[0] = a[0];
	b[0];
	for (int i = 1; i < LEN; i = i + 1) {
		a[i] = b[i];
	}
}

int main() {
	foo(0, 0);
	bar(0, 0);
}