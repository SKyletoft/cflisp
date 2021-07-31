// test8.c
int factorial(int n) {
	if (n <= 1) {
		return 1;
	} else {
		return n * factorial(n - 1);
	}
}

int main() {
	int res = factorial(5);
	int *fc = 0xFC;
	*fc     = res;
}
