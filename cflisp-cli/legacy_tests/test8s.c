// test8s.c
int factorial(int n) {
	n;
	if (n <= 1) {
		n;
		return 1;
	} else {
		n;
		int m    = n - 1;
		int rest = factorial(m);
		int prod = rest * n;
		return prod;
	}
}

int main() {
	int res = factorial(5);
	int *fc = 0xFC;
	*fc     = res;
}
