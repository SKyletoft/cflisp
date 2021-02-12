int factorial(int n) {
	if (n <= 1) {
		return 1;
	} else {
		int m    = n - 1;
		int rest = factorial(m);
		return n * rest;
	}
}

int main() {
	int res = factorial(5);
	*0xFC   = res;
}
