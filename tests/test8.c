int factorial(int n) {
	if (n <= 1) {
		return 1;
	} else {
		int rest = factorial(n - 1);
		return n * rest;
	}
}

int main() {
	int res = factorial(5);
	*0xFC   = res;
}
