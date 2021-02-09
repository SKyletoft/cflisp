int factorial(int n) {
	if (n < 2) {
		return 1;
	} else {
		return n * factorial(n - 1);
	}
}

int main() {
	int res = factorial(5);
	*0xFC   = res;
}
