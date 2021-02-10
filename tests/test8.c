int factorial(int n) {
	if (2 > n) {
		return 1;
	} else {
		return n * factorial(n - 1);
	}
}

int main() {
	int res = factorial(5);
	*0xFC   = res;
}
