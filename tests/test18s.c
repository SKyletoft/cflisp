int sum_to(int n) {
	if (n <= 1) {
		return 1;
	} else {
		int m   = n - 1;
		int sum = sum_to(m);
		int res = n + sum;
		return res;
	}
}

int main() {
	*0xFC = sum_to(10);
	// printf("0x%X\n", sum_to(10));
}