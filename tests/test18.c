int sum_to(int n) {
	if (n <= 1) {
		return 1;
	} else {
		return n + sum_to(n - 1);
	}
}

int main() {
	*0xFC = sum_to(10);
	// printf("0x%X\n", sum_to(10));
}