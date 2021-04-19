int main() {
	int *arr_1 = {1, 2, 3, 4, 5};
	int *arr_2 = {0, 0, 0, 0, 0};
	// for (int i = 0; i < 5; i = i + 1) { arr_2[i] = arr_1[i]; }
	volatile int x = arr_1[3];
}