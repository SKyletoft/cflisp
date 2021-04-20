
int main() {
	int arr_1[5] = {1, 2, 3, 4, 5};
	int arr_2[5] = {0, 0, 0, 0, 0};
	// int *arr_1_ptr = arr_1;
	// int *arr_2_ptr = arr_2;
	// for (int i = 0; i < 5; i = i + 1) { arr_2[i] = arr_1[i]; }
	// for (int i = 0; i < 5; i = i + 1) { arr_2[i] = arr_1[i]; }
	volatile int x = arr_1[3];
}
