int main() {
	const int arr_1[5] = {1, 2, 3, 4, 5};
	int arr_2[5];
	// arr_2[0] = arr_1[0];
	for (int i = 0; i < 5; i = i + 1) { arr_2[i] = arr_1[i]; }
	/*arr_2[0] = arr_1[0];
	arr_2[1] = arr_1[1];
	arr_2[2] = arr_1[2];
	arr_2[3] = arr_1[3];
	arr_2[4] = arr_1[4];*/
	arr_2[2] = 0;
}
