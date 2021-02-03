#include <stdio.h>

int main() {
	int foo = 5;
	do {
		int foo = 6;
		printf("%d\n", foo); // 6
	} while (0);
	printf("%d\n", foo); // 5
}