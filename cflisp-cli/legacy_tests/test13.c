// test13.c
int main() {
	int *fc = 0xFC;
	int *fb = 0xFB;
	*fb     = 5 * 4 * 3 * 2 * 1;
	*fc     = 0x5 - 0x2;
}
