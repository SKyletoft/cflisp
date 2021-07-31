int main() {
	int a   = 5;
	int b   = 3;
	int c   = 2;
	int d   = 7;
	int *fb = 0xFB;
	int *fc = 0xFC;
	*fb     = a + b;
	*fc     = (a + b) - (c ^ d);
}
