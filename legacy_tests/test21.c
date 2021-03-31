int main() {
	int x = 1;
	int y = 1;
	while (true) {
		*0xFC = x;
		//*0xFB = y;
		x = x << y;
		// y     = y >> 2;
		if (x == 0) { x = 1; }
		// if (y == 0) { y = 128; }
	}
}