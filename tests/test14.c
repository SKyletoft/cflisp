int main() {
	int x = 0;

	// Expected true
	// if (3 == 3) { x = x + 1; }
	// if (3 != 4) { x = x + 1; }
	// if (3 == 4) { x = x + 1; }
	// if (3 != 3) { x = x + 1; }
	// if (5 > 4) { x = x + 1; }
	// if (7 < 10) { x = x + 1; }
	// if (4 >= 3) { x = x + 1; }
	// if (13 <= 14) { x = x + 1; }
	// if (24 >= 24) { x = x + 1; }
	// if (54 <= 54) { x = x + 1; }

	//*0xFB = x;

	// Expected false
	if (5 < 4) { x = x + 1; }
	if (7 > 10) { x = x + 1; }
	if (4 <= 3) { x = x + 1; }
	if (13 >= 14) { x = x + 1; }

	*0xFC = x;
}
