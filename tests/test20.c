int main() {
	int right = 0;
	int wrong = 0;

	if (5 < 6) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}

	if (7 < 7) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}

	if (8 < 2) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}

	*0xFB = wrong;
	*0xFC = right;
}