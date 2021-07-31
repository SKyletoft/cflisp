// test14.c
int main() {
	int right = 0;
	int wrong = 0;

	// Expected true
	if (3 == 3) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (3 != 4) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (5 > 4) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (7 < 10) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (4 >= 3) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (13 <= 14) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (24 >= 24) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}
	if (54 <= 54) {
		right = right + 1;
	} else {
		wrong = wrong + 1;
	}

	// Expected false
	if (5 < 4) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (7 > 10) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (23 < 23) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (56 > 56) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (4 <= 3) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (13 >= 14) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (3 != 3) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}
	if (3 == 4) {
		wrong = wrong + 1;
	} else {
		right = right + 1;
	}

	*0xFC = right;
	*0xFB = wrong;
}
