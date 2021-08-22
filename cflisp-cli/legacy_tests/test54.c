// test54.c

static const int *OUT = 0xFC;
static int COUNTER    = 0xFF;

int main() {
	int b = 23;
	while (true) {
		*OUT = COUNTER;
		if (COUNTER != 0) { COUNTER -= 1; }
	}
}

void interrupt() {
	COUNTER = 5;
}
