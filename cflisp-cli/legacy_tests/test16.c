// test16.c
int main() {
	int *fc = 0xFC;
	int *fb = 0xFB;
	int a   = 1;
	int b   = 2;
	int c   = 3;
	int d   = 4;
	*fc     = a + b + c + d;
	if (a == 1) { a = 11; }
	*fb = a + b + c + d;
}