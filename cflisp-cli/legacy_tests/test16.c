int main() {
	int a = 1;
	int b = 2;
	int c = 3;
	int d = 4;
	*0xFC = a + b + c + d;
	if (a == 1) { a = 11; }
	*0xFB = a + b + c + d;
}