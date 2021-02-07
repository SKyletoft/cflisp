int main() {
	int a = 5;
	int b = 3;
	int c = 2;
	int d = 7;
	*0xFB = a + b;
	*0xFC = (a + b) - (c ^ d);
}
