int main() {
	if (true) {
		int a   = 1;
		int b   = a;
		int foo = ((a + a) + (a + a)) + ((a + a) + (a + a));     // 8
		int bar = ((a + a) + (a + a)) + ((a + a) + (a + a)) + 6; // 14, 0x0E
		*0xFC   = foo;
		*0xFB   = bar;
	}
}
