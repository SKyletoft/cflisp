// test15.c
int main() {
	int *fc = 0xFC;
	// if (true) {
	// int a   = 1;
	// int b   = a;
	// int foo = ((a + a) + (a + a)) + ((a + a) + (a + a));     // 8
	// int bar = ((a + a) + (a + a)) + ((a + a) + (a + a)) + 6; // 14, 0x0E
	//*0xFC   = foo;
	//*0xFB   = bar;
	//}

	int a = 1;
	int b = a + 1;
	int c = b + 2;
	int d = c + 3;
	int e = d + 4;
	int f = e + 5;
	int g = f + 6;
	int h = g + 7;

	// int foo = ((a + b) + (c + d)) + ((e + f) + (g + h));
	int foo = ((a + b) + (c + d)) + ((e + f) + (g + h));
	*fc     = foo;

	// int bar = a + b + c + d + e + f + g + h;
	// int bar = a+b+c+d+e+f+g+h;
	//*0xFB   = bar;
	// printf("0x%X\n", foo);
}
