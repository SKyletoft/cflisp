int main() {
	int a   = 1;
	int foo = ((a + a) + (a + a)) + ((a + a) + (a + a));
	*0xFC   = foo;
}
