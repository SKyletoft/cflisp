int main() {
	int *nullptr = 0;
	int x        = 5;
	int foo      = x;
	if (x == 5) { x = 3; }
	*nullptr = x;
}
