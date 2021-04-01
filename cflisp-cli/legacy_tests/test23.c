// Expected failure

static int foo = 54;

int main() {
	static int bar = 432;
	int ****adr    = &&&&bar;
}