int foo(int i, char c, bool b) {
	return i & c;
}

int main() {
	bool c = foo(23, 'a', true);
}