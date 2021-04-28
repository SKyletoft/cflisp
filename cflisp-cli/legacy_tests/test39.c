void foo() {
	static int global_counter;
	global_counter = global_counter + 12;
}

int main() {
	for (int i = 0; i < 3; i = i + 1) {
		foo();
	}
}