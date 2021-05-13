static int loops;

int main() {
	while (true) {
		loops = loops + 1;
	}
}

void interrupt() {}