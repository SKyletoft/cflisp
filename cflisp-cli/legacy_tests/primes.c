static int PRIMES[61]  = {2, 3};
static int PRIME_COUNT = 2;
static int REM;
static int I;

bool is_prime(int n) {
	for (I = 0; I < PRIME_COUNT; I = I + 1) {
		REM = n % PRIMES[I];
		if (REM == 0) { return false; }
	}
	return true;
}

int main() {
	for (int candidate = 5; candidate >= 5; candidate = candidate + 2) {
		if (is_prime(candidate)) {
			PRIMES[PRIME_COUNT] = candidate;
			PRIME_COUNT         = PRIME_COUNT + 1;
		}
	}
}
