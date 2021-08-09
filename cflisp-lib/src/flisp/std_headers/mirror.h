// mirror.h

/// Mirrors the value pointed at by a in place
void mirror_in_place(int *a);

/// Mirrors the value pointed at by a in place
int mirror(int a) {
	mirror_in_place(&a);
	return a;
}
