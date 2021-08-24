#pragma once
// long.h

#include <flisp_imports.h>

#define ulong Ulong
#define long Long

// Unsigned long type (16 bits)
struct Ulong {
	uint hi;
	uint lo;
};
// Signed long type (16 bits)
struct Long {
	uint hi;
	uint lo;
};

// Implemented in assembly

Ulong ulong_add(Ulong lhs, Ulong rhs);
Ulong ulong_sub(Ulong lhs, Ulong rhs);
// Ulong ulong_mul(Ulong lhs, Ulong rhs);
// Ulong ulong_div(Ulong lhs, Ulong rhs);
// Ulong ulong_mod(Ulong lhs, Ulong rhs);
bool ulong_eq(Ulong lhs, Ulong rhs);

bool ulong_gt(Ulong lhs, Ulong rhs);
Long long_add(Long lhs, Long rhs);
Long long_sub(Long lhs, Long rhs);
// Long long_mul(Long lhs, Long rhs);
// Long long_div(Long lhs, Long rhs);
// Long long_mod(Long lhs, Long rhs);
bool long_eq(Long lhs, Long rhs);
bool long_gt(Long lhs, Long rhs);

// Wrappers around a single assembly version

bool ulong_lte(Ulong lhs, Ulong rhs) {
	return !ulong_gt(lhs, rhs);
}
bool ulong_lt(Ulong lhs, Ulong rhs) {
	return ulong_gt(rhs, lhs);
}
bool ulong_gte(Ulong lhs, Ulong rhs) {
	return !ulong_gt(rhs, lhs);
}
bool ulong_neq(Ulong lhs, Ulong rhs) {
	return !ulong_eq(lhs, rhs);
}
bool long_lte(Long lhs, Long rhs) {
	return !long_gt(lhs, rhs);
}
bool long_lt(Long lhs, Long rhs) {
	return long_gt(rhs, lhs);
}
bool long_gte(Long lhs, Long rhs) {
	return !long_gt(rhs, lhs);
}
bool long_neq(Long lhs, Long rhs) {
	return !long_eq(lhs, rhs);
}
