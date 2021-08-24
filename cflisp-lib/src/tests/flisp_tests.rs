#![allow(unused_imports)]
use super::super::*;

#[test]
fn reduce_reserves() {
	//let mut case_1 =
}

#[test]
fn signed_long_gt_logic() {
	fn test_algorithm(lhs: i16, rhs: i16) -> bool {
		let [l_lo_u, l_hi_u] = lhs.to_le_bytes();
		let [r_lo_u, r_hi_u] = rhs.to_le_bytes();
		let l_hi_i = l_hi_u as i8;
		let r_hi_i = r_hi_u as i8;
		if l_hi_i == r_hi_i {
			l_lo_u > r_lo_u
		} else {
			l_hi_i > r_hi_i
		}
	}

	let range = (i16::MIN as i32)..=(i16::MAX as i32);

	for lhs in range.clone() {
		for rhs in range.clone() {
			let lhs = lhs as i16;
			let rhs = rhs as i16;
			assert_eq!(
				lhs > rhs,
				test_algorithm(lhs, rhs),
				"{} (0x{0:X}) {} (0x{1:X})",
				lhs,
				rhs
			);
		}
	}
}
