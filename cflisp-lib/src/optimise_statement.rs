use std::collections::HashMap;

use crate::*;

pub fn all_optimisations(element: &mut StructlessStatement) -> Result<(), ParseError> {
	const_eval(element);
	fast_mul(element);
	fast_div(element);
	fast_mod(element);
	Ok(())
}

pub(crate) fn const_prop<'a>(
	elem: &mut StructlessStatement<'a>,
	constants: &HashMap<String, StructlessStatement<'a>>,
) {
	match elem {
		StructlessStatement::Add { lhs, rhs, .. }
		| StructlessStatement::Sub { lhs, rhs, .. }
		| StructlessStatement::Mul { lhs, rhs, .. }
		| StructlessStatement::Div { lhs, rhs, .. }
		| StructlessStatement::Mod { lhs, rhs, .. }
		| StructlessStatement::LShift { lhs, rhs, .. }
		| StructlessStatement::RShift { lhs, rhs, .. }
		| StructlessStatement::And { lhs, rhs, .. }
		| StructlessStatement::Or { lhs, rhs, .. }
		| StructlessStatement::Xor { lhs, rhs, .. }
		| StructlessStatement::GreaterThan { lhs, rhs, .. }
		| StructlessStatement::LessThan { lhs, rhs, .. }
		| StructlessStatement::GreaterThanEqual { lhs, rhs, .. }
		| StructlessStatement::LessThanEqual { lhs, rhs, .. }
		| StructlessStatement::Cmp { lhs, rhs, .. }
		| StructlessStatement::NotCmp { lhs, rhs, .. } => {
			const_prop(lhs, constants);
			const_prop(rhs, constants);
		}
		StructlessStatement::FunctionCall { parametres, .. } => {
			for param in parametres.iter_mut() {
				const_prop(param, constants);
			}
		}
		StructlessStatement::Var(name) => {
			let name: &str = name;
			if let Some(val) = constants.get(name) {
				*elem = val.clone();
			}
		}
		_ => {}
	}
}

pub(crate) fn fast_mul(elem: &mut StructlessStatement) {
	if let StructlessStatement::Mul {
		lhs,
		rhs,
		signedness,
	} = elem
	{
		match (lhs.as_ref(), rhs.as_ref()) {
			(StructlessStatement::Num(a), StructlessStatement::Num(b)) => {
				*elem = StructlessStatement::Num(*a * *b);
			}
			(StructlessStatement::Num(a), b) | (b, StructlessStatement::Num(a)) => {
				let a = a.val as usize;
				let mut inner = b.clone();
				let compiler_word_size = std::usize::MAX.count_ones();
				for bit in 1..compiler_word_size {
					let set_bit = 1 << bit;
					if a & set_bit == set_bit {
						inner = StructlessStatement::Add {
							lhs: Box::new(inner),
							rhs: Box::new(StructlessStatement::LShift {
								lhs: Box::new(b.clone()),
								rhs: Box::new(StructlessStatement::Num((bit as isize).into())),
								signedness: *signedness,
							}),
							signedness: *signedness,
						};
					}
				}
				*elem = inner;
			}
			_ => {}
		}
	}
}

pub(crate) fn fast_div(elem: &mut StructlessStatement) {
	if let StructlessStatement::Div {
		lhs,
		rhs,
		signedness,
	} = elem
	{
		match (lhs.as_ref(), rhs.as_ref()) {
			(StructlessStatement::Num(a), StructlessStatement::Num(b)) => {
				if b.val != 0 {
					*elem = StructlessStatement::Num(*a / *b);
				}
			}
			(a, StructlessStatement::Num(b)) => {
				// = is a multiple of two
				let b = b.val;
				if b.count_ones() == 1 && b >= 0 {
					let mut shifts = -1;
					let mut b_copy = b as usize;
					while b_copy != 0 {
						b_copy >>= 1;
						shifts += 1;
					}
					*elem = StructlessStatement::RShift {
						lhs: Box::new(a.clone()),
						rhs: Box::new(StructlessStatement::Num(shifts.into())),
						signedness: *signedness,
					};
				}
			}
			_ => {}
		}
	}
}

pub(crate) fn fast_mod(elem: &mut StructlessStatement) {
	if let StructlessStatement::Mod {
		lhs,
		rhs,
		signedness,
	} = elem
	{
		match (lhs.as_ref(), rhs.as_ref()) {
			(StructlessStatement::Num(a), StructlessStatement::Num(b)) => {
				*elem = StructlessStatement::Num(*a / *b);
			}
			(a, StructlessStatement::Num(b)) => {
				// = is a multiple of two
				if b.val.count_ones() == 1 && b.val >= 0 {
					let mask = b.val - 1;
					*elem = StructlessStatement::And {
						lhs: Box::new(a.clone()),
						rhs: Box::new(StructlessStatement::Num(mask.into())),
						signedness: *signedness,
					};
				}
			}
			_ => {}
		}
	}
}

pub(crate) fn const_eval<'a>(
	elem: &mut StructlessStatement<'a>,
) -> Option<StructlessStatement<'a>> {
	let maybe_get_nums = |lhs: &mut Box<StructlessStatement<'a>>,
	                      rhs: &mut Box<StructlessStatement<'a>>|
	 -> Option<(Number, Number)> {
		let lhs = const_eval(lhs.as_mut());
		let rhs = const_eval(rhs.as_mut());
		if let (Some(StructlessStatement::Num(a)), Some(StructlessStatement::Num(b))) = (lhs, rhs) {
			Some((a, b))
		} else {
			None
		}
	};
	let maybe_get_bools = |lhs: &mut Box<StructlessStatement<'a>>,
	                       rhs: &mut Box<StructlessStatement<'a>>|
	 -> Option<(bool, bool)> {
		let lhs = const_eval(lhs.as_mut());
		let rhs = const_eval(rhs.as_mut());
		if let (Some(StructlessStatement::Bool(a)), Some(StructlessStatement::Bool(b))) = (lhs, rhs)
		{
			Some((a, b))
		} else {
			None
		}
	};

	let this = match elem {
		StructlessStatement::Add { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a + b))
		}
		StructlessStatement::Sub { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a - b))
		}
		StructlessStatement::Mul { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a * b))
		}
		StructlessStatement::Div { lhs, rhs, .. }
			if !matches!(rhs.as_ref(), &StructlessStatement::Num(Number::ZERO)) =>
		{
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a / b))
		}
		StructlessStatement::Mod { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a % b))
		}
		StructlessStatement::LShift { lhs, rhs, .. } => {
			if !matches!(rhs.as_ref(), &StructlessStatement::Num(n) if n < Number::ZERO) {
				maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a << b))
			} else {
				None
			}
		}
		StructlessStatement::RShift { lhs, rhs, .. } => {
			if !matches!(rhs.as_ref(), &StructlessStatement::Num(n) if n < Number::ZERO) {
				maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Num(a >> b))
			} else {
				None
			}
		}
		StructlessStatement::GreaterThan { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a > b))
		}
		StructlessStatement::LessThan { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a < b))
		}
		StructlessStatement::GreaterThanEqual { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a >= b))
		}
		StructlessStatement::LessThanEqual { lhs, rhs, .. } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a <= b))
		}
		StructlessStatement::And { lhs, rhs, .. } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StructlessStatement::Num(a & b))
			.or_else(|| maybe_get_bools(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a && b))),
		StructlessStatement::Or { lhs, rhs, .. } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StructlessStatement::Num(a | b))
			.or_else(|| maybe_get_bools(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a || b))),
		StructlessStatement::Cmp { lhs, rhs, .. } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StructlessStatement::Bool(a == b))
			.or_else(|| maybe_get_bools(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a == b))),
		StructlessStatement::NotCmp { lhs, rhs, .. } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StructlessStatement::Bool(a != b))
			.or_else(|| maybe_get_bools(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a != b))),
		StructlessStatement::Xor { lhs, rhs, .. } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StructlessStatement::Num(a ^ b))
			.or_else(|| maybe_get_bools(lhs, rhs).map(|(a, b)| StructlessStatement::Bool(a ^ b))),
		StructlessStatement::Not(lhs) => match const_eval(lhs.as_mut()) {
			Some(StructlessStatement::Num(a)) => Some(StructlessStatement::Num(!a)),
			Some(StructlessStatement::Bool(a)) => Some(StructlessStatement::Bool(!a)),
			_ => None,
		},

		StructlessStatement::Num(_) | StructlessStatement::Bool(_) => {
			return Some(elem.clone());
		}

		_ => {
			return None;
		}
	};
	if let Some(this) = &this {
		*elem = this.clone();
	}
	this
}
