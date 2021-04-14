use crate::*;

pub fn all_optimisations(element: &mut StatementElementStructless) -> Result<(), ParseError> {
	const_eval(element);
	fast_mul(element);
	Ok(())
}

pub(crate) fn fast_mul(elem: &mut StatementElementStructless) {
	if let StatementElementStructless::Mul { lhs, rhs } = elem {
		match (lhs.as_ref(), rhs.as_ref()) {
			(StatementElementStructless::Num(a), StatementElementStructless::Num(b)) => {
				*elem = StatementElementStructless::Num(*a * *b);
			}
			(StatementElementStructless::Num(a), b) | (b, StatementElementStructless::Num(a)) => {
				let a = *a as usize;
				let mut inner = b.clone();
				let compiler_word_size = std::usize::MAX.count_ones();
				for bit in 0..compiler_word_size {
					let set_bit = 1 << bit;
					if a & set_bit == set_bit {
						inner = StatementElementStructless::Add {
							lhs: Box::new(inner),
							rhs: Box::new(StatementElementStructless::LShift {
								lhs: Box::new(b.clone()),
								rhs: Box::new(StatementElementStructless::Num(bit as isize)),
							}),
						};
					}
				}
				*elem = inner;
			}
			_ => {}
		}
	}
}

pub(crate) fn const_eval<'a>(
	elem: &mut StatementElementStructless<'a>,
) -> Option<StatementElementStructless<'a>> {
	let maybe_get_nums = |lhs: &mut Box<StatementElementStructless<'a>>,
	                      rhs: &mut Box<StatementElementStructless<'a>>|
	 -> Option<(isize, isize)> {
		let lhs = const_eval(lhs.as_mut());
		let rhs = const_eval(rhs.as_mut());
		if let (
			Some(StatementElementStructless::Num(a)),
			Some(StatementElementStructless::Num(b)),
		) = (lhs, rhs)
		{
			Some((a, b))
		} else {
			None
		}
	};
	let maybe_get_bools = |lhs: &mut Box<StatementElementStructless<'a>>,
	                       rhs: &mut Box<StatementElementStructless<'a>>|
	 -> Option<(bool, bool)> {
		let lhs = const_eval(lhs.as_mut());
		let rhs = const_eval(rhs.as_mut());
		if let (
			Some(StatementElementStructless::Bool(a)),
			Some(StatementElementStructless::Bool(b)),
		) = (lhs, rhs)
		{
			Some((a, b))
		} else {
			None
		}
	};

	let this = match elem {
		StatementElementStructless::Add { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a + b))
		}
		StatementElementStructless::Sub { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a - b))
		}
		StatementElementStructless::Mul { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a * b))
		}
		StatementElementStructless::Div { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a / b))
		}
		StatementElementStructless::Mod { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a % b))
		}
		StatementElementStructless::LShift { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a << b))
		}
		StatementElementStructless::RShift { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Num(a >> b))
		}
		StatementElementStructless::GreaterThan { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a > b))
		}
		StatementElementStructless::LessThan { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a < b))
		}
		StatementElementStructless::GreaterThanEqual { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a >= b))
		}
		StatementElementStructless::LessThanEqual { lhs, rhs } => {
			maybe_get_nums(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a <= b))
		}
		StatementElementStructless::And { lhs, rhs } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StatementElementStructless::Num(a & b))
			.or_else(|| {
				maybe_get_bools(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a && b))
			}),
		StatementElementStructless::Or { lhs, rhs } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StatementElementStructless::Num(a | b))
			.or_else(|| {
				maybe_get_bools(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a || b))
			}),
		StatementElementStructless::Cmp { lhs, rhs } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StatementElementStructless::Bool(a == b))
			.or_else(|| {
				maybe_get_bools(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a == b))
			}),
		StatementElementStructless::NotCmp { lhs, rhs } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StatementElementStructless::Bool(a != b))
			.or_else(|| {
				maybe_get_bools(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a != b))
			}),
		StatementElementStructless::Xor { lhs, rhs } => maybe_get_nums(lhs, rhs)
			.map(|(a, b)| StatementElementStructless::Num(a ^ b))
			.or_else(|| {
				maybe_get_bools(lhs, rhs).map(|(a, b)| StatementElementStructless::Bool(a ^ b))
			}),
		StatementElementStructless::Not(lhs) => match const_eval(lhs.as_mut()) {
			Some(StatementElementStructless::Num(a)) => Some(StatementElementStructless::Num(!a)),
			Some(StatementElementStructless::Bool(a)) => Some(StatementElementStructless::Bool(!a)),
			_ => None,
		},
		// ^ todo! Implement these when they've been split into bit and bool versions
		StatementElementStructless::FunctionCall { .. }
		| StatementElementStructless::Var(_)
		| StatementElementStructless::Char(_)
		| StatementElementStructless::Array(_)
		| StatementElementStructless::Deref(_)
		| StatementElementStructless::AdrOf(_) => {
			return None;
		}

		StatementElementStructless::Num(_) | StatementElementStructless::Bool(_) => {
			return Some(elem.clone());
		}
	};
	if let Some(this) = &this {
		*elem = this.clone();
	}
	this
}
