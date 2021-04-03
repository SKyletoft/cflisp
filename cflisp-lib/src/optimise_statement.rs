use crate::*;

pub fn all_optimisations(elements: &mut Vec<StatementElement>) -> Result<(), ParseError> {
	elements.iter_mut().for_each(fast_mul);
	for element in elements.iter_mut() {
		const_eval(element);
	}
	Ok(())
}

pub(crate) fn fast_mul(elem: &mut StatementElement) {
	if let StatementElement::Mul { lhs, rhs } = elem {
		match (lhs.as_ref(), rhs.as_ref()) {
			(StatementElement::Num(a), StatementElement::Num(b)) => {
				*elem = StatementElement::Num(*a * *b);
			}
			(StatementElement::Num(a), b) | (b, StatementElement::Num(a)) => {
				let a = *a;
				let mut inner = b.clone();
				let compiler_word_size = std::usize::MAX.count_ones();
				for bit in 1..compiler_word_size {
					let set_bit = 1 << bit;
					if a as usize & set_bit == set_bit {
						inner = StatementElement::Add {
							lhs: Box::new(inner),
							rhs: Box::new(StatementElement::LShift {
								lhs: Box::new(b.clone()),
								rhs: Box::new(StatementElement::Num(bit as isize)),
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

pub(crate) fn const_eval<'a>(elem: &mut StatementElement<'a>) -> Option<StatementElement<'a>> {
	let get_nums = |lhs: &mut Box<StatementElement<'a>>,
	                rhs: &mut Box<StatementElement<'a>>|
	 -> Option<(isize, isize)> {
		let lhs = const_eval(lhs.as_mut());
		let rhs = const_eval(rhs.as_mut());
		if let (Some(StatementElement::Num(a)), Some(StatementElement::Num(b))) = (lhs, rhs) {
			Some((a, b))
		} else {
			None
		}
	};

	let this = match elem {
		StatementElement::Add { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a + b)),
		StatementElement::Sub { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a - b)),
		StatementElement::Mul { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a * b)),
		StatementElement::Div { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a / b)),
		StatementElement::Mod { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a % b)),
		StatementElement::LShift { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a << b)),
		StatementElement::RShift { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Num(a >> b)),
		StatementElement::GreaterThan { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Bool(a > b)),
		StatementElement::LessThan { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Bool(a < b)),
		StatementElement::GreaterThanEqual { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Bool(a >= b)),
		StatementElement::LessThanEqual { lhs, rhs } =>
			get_nums(lhs, rhs).map(|(a,b)| StatementElement::Bool(a <= b)),
		StatementElement::Cmp { lhs, rhs } => {
			match (const_eval(lhs.as_mut()), const_eval(rhs.as_mut())) {
				(Some(StatementElement::Num(a)), Some(StatementElement::Num(b))) => {
					Some(StatementElement::Bool(a == b))
				}
				(Some(StatementElement::Bool(a)), Some(StatementElement::Bool(b))) => {
					Some(StatementElement::Bool(a == b))
				}
				_ => None
			}
		}
		StatementElement::NotCmp { lhs, rhs } => {
			match (const_eval(lhs.as_mut()), const_eval(rhs.as_mut())) {
				(Some(StatementElement::Num(a)), Some(StatementElement::Num(b))) => {
					Some(StatementElement::Bool(a != b))
				}
				(Some(StatementElement::Bool(a)), Some(StatementElement::Bool(b))) => {
					Some(StatementElement::Bool(a != b))
				}
				_ => None
			}
		}
		StatementElement::Xor { lhs, rhs } => {
			match (const_eval(lhs.as_mut()), const_eval(rhs.as_mut())) {
				(Some(StatementElement::Num(a)), Some(StatementElement::Num(b))) => {
					Some(StatementElement::Num(a ^ b))
				}
				(Some(StatementElement::Bool(a)), Some(StatementElement::Bool(b))) => {
					Some(StatementElement::Bool(a ^ b))
				}
				_ => None
			}
		}
		StatementElement::Not { lhs  } => {
			match const_eval(lhs.as_mut()) {
				Some(StatementElement::Num(a)) => {
					Some(StatementElement::Num(!a))
				}
				Some(StatementElement::Bool(a)) => {
					Some(StatementElement::Bool(!a))
				}
				_ => None
			}
		}

		StatementElement::And { .. }
		| StatementElement::Or { .. }
		// ^ todo! Implement these when they've been split into bit and bool versions
		| StatementElement::FunctionCall { .. }
		| StatementElement::Var(_)
		| StatementElement::Char(_)
		| StatementElement::Array(_)
		| StatementElement::Deref(_)
		| StatementElement::AdrOf(_)
		| StatementElement::FieldPointerAccess(_, _) => {
			return None;
		}

		StatementElement::Num(_) |
		StatementElement::Bool(_) => {
			return Some(elem.clone());
		}
	};
	if let Some(this) = &this {
		*elem = this.clone();
	}
	this
}
