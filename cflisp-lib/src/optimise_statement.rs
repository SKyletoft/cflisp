use crate::*;

pub fn all_optimisations(elements: &mut Vec<StatementElement>) -> Result<(), ParseError> {
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
