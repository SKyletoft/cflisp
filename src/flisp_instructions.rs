pub(crate) enum Register {
	A,
	X,
	Y,
}

pub(crate) type CommentedInstruction<'a> = (Instruction, &'a str);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Instruction {
	LDA(Addressing),
	LDX(Addressing),
	LDY(Addressing),
	ADDA(Addressing),
	SUBA(Addressing),
	ANDA(Addressing),
	ASL(Addressing),
	ASR(Addressing),
	EORA(Addressing),
	STA(Addressing),
	AddToStack,
	RemoveFromStack,
}

impl Instruction {
	pub(crate) fn address(self) -> Option<Addressing> {
		match self {
			Instruction::LDA(a)
			| Instruction::LDX(a)
			| Instruction::LDY(a)
			| Instruction::ADDA(a)
			| Instruction::ANDA(a)
			| Instruction::ASL(a)
			| Instruction::ASR(a)
			| Instruction::EORA(a)
			| Instruction::SUBA(a)
			| Instruction::STA(a) => Some(a),
			Instruction::AddToStack | Instruction::RemoveFromStack => None,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Addressing {
	Data(isize),
	Adr(isize),
	SP(usize),
	Xn(isize),
	Yn(isize),
}
