pub(crate) enum Register {
	A,
	X,
	Y,
}

pub(crate) type CommentedInstruction<'a> = (Instruction, Option<&'a str>);

#[derive(Debug, Clone, PartialEq, Eq)]
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
	JMP(Addressing),
	BEQ(Addressing),
	LEASP(Addressing),
	PSHA,
	PULA,
	TSTA,
	AddToStack,      //LEA SP,-1
	RemoveFromStack, //LEA SP,1
	Label(String),
}

impl Instruction {
	pub(crate) fn address(&self) -> Option<Addressing> {
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
			| Instruction::JMP(a)
			| Instruction::BEQ(a)
			| Instruction::LEASP(a)
			| Instruction::STA(a) => Some(a.clone()),
			Instruction::AddToStack
			| Instruction::RemoveFromStack
			| Instruction::Label(_)
			| Instruction::PSHA
			| Instruction::PULA
			| Instruction::TSTA => None,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Addressing {
	Data(isize),
	Adr(isize),
	SP(isize),
	Xn(isize),
	Yn(isize),
	Label(String),
}
