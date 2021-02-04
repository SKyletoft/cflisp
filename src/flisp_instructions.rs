use crate::*;

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
	ORA(Addressing),
	EORA(Addressing),
	STA(Addressing),
	JMP(Addressing),
	BEQ(Addressing),
	LEASP(Addressing),
	PSHA,
	PULA,
	TSTA,
	COMA,
	AddToStack,      //LEA SP,-1
	RemoveFromStack, //LEA SP,1
	Label(String),
}

impl fmt::Display for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Instruction::LDA(a) => write!(f, "LDA     {}", *a),
			Instruction::LDX(a) => write!(f, "LDX     {}", *a),
			Instruction::LDY(a) => write!(f, "LDY     {}", *a),
			Instruction::ADDA(a) => write!(f, "ADDA    {}", *a),
			Instruction::SUBA(a) => write!(f, "SUBA    {}", *a),
			Instruction::ANDA(a) => write!(f, "ANDA    {}", *a),
			Instruction::ASL(a) => write!(f, "ASLA    {}", *a),
			Instruction::ASR(a) => write!(f, "ASRA    {}", *a),
			Instruction::ORA(a) => write!(f, "ORA     {}", *a),
			Instruction::EORA(a) => write!(f, "EORA    {}", *a),
			Instruction::STA(a) => write!(f, "STA     {}", *a),
			Instruction::JMP(a) => write!(f, "JMP     {}", *a),
			Instruction::BEQ(a) => write!(f, "BEQ     {}", *a),
			Instruction::LEASP(a) => write!(f, "LEASP   {}", *a),
			Instruction::PSHA => write!(f, "PSHA    "),
			Instruction::PULA => write!(f, "PULA    "),
			Instruction::TSTA => write!(f, "TSTA    "),
			Instruction::COMA => write!(f, "COMA    "),
			Instruction::AddToStack => write!(f, "LEASP   SP, -1"),
			Instruction::RemoveFromStack => write!(f, "LEASP   SP, 1"),
			Instruction::Label(l) => write!(f, "{}", l),
		}
	}
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
			| Instruction::ORA(a)
			| Instruction::STA(a) => Some(a.clone()),
			Instruction::AddToStack
			| Instruction::RemoveFromStack
			| Instruction::Label(_)
			| Instruction::COMA
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

impl fmt::Display for Addressing {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Addressing::Data(d) => write!(f, "%#{:02X}", *d as u8),
			Addressing::Adr(d) => write!(f, "%{:02X}", *d as u8),
			Addressing::SP(d) => write!(f, "SP, %{:02X}", *d as u8),
			Addressing::Xn(d) => write!(f, "X, %{:02X}", *d as u8),
			Addressing::Yn(d) => write!(f, "Y, %{:02X}", *d as u8),
			Addressing::Label(l) => write!(f, "{}", l),
		}
	}
}
