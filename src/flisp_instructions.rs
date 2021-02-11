use crate::*;

pub(crate) type CommentedInstruction<'a> = (Instruction, Option<&'a str>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instruction {
	LDA(Addressing),
	LDX(Addressing),
	LDY(Addressing),
	LDSP(Addressing),
	ADDA(Addressing),
	SUBA(Addressing),
	ANDA(Addressing),
	ROLA(Addressing),
	RORA(Addressing),
	ORA(Addressing),
	EORA(Addressing),
	STA(Addressing),
	JMP(Addressing),
	BNE(Addressing),
	BEQ(Addressing),
	BGT(Addressing),
	BLT(Addressing),
	LEASP(Addressing),
	CMPA(Addressing),
	INC(Addressing),
	INCA,
	PSHA,
	PULA,
	TSTA,
	COMA,
	RTS,
	JSR(Addressing),
	Label(String),
}

impl fmt::Display for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Instruction::LDA(a) => write!(f, "\tLDA\t{}", *a),
			Instruction::LDX(a) => write!(f, "\tLDX\t{}", *a),
			Instruction::LDY(a) => write!(f, "\tLDY\t{}", *a),
			Instruction::LDSP(a) => write!(f, "\tLDSP\t{}", *a),
			Instruction::ADDA(a) => write!(f, "\tADDA\t{}", *a),
			Instruction::SUBA(a) => write!(f, "\tSUBA\t{}", *a),
			Instruction::ANDA(a) => write!(f, "\tANDA\t{}", *a),
			Instruction::ROLA(a) => write!(f, "\tROLA\t{}", *a),
			Instruction::RORA(a) => write!(f, "\tRORA\t{}", *a),
			Instruction::ORA(a) => write!(f, "\tORA\t{}", *a),
			Instruction::EORA(a) => write!(f, "\tEORA\t{}", *a),
			Instruction::STA(a) => write!(f, "\tSTA\t{}", *a),
			Instruction::JMP(a) => write!(f, "\tJMP\t{}", *a),
			Instruction::BNE(a) => write!(f, "\tBNE\t{}", *a),
			Instruction::BEQ(a) => write!(f, "\tBEQ\t{}", *a),
			Instruction::BGT(a) => write!(f, "\tBGT\t{}", *a),
			Instruction::BLT(a) => write!(f, "\tBLT\t{}", *a),
			Instruction::LEASP(a) => write!(f, "\tLEASP\t{}", *a),
			Instruction::JSR(a) => write!(f, "\tJSR\t{}", *a),
			Instruction::CMPA(a) => write!(f, "\tCMPA\t{}", *a),
			Instruction::INC(a) => write!(f, "\tINC\t{}", *a),
			Instruction::INCA => write!(f, "\tINCA\t"),
			Instruction::PSHA => write!(f, "\tPSHA\t"),
			Instruction::PULA => write!(f, "\tPULA\t"),
			Instruction::TSTA => write!(f, "\tTSTA\t"),
			Instruction::COMA => write!(f, "\tCOMA\t"),
			Instruction::RTS => write!(f, "\tRTS\t"),
			Instruction::Label(l) => write!(f, "{}", l),
		}
	}
}
impl fmt::UpperHex for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Instruction::LDA(a) => write!(f, "\tLDA\t{:X}", *a),
			Instruction::LDX(a) => write!(f, "\tLDX\t{:X}", *a),
			Instruction::LDY(a) => write!(f, "\tLDY\t{:X}", *a),
			Instruction::LDSP(a) => write!(f, "\tLDSP\t{:X}", *a),
			Instruction::ADDA(a) => write!(f, "\tADDA\t{:X}", *a),
			Instruction::SUBA(a) => write!(f, "\tSUBA\t{:X}", *a),
			Instruction::ANDA(a) => write!(f, "\tANDA\t{:X}", *a),
			Instruction::ROLA(a) => write!(f, "\tROLA\t{:X}", *a),
			Instruction::RORA(a) => write!(f, "\tRORA\t{:X}", *a),
			Instruction::ORA(a) => write!(f, "\tORA\t{:X}", *a),
			Instruction::EORA(a) => write!(f, "\tEORA\t{:X}", *a),
			Instruction::STA(a) => write!(f, "\tSTA\t{:X}", *a),
			Instruction::JMP(a) => write!(f, "\tJMP\t{:X}", *a),
			Instruction::BNE(a) => write!(f, "\tBNE\t{:X}", *a),
			Instruction::BEQ(a) => write!(f, "\tBEQ\t{:X}", *a),
			Instruction::BGT(a) => write!(f, "\tBGT\t{:X}", *a),
			Instruction::BLT(a) => write!(f, "\tBLT\t{:X}", *a),
			Instruction::LEASP(a) => write!(f, "\tLEASP\t{:X}", *a),
			Instruction::JSR(a) => write!(f, "\tJSR\t{:X}", *a),
			Instruction::CMPA(a) => write!(f, "\tCMPA\t{:X}", *a),
			Instruction::INC(a) => write!(f, "\tINC\t{:X}", *a),
			Instruction::INCA => write!(f, "\tINCA\t"),
			Instruction::PSHA => write!(f, "\tPSHA\t"),
			Instruction::PULA => write!(f, "\tPULA\t"),
			Instruction::TSTA => write!(f, "\tTSTA\t"),
			Instruction::COMA => write!(f, "\tCOMA\t"),
			Instruction::RTS => write!(f, "\tRTS\t"),
			Instruction::Label(l) => write!(f, "{}", l),
		}
	}
}

impl Instruction {
	pub(crate) fn size(&self) -> usize {
		match self {
			Instruction::LDA(a)
			| Instruction::LDX(a)
			| Instruction::LDY(a)
			| Instruction::LDSP(a)
			| Instruction::ADDA(a)
			| Instruction::SUBA(a)
			| Instruction::ANDA(a)
			| Instruction::ROLA(a)
			| Instruction::RORA(a)
			| Instruction::ORA(a)
			| Instruction::EORA(a)
			| Instruction::STA(a)
			| Instruction::JMP(a)
			| Instruction::BNE(a)
			| Instruction::BEQ(a)
			| Instruction::BGT(a)
			| Instruction::BLT(a)
			| Instruction::JSR(a)
			| Instruction::CMPA(a)
			| Instruction::INC(a)
			| Instruction::LEASP(a) => a.size(),
			Instruction::PSHA
			| Instruction::PULA
			| Instruction::TSTA
			| Instruction::INCA
			| Instruction::COMA
			| Instruction::RTS => 1,
			Instruction::Label(_) => 0,
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
	AX,
	AY,
}

impl Addressing {
	fn size(&self) -> usize {
		match self {
			Addressing::Data(_)
			| Addressing::Adr(_)
			| Addressing::SP(_)
			| Addressing::Xn(_)
			| Addressing::Yn(_) => 1,
			Addressing::Label(_) | Addressing::AX | Addressing::AY => 0,
		}
	}
}

impl fmt::Display for Addressing {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			// mod 256 to allow -255 to 255 instead of -127 to 127
			Addressing::Data(d) => write!(f, "#{:03}", *d % 256),
			Addressing::Adr(d) => write!(f, "{:03}", *d % 256),
			Addressing::SP(d) => write!(f, "{:03},SP", *d % 256),
			Addressing::Xn(d) => write!(f, "{:03},X", *d % 256),
			Addressing::Yn(d) => write!(f, "{:03},Y", *d % 256),
			Addressing::Label(l) => write!(f, "{}", l),
			Addressing::AX => write!(f, "A,X"),
			Addressing::AY => write!(f, "A,Y"),
		}
	}
}

impl fmt::UpperHex for Addressing {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Addressing::Data(d) => write!(f, "#${:02X}", *d as u8),
			Addressing::Adr(d) => write!(f, "${:02X}", *d as u8),
			Addressing::SP(d) => write!(f, "${:02X},SP", *d as u8),
			Addressing::Xn(d) => write!(f, "${:02X},X", *d as u8),
			Addressing::Yn(d) => write!(f, "${:02X},Y", *d as u8),
			Addressing::Label(l) => write!(f, "{}", l),
			Addressing::AX => write!(f, "A,X"),
			Addressing::AY => write!(f, "A,Y"),
		}
	}
}
