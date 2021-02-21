use crate::*;

// Replace &'a str with a Cow<String>?
///`(Instruction, Option<&'a str>)`
///
/// The comment should have the same lifetime as the input source code (or `&'static`)
pub(crate) type CommentedInstruction<'a> = (Instruction, Option<&'a str>);

///A flisp instruction. Usually appears as the first half of a `CommentedInstruction` tuple.
/// Can also be a label or FCB assembler directive.
/// Naming scheme follows flisp rather than Rust naming conventions
#[allow(clippy::upper_case_acronyms, dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instruction {
	LDA(Addressing),
	STA(Addressing),
	LDX(Addressing),
	LDY(Addressing),
	LDSP(Addressing),
	STSP(Addressing),
	LEASP(Addressing),

	ADDA(Addressing),
	SUBA(Addressing),
	ANDA(Addressing),
	ORA(Addressing),
	EORA(Addressing),
	CMPA(Addressing),
	INCA,
	DECA,

	TSTA,
	COMA,
	LSLA,
	LSRA,

	JMP(Addressing),
	BNE(Addressing),
	BEQ(Addressing),
	BGE(Addressing),
	BLT(Addressing),

	INC(Addressing),
	DEC(Addressing),
	LSL(Addressing),
	LSR(Addressing),

	PSHA,
	PULA,
	RTS,
	JSR(Addressing),
	Label(String),
	FCB(Vec<isize>),
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
			Instruction::ORA(a) => write!(f, "\tORA\t{}", *a),
			Instruction::EORA(a) => write!(f, "\tEORA\t{}", *a),
			Instruction::STA(a) => write!(f, "\tSTA\t{}", *a),
			Instruction::STSP(a) => write!(f, "\tSTSP\t{}", *a),
			Instruction::JMP(a) => write!(f, "\tJMP\t{}", *a),
			Instruction::BNE(a) => write!(f, "\tBNE\t{}", *a),
			Instruction::BEQ(a) => write!(f, "\tBEQ\t{}", *a),
			Instruction::BGE(a) => write!(f, "\tBGE\t{}", *a),
			Instruction::BLT(a) => write!(f, "\tBLT\t{}", *a),
			Instruction::LEASP(a) => write!(f, "\tLEASP\t{}", *a),
			Instruction::JSR(a) => write!(f, "\tJSR\t{}", *a),
			Instruction::CMPA(a) => write!(f, "\tCMPA\t{}", *a),
			Instruction::INC(a) => write!(f, "\tINC\t{}", *a),
			Instruction::DEC(a) => write!(f, "\tDEC\t{}", *a),
			Instruction::LSL(a) => write!(f, "\tLSL\t{}", *a),
			Instruction::LSR(a) => write!(f, "\tLSR\t{}", *a),
			Instruction::LSLA => write!(f, "\tLSLA\t"),
			Instruction::LSRA => write!(f, "\tLSRA\t"),
			Instruction::INCA => write!(f, "\tINCA\t"),
			Instruction::DECA => write!(f, "\tDECA\t"),
			Instruction::PSHA => write!(f, "\tPSHA\t"),
			Instruction::PULA => write!(f, "\tPULA\t"),
			Instruction::TSTA => write!(f, "\tTSTA\t"),
			Instruction::COMA => write!(f, "\tCOMA\t"),
			Instruction::RTS => write!(f, "\tRTS\t"),
			Instruction::Label(l) => write!(f, "{}", l),
			Instruction::FCB(bytes) => {
				//Maybe insert a newline every eight values?
				write!(f, "\tFCB\t")?;
				for &val in bytes.iter().take(bytes.len() - 1) {
					write!(f, "{:03},", val % 256)?;
				}
				write!(f, "{:03}", bytes[bytes.len() - 1] % 256)
			}
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
			Instruction::ORA(a) => write!(f, "\tORA\t{:X}", *a),
			Instruction::EORA(a) => write!(f, "\tEORA\t{:X}", *a),
			Instruction::STA(a) => write!(f, "\tSTA\t{:X}", *a),
			Instruction::STSP(a) => write!(f, "\tSTSP\t{:X}", *a),
			Instruction::JMP(a) => write!(f, "\tJMP\t{:X}", *a),
			Instruction::BNE(a) => write!(f, "\tBNE\t{:X}", *a),
			Instruction::BEQ(a) => write!(f, "\tBEQ\t{:X}", *a),
			Instruction::BGE(a) => write!(f, "\tBGE\t{:X}", *a),
			Instruction::BLT(a) => write!(f, "\tBLT\t{:X}", *a),
			Instruction::LEASP(a) => write!(f, "\tLEASP\t{:X}", *a),
			Instruction::JSR(a) => write!(f, "\tJSR\t{:X}", *a),
			Instruction::CMPA(a) => write!(f, "\tCMPA\t{:X}", *a),
			Instruction::INC(a) => write!(f, "\tINC\t{:X}", *a),
			Instruction::DEC(a) => write!(f, "\tDEC\t{:X}", *a),
			Instruction::LSL(a) => write!(f, "\rLSL\t{:X}", *a),
			Instruction::LSR(a) => write!(f, "\tLSR\t{:X}", *a),
			Instruction::LSLA => write!(f, "\tLSLA\t"),
			Instruction::LSRA => write!(f, "\tLSRA\t"),
			Instruction::INCA => write!(f, "\tINCA\t"),
			Instruction::DECA => write!(f, "\tDECA\t"),
			Instruction::PSHA => write!(f, "\tPSHA\t"),
			Instruction::PULA => write!(f, "\tPULA\t"),
			Instruction::TSTA => write!(f, "\tTSTA\t"),
			Instruction::COMA => write!(f, "\tCOMA\t"),
			Instruction::RTS => write!(f, "\tRTS\t"),
			Instruction::Label(l) => write!(f, "{}", l),
			Instruction::FCB(bytes) => {
				write!(f, "\tFCB\t")?;
				for &val in bytes.iter().take(bytes.len() - 1) {
					write!(f, "${:02X},", val % 256)?;
				}
				write!(f, "${:02X}", bytes[bytes.len() - 1])
			}
		}
	}
}

impl Instruction {
	///The size in bytes that it will take up in the final compiled binary
	pub(crate) fn size(&self) -> usize {
		match self {
			Instruction::LDA(a)
			| Instruction::LDX(a)
			| Instruction::LDY(a)
			| Instruction::LDSP(a)
			| Instruction::ADDA(a)
			| Instruction::SUBA(a)
			| Instruction::ANDA(a)
			| Instruction::ORA(a)
			| Instruction::EORA(a)
			| Instruction::STA(a)
			| Instruction::STSP(a)
			| Instruction::JMP(a)
			| Instruction::BNE(a)
			| Instruction::BEQ(a)
			| Instruction::BGE(a)
			| Instruction::BLT(a)
			| Instruction::JSR(a)
			| Instruction::CMPA(a)
			| Instruction::INC(a)
			| Instruction::DEC(a)
			| Instruction::LSL(a)
			| Instruction::LSR(a)
			| Instruction::LEASP(a) => a.size() + 1,
			Instruction::PSHA
			| Instruction::PULA
			| Instruction::TSTA
			| Instruction::INCA
			| Instruction::DECA
			| Instruction::COMA
			| Instruction::LSLA
			| Instruction::LSRA
			| Instruction::RTS => 1,
			Instruction::FCB(n) => n.len(),
			Instruction::Label(_) => 0,
		}
	}

	///Gets the inner Addressing enum. Doesn't work on Labels as they contain a `String` directly
	pub(crate) fn get_adr_mut(&mut self) -> Option<&mut Addressing> {
		match self {
			Instruction::LDA(a)
			| Instruction::LDX(a)
			| Instruction::LDY(a)
			| Instruction::LDSP(a)
			| Instruction::ADDA(a)
			| Instruction::SUBA(a)
			| Instruction::ANDA(a)
			| Instruction::ORA(a)
			| Instruction::EORA(a)
			| Instruction::STA(a)
			| Instruction::STSP(a)
			| Instruction::JMP(a)
			| Instruction::BNE(a)
			| Instruction::BEQ(a)
			| Instruction::BGE(a)
			| Instruction::BLT(a)
			| Instruction::JSR(a)
			| Instruction::CMPA(a)
			| Instruction::INC(a)
			| Instruction::DEC(a)
			| Instruction::LSL(a)
			| Instruction::LSR(a)
			| Instruction::LEASP(a) => Some(a),
			Instruction::PSHA
			| Instruction::PULA
			| Instruction::TSTA
			| Instruction::INCA
			| Instruction::DECA
			| Instruction::COMA
			| Instruction::LSLA
			| Instruction::LSRA
			| Instruction::RTS
			| Instruction::FCB(_)
			| Instruction::Label(_) => None,
		}
	}

	///Gets the inner Addressing enum. Doesn't work on Labels as they contain a `String` directly
	pub(crate) fn get_adr(&self) -> Option<&Addressing> {
		match self {
			Instruction::LDA(a)
			| Instruction::LDX(a)
			| Instruction::LDY(a)
			| Instruction::LDSP(a)
			| Instruction::ADDA(a)
			| Instruction::SUBA(a)
			| Instruction::ANDA(a)
			| Instruction::ORA(a)
			| Instruction::EORA(a)
			| Instruction::STA(a)
			| Instruction::STSP(a)
			| Instruction::JMP(a)
			| Instruction::BNE(a)
			| Instruction::BEQ(a)
			| Instruction::BGE(a)
			| Instruction::BLT(a)
			| Instruction::JSR(a)
			| Instruction::CMPA(a)
			| Instruction::INC(a)
			| Instruction::DEC(a)
			| Instruction::LSL(a)
			| Instruction::LSR(a)
			| Instruction::LEASP(a) => Some(a),
			Instruction::PSHA
			| Instruction::PULA
			| Instruction::TSTA
			| Instruction::INCA
			| Instruction::DECA
			| Instruction::COMA
			| Instruction::LSLA
			| Instruction::LSRA
			| Instruction::RTS
			| Instruction::FCB(_)
			| Instruction::Label(_) => None,
		}
	}
}

///The different addressing modes for the instructions. Unfortunately not every addressing mode
/// works with every instruction, but as we only use one `Addressing` enum type the type system won't
/// stop you from misusing this.
#[allow(clippy::upper_case_acronyms, dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Addressing {
	Data(isize),
	Adr(isize),
	SP(isize),
	Xn(isize),
	Yn(isize),
	Label(String),
	DataLabel(String),
	AX,
	AY,
}

impl Addressing {
	///The size in bytes that it will take up in the final compiled binary
	fn size(&self) -> usize {
		match self {
			Addressing::Data(_)
			| Addressing::Adr(_)
			| Addressing::SP(_)
			| Addressing::Xn(_)
			| Addressing::Yn(_)
			| Addressing::DataLabel(_) => 1,
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
			Addressing::DataLabel(l) => write!(f, "#{}", l),
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
			Addressing::DataLabel(l) => write!(f, "#{}", l),
			Addressing::AX => write!(f, "A,X"),
			Addressing::AY => write!(f, "A,Y"),
		}
	}
}
