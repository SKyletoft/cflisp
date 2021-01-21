pub(crate) enum Register {
	A,
	X,
	Y,
}

pub(crate) enum Instruction {
	LDA(Addressing),
	LDX(Addressing),
	LDY(Addressing),
	ADDA(Addressing),
	ANDA(Addressing),
	ASL(Addressing),
	ASR(Addressing),
	EORA(Addressing),
}

pub(crate) enum Addressing {
	Data(isize),
	Adr(isize),
	SP(isize),
	Xn(isize),
	Yn(isize),
}
