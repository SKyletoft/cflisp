use std::{borrow::Cow, fmt};

use crate::*;

//When try blocks are stablised: Convert to try block for clear error handling at call site
macro_rules! write_indented {
	($f:expr, $i:expr, $t:expr) => {
		DisplayWithIndent::fmt($t, $f, $i)?;
	};
	($f:expr, $i:expr, $a:expr, $($t:expr), +) => {
		DisplayWithIndent::fmt($a, $f, $i)?;
		write_indented!($f, $i, $($t), +);
	};
}

trait DisplayWithIndent {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result;
}

impl DisplayWithIndent for str {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		write!(f, "{}", self)
	}
}

impl DisplayWithIndent for Cow<'_, str> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		let s: &str = self;
		DisplayWithIndent::fmt(s, f, indent)
	}
}

impl DisplayWithIndent for usize {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		//write!(f, "[{} + {}]", self, indent)?;
		for _ in 0..(*self) {
			write!(f, "\t")?;
		}
		Ok(())
	}
}

impl<T: DisplayWithIndent + fmt::Display> DisplayWithIndent for (&[T], &str, usize) {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		let (tokens, separator, add_indent) = self;
		write_slice(tokens, f, separator, indent + add_indent)
	}
}

fn write_slice<T: DisplayWithIndent + fmt::Display>(
	tokens: &[T],
	f: &mut fmt::Formatter<'_>,
	separator: &str,
	indent: usize,
) -> fmt::Result {
	if let [start @ .., end] = tokens {
		for token in start.iter() {
			write_indented!(f, indent, &indent, token, separator);
		}
		write_indented!(f, indent, &indent, end);
	}
	Ok(())
}

impl DisplayWithIndent for StatementToken<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		use StatementToken::*;
		match self {
			Add => write!(f, "+"),
			BitAnd => write!(f, "&",),
			BitNot => write!(f, "~",),
			BitOr => write!(f, "|",),
			Bool(b) => write!(f, "{}", b),
			BoolAnd => write!(f, "&&",),
			BoolNot => write!(f, "!"),
			BoolOr => write!(f, "||"),
			Char(c) => write!(f, "{}", c),
			Cmp => write!(f, "=="),
			Div => write!(f, "/"),
			FieldAccess => write!(f, "."),
			FieldPointerAccess => write!(f, "->"),
			GreaterThan => write!(f, ">"),
			GreaterThanEqual => write!(f, ">="),
			LessThan => write!(f, "<"),
			LessThanEqual => write!(f, "<="),
			LShift => write!(f, "<<"),
			Mod => write!(f, "%"),
			Mul => write!(f, "*"),
			NotCmp => write!(f, "!="),
			Num(n) => write!(f, "{}", n),
			RShift => write!(f, ">>"),
			Sub => write!(f, "-"),
			Xor => write!(f, "^"),
			Parentheses(block) => {
				write_indented!(f, indent, "(", &(block.as_slice(), " ", 1), ")");
				Ok(())
			}
			ArrayAccess(block) => {
				write!(f, "[")?;
				write_slice(block, f, " ", indent + 1)?;
				write!(f, "]")
			}
			Ternary(block) => {
				write!(f, "? ")?;
				write_slice(block, f, " ", indent + 1)?;
				write!(f, " :")
			}
			Var(n) => write!(f, "{}", n),
			FunctionCall(n, args) => {
				write!(f, "{}(", n)?;
				if let [start @ .., end] = args.as_slice() {
					for item in start.iter() {
						write_slice(item, f, " ", indent + 1)?;
						write!(f, ", ")?;
					}
					write_slice(end, f, " ", indent + 1)?;
				}
				write!(f, ")")
			}
			Cast(typ) => write!(f, "({})", typ),
			Array(arr) => {
				write!(f, "{{")?;
				if let [start @ .., end] = arr.as_slice() {
					for item in start.iter() {
						write_slice(item, f, " ", indent + 1)?;
						write!(f, ", ")?;
					}
					write_slice(end, f, " ", indent + 1)?;
				}
				write!(f, "}}")
			}
		}
	}
}

impl DisplayWithIndent for Token<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		use Token::*;
		match self {
			Add => write!(f, "+"),
			AdrOf => write!(f, "&",),
			AlignAs => write!(f, "_Alignas",),
			AlignOf => write!(f, "_Alignof",),
			AnyCast => write!(f, "(any)",),
			Assign => write!(f, "=",),
			Atomic => write!(f, "_Atomic",),
			Auto => write!(f, "auto",),
			BitAnd => write!(f, "&",),
			BitNot => write!(f, "~",),
			BitOr => write!(f, "|",),
			Bool(b) => write!(f, "{}", b),
			BoolAnd => write!(f, "&&",),
			BoolCast => write!(f, "(bool)",),
			BoolNot => write!(f, "!"),
			BoolOr => write!(f, "||"),
			Break => write!(f, "break"),
			Case => write!(f, "case"),
			Char(c) => write!(f, "{}", c),
			CharCast => write!(f, "(char)"),
			Cmp => write!(f, "=="),
			Colon => write!(f, ":"),
			Comma => write!(f, ","),
			Complex => write!(f, "_Complex"),
			Const => write!(f, "const"),
			Continue => write!(f, "continue"),
			Decl(t) => write!(f, "{}", t),
			Default => write!(f, "default"),
			Deref => write!(f, "*"),
			Div => write!(f, "/"),
			Do => write!(f, "do"),
			Double => write!(f, "double"),
			Else => write!(f, "else"),
			Enum => write!(f, "enum"),
			Extern => write!(f, "extern"),
			FieldAccess => write!(f, "."),
			FieldPointerAccess => write!(f, "->"),
			Float => write!(f, "float"),
			For => write!(f, "for"),
			Generic => write!(f, "_Generic"),
			Goto => write!(f, "goto"),
			GreaterThan => write!(f, ">"),
			GreaterThanEqual => write!(f, ">="),
			If => write!(f, "if"),
			Imaginary => write!(f, "_Imaginary"),
			Inline => write!(f, "inline"),
			IntCast => write!(f, "(int)"),
			LessThan => write!(f, "<"),
			LessThanEqual => write!(f, "<="),
			Long => write!(f, "long"),
			LShift => write!(f, "<<"),
			Mod => write!(f, "%"),
			Mul => write!(f, "*"),
			Name(n) => write!(f, "{}", n),
			Namespace => write!(f, "namespace"),
			NamespaceSplitter => write!(f, "::"),
			NewLine => write!(f, ";"),
			NoReturn => write!(f, "_Noreturn"),
			NotCmp => write!(f, "!="),
			Num(n) => write!(f, "{}", n),
			Register => write!(f, "register"),
			Restrict => write!(f, "restrict"),
			Return => write!(f, "return"),
			RShift => write!(f, ">>"),
			Short => write!(f, "short"),
			Signed => write!(f, "signed"),
			SizeOf => write!(f, "sizeof"),
			Static => write!(f, "static"),
			StaticAssert => write!(f, "static_assert"),
			StringLiteral(s) => write!(f, "\"{}\"", s),
			Struct => write!(f, "struct"),
			Sub => write!(f, "-"),
			Switch => write!(f, "switch"),
			ThreadLocal => write!(f, "Thread_local"),
			TypeDef => write!(f, "typedef"),
			UintCast => write!(f, "(uint)"),
			Union => write!(f, "union"),
			Unsigned => write!(f, "unsigned"),
			Volatile => write!(f, "volatile"),
			While => write!(f, "while"),
			Xor => write!(f, "^"),
			Parentheses(block) => {
				write!(f, "(")?;
				write_slice(block, f, " ", 0)?;
				write!(f, ")")
			}
			Source(block) | Block(block) => {
				write!(f, "{{")?;
				write_slice(block, f, " ", 0)?;
				write!(f, "}}")
			}
			ArrayAccess(block) => {
				write!(f, "[")?;
				write_slice(block, f, " ", 0)?;
				write!(f, "]")
			}
			Ternary(block) => {
				write!(f, "? ")?;
				write_slice(block, f, " ", 0)?;
				write!(f, " :")
			}
		}
	}
}

impl DisplayWithIndent for LanguageElement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		match self {
			LanguageElement::VariableDeclaration {
				typ,
				name,
				is_static,
				is_const,
				is_volatile,
			} => {
				let scv: StaticConstVolatile = (is_static, is_const, is_volatile).into();
				write!(f, "{}{} {};", scv, typ, name)
			}
			LanguageElement::VariableAssignment { name, value } => {
				write!(f, "{} = {};", name, value)
			}
			LanguageElement::StructAssignment { name, value } => {
				write_indented!(f, 0, name, " = {", &(value.as_slice(), ", ", 1), "};");
				Ok(())
			}
			LanguageElement::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_static,
				is_const,
				is_volatile,
			} => {
				let scv: StaticConstVolatile = (is_static, is_const, is_volatile).into();
				write!(f, "{}{} {} = {};", scv, typ, name, value)
			}
			LanguageElement::StructDeclarationAssignment {
				typ,
				name,
				value,
				is_static,
				is_const,
				is_volatile,
			} => {
				let scv: StaticConstVolatile = (is_static, is_const, is_volatile).into();
				write_indented!(
					f,
					0,
					&scv,
					typ,
					" ",
					name,
					" = {",
					&(value.as_slice(), ", ", 0),
					"};"
				);
				Ok(())
			}
			LanguageElement::PointerAssignment { ptr, value } => {
				write!(f, "*{} = {};", ptr, value)
			}
			LanguageElement::StructFieldPointerAssignment { name, field, value } => {
				write!(f, "{}->{} = {};", name, field, value)
			}
			LanguageElement::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				write_indented!(f, indent, typ, " ", name);
				if args.is_empty() {
					write_indented!(f, indent, "() {\n");
				} else {
					write_indented!(
						f,
						indent,
						"(\n",
						&(args.as_slice(), ",\n", 1),
						"\n",
						&indent,
						") {\n"
					);
				}
				write_indented!(
					f,
					indent,
					&(block.as_slice(), "\n", 1),
					"\n",
					&indent,
					"}\n"
				);
				Ok(())
			}
			LanguageElement::StructDefinition { name, members } => {
				write_indented!(
					f,
					indent,
					"struct ",
					name,
					" {\n",
					&(members.as_slice(), ";\n", 1),
					"\n",
					&indent,
					"};"
				);
				Ok(())
			}
			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				write_indented!(
					f,
					indent,
					"if (",
					condition,
					") {\n",
					&(then.as_slice(), "\n", 1),
					"\n",
					&indent,
					"}"
				);
				if let Some(else_then) = else_then {
					write_indented!(
						f,
						indent,
						" else {\n",
						&(else_then.as_slice(), "\n", 1),
						"\n",
						&indent,
						"}"
					);
				}
				Ok(())
			}
			LanguageElement::For {
				init,
				condition,
				post,
				body,
			} => {
				write_indented!(
					f,
					0,
					"for (",
					&(init.as_slice(), " ", 0),
					" ",
					condition,
					"; ",
					&(post.as_slice(), " ", 0),
					") {\n"
				);
				write_indented!(f, indent, &(body.as_slice(), "\n", 1), "\n", &indent, "}");
				Ok(())
			}
			LanguageElement::While { condition, body } => {
				write_indented!(
					f,
					indent,
					"while (",
					condition,
					") {\n",
					&(body.as_slice(), "\n", 1),
					"\n",
					&indent,
					"}"
				);
				Ok(())
			}
			LanguageElement::Return(val) => {
				write!(f, "return")?;
				if let Some(val) = val {
					write!(f, " {}", val)?;
				}
				write!(f, ";")
			}
			LanguageElement::Statement(statement) => {
				DisplayWithIndent::fmt(&indent, f, 0)?;
				write!(f, "{};", statement)
			}
		}
	}
}

impl DisplayWithIndent for StructlessLanguage<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		match self {
			StructlessLanguage::VariableDeclaration {
				typ,
				name,
				is_static,
				is_const,
				is_volatile,
			} => {
				let scv: StaticConstVolatile = (is_static, is_const, is_volatile).into();
				write!(f, "{}{} {};", scv, typ, name)
			}
			StructlessLanguage::VariableAssignment { name, value } => {
				write!(f, "{} = {};", name, value)
			}
			StructlessLanguage::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_static,
				is_const,
				is_volatile,
			} => {
				let scv: StaticConstVolatile = (is_static, is_const, is_volatile).into();
				write!(f, "{}{} {} = {};", scv, typ, name, value)
			}
			StructlessLanguage::PointerAssignment { ptr, value } => {
				write!(f, "*({}) = {};", ptr, value)
			}
			StructlessLanguage::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				write_indented!(f, indent, typ, " ", name);
				if args.is_empty() {
					write_indented!(f, indent, "() {\n");
				} else {
					write_indented!(
						f,
						indent,
						"(\n",
						&(args.as_slice(), ",\n", 1),
						"\n",
						&indent,
						") {\n"
					);
				}
				write_indented!(
					f,
					indent,
					&(block.as_slice(), "\n", 1),
					"\n",
					&indent,
					"}\n"
				);
				Ok(())
			}
			StructlessLanguage::IfStatement {
				condition,
				then,
				else_then,
			} => {
				write_indented!(f, indent, "if (", condition, ") ", then.as_ref());
				if let Some(else_then) = else_then {
					write_indented!(f, indent, " else ", else_then.as_ref());
				}
				Ok(())
			}
			StructlessLanguage::Loop { condition, body } => {
				write_indented!(f, indent, "while (", condition, ") ", body.as_ref());
				Ok(())
			}
			StructlessLanguage::Return(statement) => {
				write!(f, "return")?;
				if let Some(statement) = statement {
					write!(f, " {}", statement)?;
				}
				write!(f, ";")
			}
			StructlessLanguage::Statement(s) => write!(f, "{};", s),
			StructlessLanguage::VariableLabelTag {
				name,
				is_static,
				is_const,
				is_volatile,
			} => {
				let scv: StaticConstVolatile = (is_static, is_const, is_volatile).into();
				write!(f, "VARIABLE_TAG_FOR_STRUCT_NAMES ({}{})", scv, name)
			}
			StructlessLanguage::Block { block, scope_name } => {
				write_indented!(
					f,
					indent,
					"'",
					scope_name,
					": {\n",
					&(block.as_slice(), "\n", 1),
					"\n",
					&indent,
					"}"
				);
				Ok(())
			}
		}
	}
}

impl DisplayWithIndent for StatementElement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		match self {
			StatementElement::Add { lhs, rhs } => write!(f, "({}) + ({})", lhs, rhs),
			StatementElement::Sub { lhs, rhs } => write!(f, "({}) - ({})", lhs, rhs),
			StatementElement::Mul { lhs, rhs } => write!(f, "({}) * ({})", lhs, rhs),
			StatementElement::Div { lhs, rhs } => write!(f, "({}) / ({})", lhs, rhs),
			StatementElement::Mod { lhs, rhs } => write!(f, "({}) % ({})", lhs, rhs),
			StatementElement::LShift { lhs, rhs } => write!(f, "({}) << ({})", lhs, rhs),
			StatementElement::RShift { lhs, rhs } => write!(f, "({}) >> ({})", lhs, rhs),
			StatementElement::BitAnd { lhs, rhs } => write!(f, "({}) & ({})", lhs, rhs),
			StatementElement::BitOr { lhs, rhs } => write!(f, "({}) | ({})", lhs, rhs),
			StatementElement::BoolAnd { lhs, rhs } => write!(f, "({}) && ({})", lhs, rhs),
			StatementElement::BoolOr { lhs, rhs } => write!(f, "({}) || ({})", lhs, rhs),
			StatementElement::Xor { lhs, rhs } => write!(f, "({}) ^ ({})", lhs, rhs),
			StatementElement::GreaterThan { lhs, rhs } => write!(f, "({}) > ({})", lhs, rhs),
			StatementElement::LessThan { lhs, rhs } => write!(f, "({}) < ({})", lhs, rhs),
			StatementElement::GreaterThanEqual { lhs, rhs } => write!(f, "({}) >= ({})", lhs, rhs),
			StatementElement::LessThanEqual { lhs, rhs } => write!(f, "({}) <= ({})", lhs, rhs),
			StatementElement::Cmp { lhs, rhs } => write!(f, "({}) == ({})", lhs, rhs),
			StatementElement::NotCmp { lhs, rhs } => write!(f, "({}) != ({})", lhs, rhs),
			StatementElement::Cast { typ, value } => write!(f, "(({}) ({}))", typ, value),
			StatementElement::Ternary { cond, lhs, rhs } => {
				write!(f, "(({}) ? ({}) : ({}))", cond, lhs, rhs)
			}
			StatementElement::FunctionCall { name, parametres } => {
				write_indented!(f, indent, name, "(", &(parametres.as_slice(), ", ", 0), ")");
				Ok(())
			}
			StatementElement::BitNot(val) => write!(f, "(~{})", val),
			StatementElement::BoolNot(val) => write!(f, "(!{})", val),
			StatementElement::Var(val) => write!(f, "{}", val),
			StatementElement::Num(val) => write!(f, "{}", val),
			StatementElement::Char(val) => write!(f, "'{}'", val),
			StatementElement::Bool(val) => write!(f, "{}", val),
			StatementElement::Array(arr) => {
				write_indented!(f, 0, "{", &(arr.as_slice(), ", ", 0), "}");
				Ok(())
			}
			StatementElement::Deref(val) => write!(f, "(*({}))", val),
			StatementElement::AdrOf(val) => write!(f, "(&{})", val),
			StatementElement::FieldPointerAccess(ptr, field) => write!(f, "({}->{})", ptr, field),
		}
	}
}

impl DisplayWithIndent for Variable<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		write!(f, "{} {}", self.typ, self.name)
	}
}

impl DisplayWithIndent for NativeVariable<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		write!(f, "{} {}", self.typ, self.name)
	}
}

impl DisplayWithIndent for StructlessStatement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		use StructlessStatement::*;
		match self {
			BinOp { op, lhs, rhs, .. } => {
				write!(f, "({}) {} ({})", lhs, op, rhs)
			}
			FunctionCall { name, parametres } => {
				write!(f, "{}(", name)?;
				write_slice(parametres.as_ref(), f, ", ", 0)?;
				write!(f, ")")
			}
			Not(inner) => write!(f, "!({})", inner),
			Var(name) => write!(f, "{}", name),
			Num(Number { val, .. }) => write!(f, "{}", val),
			Char(c) => write!(f, "{}", c),
			Bool(b) => write!(f, "{}", b),
			Array(arr) => {
				write!(f, "{{",)?;
				write_slice(arr, f, ", ", 0)?;
				write!(f, "}}")
			}
			Deref(adr) => write!(f, "*({})", adr),
			AdrOf(name) => write!(f, "&{}", name),
		}
	}
}

impl DisplayWithIndent for MaybeParsed<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		match self {
			MaybeParsed::Parsed(a) => DisplayWithIndent::fmt(a, f, indent),
			MaybeParsed::Unparsed(a) => DisplayWithIndent::fmt(a, f, indent),
		}
	}
}

impl DisplayWithIndent for NativeType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		match self {
			NativeType::Uint => write!(f, "uint"),
			NativeType::Int => write!(f, "int"),
			NativeType::Char => write!(f, "char"),
			NativeType::Bool => write!(f, "bool"),
			NativeType::Void => write!(f, "void"),
			NativeType::Ptr(inner) => write!(f, "{}*", inner),
		}
	}
}

impl DisplayWithIndent for StaticConstVolatile {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		if self.is_static {
			write!(f, "static ")?;
		}
		if self.is_const {
			write!(f, "const ")?;
		}
		if self.is_volatile {
			write!(f, "volatile ")?;
		}
		Ok(())
	}
}

impl DisplayWithIndent for Type<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
		match self {
			Type::Uint => write!(f, "uint"),
			Type::Int => write!(f, "int"),
			Type::Char => write!(f, "char"),
			Type::Bool => write!(f, "bool"),
			Type::Void => write!(f, "void"),
			Type::Ptr(inner) => write!(f, "{}*", inner),
			Type::Struct(name) => write!(f, "{}", name),
			Type::Arr(typ, len) => write!(f, "{}[{}]", typ, len),
		}
	}
}

impl fmt::Display for language_element::LanguageBlock<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write_slice(self.0, f, "\n", 0)
	}
}

impl fmt::Display for ErrorInfo<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ErrorInfo::Source(a) => write!(f, "{}", a),
			ErrorInfo::Token(a) => write_slice(a, f, ", ", 0),
			ErrorInfo::Statement(a) => write!(f, "{}", a),
			ErrorInfo::Language(a) => write!(f, "{}", a),
			ErrorInfo::StructlessStatement(a) => write!(f, "{}", a),
			ErrorInfo::StructlessLanguage(a) => write!(f, "{}", a),
			ErrorInfo::UnTree(a, b) => write!(f, "{}{}", b, a),
			ErrorInfo::BinTree(a, b, c) => write!(f, "{}{:?}{}", a, c, b),
			ErrorInfo::StatementToken(a) => write_slice(a, f, " ", 0),
			ErrorInfo::MaybeParsed(a) => write_slice(a, f, " ", 0),
			ErrorInfo::Variable(a) => write!(f, "{}", a),
		}
	}
}

impl fmt::Display for StatementToken<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for StructlessLanguage<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for StructlessStatement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for Token<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for LanguageElement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for MaybeParsed<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for StatementElement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for Variable<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for NativeVariable<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for NativeType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for Type<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}

impl fmt::Display for Number {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.val)
	}
}

impl fmt::Display for BinOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use BinOp::*;
		let token = match self {
			Add => "+",
			Sub => "-",
			Mul => "*",
			Div => "/",
			Mod => "%",
			LShift => "<<",
			RShift => ">>",
			And => "&&",
			Or => "||",
			Xor => "^",
			GreaterThan => ">",
			LessThan => "<",
			GreaterThanEqual => ">=",
			LessThanEqual => "<=",
			Cmp => "==",
			NotCmp => "!=",
		};
		write!(f, "{}", token)
	}
}

impl fmt::Display for parser::StaticConstVolatile {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		DisplayWithIndent::fmt(self, f, 0)
	}
}
