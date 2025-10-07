use proc_macro::*;

#[cfg(test)]
#[proc_macro]
pub fn print_stream(input: TokenStream) -> TokenStream {
    for i in input.into_iter() {
        println!("{:?}", i);
    }
    TokenStream::new()
}

/// Constructs new Atom using MeTTa S-expressions syntax for expressions. Recognizes grounded
/// strings, numbers and booleans. For other grounded symbols use braces.
/// Macros has a performance penalty because it creates and uses an additional
/// wrapper for grounded atoms.
#[proc_macro]
pub fn metta(input: TokenStream) -> TokenStream {
    MettaConverter::new(input, PrinterMut::default()).run()
}

/// Similar to [metta!] but constructs a constant Atom. Main goal is to be able
/// writing `const SOME_SYMBOL: Atom = metta_const!(SomeSymbol)`. This macros
/// uses constant constructors for symbols, variables and expressions internally.
/// Grounded values are not supported as they cannot be instantiated without
/// allocating memory.
#[proc_macro]
pub fn metta_const(input: TokenStream) -> TokenStream {
    MettaConverter::new(input, PrinterConst::default()).run()
}

#[derive(Debug)]
enum InternalToken {
    ExprStart((usize, usize), (usize, usize)),
    ExprEnd((usize, usize), (usize, usize)),
    TokenTree(TokenTree),
    Space,
}

impl InternalToken {
    fn range(span: Span) -> (usize, usize) {
        (span.line(), span.column())
    }

    fn start_expr(tt: &TokenTree) -> Self {
        let (l, c) = Self::range(tt.span().start());
        Self::ExprStart((l, c), (l, c+1))
    }

    fn end_expr(tt: &TokenTree) -> Self {
        let (l, c) = Self::range(tt.span().start());
        Self::ExprEnd((l, c), (l, c+1))
    }

    fn start(&self) -> (usize, usize) {
        match self {
            Self::ExprStart(s, _) => *s,
            Self::ExprEnd(s, _) => *s,
            Self::TokenTree(tt) => Self::range(tt.span().start()),
            Self::Space => unreachable!(),
        }
    }

    fn end(&self) -> (usize, usize) {
        match self {
            Self::ExprStart(_, e) => *e,
            Self::ExprEnd(_, e) => *e,
            Self::TokenTree(tt) => Self::range(tt.span().end()),
            Self::Space => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum TokenizerState {
    Start,
    Symbol(String),
    Variable(String),
    Sign(String),
    Gnd(String, GndType),
    Token(InternalToken),
}

#[derive(Debug)]
enum GndType {
    Int,
    Float,
    Str,
    Bool,
}

#[derive(Debug)]
enum Token {
    ExprStart,
    ExprEnd,
    Int(String),
    Float(String),
    Str(String),
    Bool(String),
    Variable(String),
    Symbol(String),
    Gnd(Group),
    End,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Self::Int(s) => s.clone(),
            Self::Float(s) => s.clone(),
            Self::Str(s) => s.clone(),
            _ => todo!(),
        }
    }
}

struct Tokenizer {
    state: TokenizerState,
    input: Box<dyn Iterator<Item=InternalToken>>,
}

impl Tokenizer {
    fn new(input: TokenStream) -> Self {
        let mut prev_end = (0, 0);
        let input = input.into_iter().flat_map(Self::unroll_group).flat_map(
            move |it| -> Box<dyn Iterator<Item=InternalToken>> {
                let is_space = prev_end != it.start();
                prev_end = it.end();
                if is_space {
                    Box::new(std::iter::once(InternalToken::Space).chain(std::iter::once(it)))
                } else {
                    Box::new(std::iter::once(it))
                }
            });
        Self {
            state: TokenizerState::Start,
            input: Box::new(input),
        }
    }

    fn unroll_group(tt: TokenTree) -> Box<dyn Iterator<Item=InternalToken>> {
        match &tt {
            TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
                let open = std::iter::once(InternalToken::start_expr(&tt));
                let close = std::iter::once(InternalToken::end_expr(&tt));
                Box::new(open.chain(g.stream().into_iter().flat_map(Self::unroll_group)).chain(close))
            },
            _ => {
                Box::new(std::iter::once(InternalToken::TokenTree(tt)))
            }
        }
    }

    fn next(&mut self) -> Token {
        type TS = TokenizerState;
        type IT = InternalToken;
        type T = Token;
        loop {
            let (state, it) = match std::mem::replace(&mut self.state, TS::Start) {
                TS::Token(t) => (TS::Start, Some(t)),
                state => (state, self.input.next()),
            };
            let (token, state) = match (state, it) {
                (TS::Start, None) => return T::End,
                (TS::Start, Some(IT::ExprStart(_, _))) => (Some(T::ExprStart), TS::Start),
                (TS::Start, Some(IT::ExprEnd(_, _))) => (Some(T::ExprEnd), TS::Start),
                (TS::Start, Some(IT::TokenTree(tt))) => {
                    match tt {
                        TokenTree::Literal(l) => {
                            let s = l.to_string();
                            let lit = litrs::Literal::parse(s.clone()).expect("Failed to parse literal");
                            match lit {
                                litrs::Literal::Integer(_) => (None, TS::Gnd(s, GndType::Int)),
                                litrs::Literal::Float(_) => (None, TS::Gnd(s, GndType::Float)),
                                litrs::Literal::String(_) => (None, TS::Gnd(s, GndType::Str)),
                                _ => (None, TS::Symbol(s)), 
                            }
                        },
                        TokenTree::Ident(i) => {
                            let s = i.to_string();
                            if s == "True" || s == "False" {
                                (None, TS::Gnd(s, GndType::Bool))
                            } else {
                                (None, TS::Symbol(s))
                            }
                        },
                        TokenTree::Punct(p) if p.as_char() == '$' => {
                            (None, TS::Variable(String::new()))
                        },
                        TokenTree::Punct(p)
                            if p.as_char() == '+' || p.as_char() == '-' =>
                                (None, TS::Sign(p.to_string())),
                        TokenTree::Group(g)
                            if g.delimiter() == Delimiter::Brace =>
                                (Some(T::Gnd(g)), TS::Start),
                        tt => (None, TS::Symbol(tt.to_string())), 
                    }
                }

                (TS::Sign(s), Some(IT::TokenTree(tt))) => {
                    match &tt {
                        TokenTree::Literal(l) => {
                            let l = l.to_string();
                            let lit = litrs::Literal::parse(l.clone()).expect("Failed to parse literal");
                            let s = s + l.as_str();
                            match lit {
                                litrs::Literal::Integer(_) => (None, TS::Gnd(s, GndType::Int)),
                                litrs::Literal::Float(_) => (None, TS::Gnd(s, GndType::Float)),
                                _ => (None, TS::Symbol(s)), 
                            }
                        },
                        _ => (None, TS::Symbol(s + tt.to_string().as_str())), 
                    }
                }
                (TS::Sign(s), Some(t)) => (Some(T::Symbol(s)), TS::Token(t)),
                (TS::Sign(s), None) => (Some(T::Symbol(s)), TS::Start),

                (TS::Gnd(s, _), Some(IT::TokenTree(tt))) => (None, TS::Symbol(s + tt.to_string().as_str())),
                (TS::Gnd(s, typ), Some(token)) => (Some(Self::gnd_to_token(s, typ)), TS::Token(token)),
                (TS::Gnd(s, typ), None) => (Some(Self::gnd_to_token(s, typ)), TS::Start),

                (TS::Symbol(s), Some(IT::TokenTree(tt))) => (None, TS::Symbol(s + tt.to_string().as_str())),
                (TS::Symbol(s), Some(t)) => (Some(T::Symbol(s)), TS::Token(t)),
                (TS::Symbol(s), None) => (Some(T::Symbol(s)), TS::Start),

                (TS::Variable(s), Some(IT::TokenTree(tt))) => (None, TS::Variable(s + tt.to_string().as_str())),
                (TS::Variable(s), Some(t)) => (Some(T::Variable(s)), TS::Token(t)),
                (TS::Variable(s), None) => (Some(T::Variable(s)), TS::Start),

                (TS::Start, Some(IT::Space)) => (None, TS::Start),

                (TS::Token(_), _) => unreachable!(),
            };

            self.state = state;
            if let Some(token) = token {
                return token
            }
        }
    }

    fn gnd_to_token(s: String, t: GndType) -> Token {
        match t {
            GndType::Int => Token::Int(s),
            GndType::Float => Token::Float(s),
            GndType::Str => Token::Str(s),
            GndType::Bool => Token::Bool(s.to_lowercase()),
        }
    }
}

struct PrinterBase {
    output: Vec<(Delimiter, TokenStream)>,
}

impl Default for PrinterBase {
    fn default() -> Self {
        Self {
            output: vec![(Delimiter::None, TokenStream::new())],
        }
    }
}

impl PrinterBase {
    fn get_token_stream(&mut self) -> TokenStream {
        assert!(self.output.len() == 1, "Unbalanced group");
        self.output.pop().unwrap().1
    }

    fn push(&mut self, tt: TokenTree) -> &mut Self {
        let (_, last) = self.output.last_mut().unwrap();
        last.extend([tt].into_iter());
        self
    }

    fn ident(&mut self, name: &str) -> &mut Self {
        self.push(TokenTree::Ident(Ident::new(name, Span::call_site())))
    }

    fn punct(&mut self, chars: &str) -> &mut Self {
        assert!(!chars.is_empty(), "Empty punct");
        let mut chars = chars.chars().peekable();
        let mut c = chars.next().unwrap();
        while chars.peek().is_some()  {
            let _ = self.push(TokenTree::Punct(Punct::new(c, Spacing::Joint)));
            c = chars.next().unwrap();
        }
        self.push(TokenTree::Punct(Punct::new(c, Spacing::Alone)))
    }

    fn group(&mut self, d: char) -> &mut Self {
        let (open, delimiter) = match d {
            '(' => (true, Delimiter::Parenthesis),
            '{' => (true, Delimiter::Brace),
            '[' => (true, Delimiter::Bracket),
            ')' => (false, Delimiter::Parenthesis),
            '}' => (false, Delimiter::Brace),
            ']' => (false, Delimiter::Bracket),
            _ => panic!("Unexpected delimiter: {}", d),
        };
        if open {
            self.output.push((delimiter, TokenStream::new()));
            self
        } else {
            assert!(self.output.len() > 1, "Unbalanced group");
            let (d, stream) = self.output.pop().unwrap();
            assert!(d == delimiter, "Closing delimiter {:?} is not equal to opening one {:?}", delimiter, d);
            self.push(TokenTree::Group(Group::new(delimiter, stream)))
        }
    }

    fn literal(&mut self, lit: Literal) -> &mut Self {
        self.push(TokenTree::Literal(lit))
    }

    fn string(&mut self, text: &str) -> &mut Self {
        self.push(TokenTree::Literal(Literal::string(text)))
    }

    fn bool(&mut self, b: &str) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon_atom").punct("::").ident("gnd").punct("::").ident("bool").punct("::").ident("Bool").group('(')
            .ident(b)
            .group(')').group(')');
    }

    fn integer(&mut self, n: i64) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon_atom").punct("::").ident("gnd").punct("::").ident("number").punct("::").ident("Number").punct("::").ident("Integer").group('(')
            .literal(Literal::i64_suffixed(n))
            .group(')').group(')');
    }

    fn float(&mut self, f: f64) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon_atom").punct("::").ident("gnd").punct("::").ident("number").punct("::").ident("Number").punct("::").ident("Float").group('(')
            .literal(Literal::f64_suffixed(f))
            .group(')').group(')');
    }

    fn str(&mut self, s: &str) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon_atom").punct("::").ident("gnd").punct("::").ident("str").punct("::").ident("Str").punct("::").ident("from_str").group('(')
            .literal(Literal::string(s))
            .group(')').group(')');
    }

    fn gnd(&mut self, g: Group) {
        self.group('(').punct("&&").ident("hyperon_atom").punct("::").ident("Wrap").group('(')
            .push(TokenTree::Group(Group::new(Delimiter::Parenthesis, g.stream())))
            .group(')').group(')')
            .punct(".").ident("to_atom").group('(').group(')');
    }

    fn expr_delimiter(&mut self) {
        self.punct(",");
    }
}

trait Printer {
    fn symbol(&mut self, name: &str);
    fn variable(&mut self, name: &str);
    fn bool(&mut self, b: &str);
    fn integer(&mut self, n: i64);
    fn float(&mut self, f: f64);
    fn str(&mut self, s: &str);
    fn gnd(&mut self, g: Group);
    fn expr_start(&mut self);
    fn expr_delimiter(&mut self);
    fn expr_end(&mut self);
    fn get_token_stream(&mut self) -> TokenStream;
}

#[repr(transparent)]
#[derive(Default)]
struct PrinterMut {
    base: PrinterBase,
}

impl Printer for PrinterMut {
    fn symbol(&mut self, name: &str) {
        self.base.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Symbol").group('(')
            .ident("hyperon_atom").punct("::").ident("SymbolAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("unique_string").punct("::").ident("UniqueString").punct("::").ident("from").group('(')
            .string(name)
            .group(')').group(')').group(')');
    }

    fn variable(&mut self, name: &str) {
        self.base.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Variable").group('(')
            .ident("hyperon_atom").punct("::").ident("VariableAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("unique_string").punct("::").ident("UniqueString").punct("::").ident("from").group('(')
            .string(name)
            .group(')').group(')').group(')');
    }

    fn expr_start(&mut self) { 
        self.base.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Expression").group('(')
            .ident("hyperon_atom").punct("::").ident("ExpressionAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("collections").punct("::").ident("CowArray").punct("::").ident("from").group('(')
            .group('[');
    }

    fn expr_end(&mut self) {
        self.base.group(']')
            .group(')').group(')').group(')');
    }

    fn bool(&mut self, b: &str) { self.base.bool(b) }
    fn integer(&mut self, n: i64) { self.base.integer(n) }
    fn float(&mut self, f: f64) { self.base.float(f) }
    fn str(&mut self, s: &str) { self.base.str(s) }
    fn gnd(&mut self, g: Group) { self.base.gnd(g) }
    fn expr_delimiter(&mut self) { self.base.expr_delimiter() }
    fn get_token_stream(&mut self) -> TokenStream { self.base.get_token_stream() }
}

#[repr(transparent)]
#[derive(Default)]
struct PrinterConst {
    base: PrinterBase,
}

impl Printer for PrinterConst {
    fn symbol(&mut self, name: &str) {
        self.base.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Symbol").group('(')
            .ident("hyperon_atom").punct("::").ident("SymbolAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("unique_string").punct("::").ident("UniqueString").punct("::").ident("Const").group('(')
            .string(name)
            .group(')').group(')').group(')');
    }

    fn variable(&mut self, name: &str) {
        self.base.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Variable").group('(')
            .ident("hyperon_atom").punct("::").ident("VariableAtom").punct("::").ident("new_const").group('(')
            .ident("hyperon_common").punct("::").ident("unique_string").punct("::").ident("UniqueString").punct("::").ident("Const").group('(')
            .string(name)
            .group(')').group(')').group(')');
    }

    fn expr_start(&mut self) {
        self.base.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Expression").group('(')
            .ident("hyperon_atom").punct("::").ident("ExpressionAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("collections").punct("::").ident("CowArray").punct("::").ident("Literal").group('(')
            .ident("const").group('{').punct("&").group('[');
    }

    fn expr_end(&mut self) {
        self.base.group(']').group('}')
            .group(')').group(')').group(')');
    }

    fn bool(&mut self, _b: &str) { panic!("Grounded atoms cannot be instantiated as const") }
    fn integer(&mut self, _n: i64) { panic!("Grounded atoms cannot be instantiated as const") }
    fn float(&mut self, _f: f64) { panic!("Grounded atoms cannot be instantiated as const") }
    fn str(&mut self, _s: &str) { panic!("Grounded atoms cannot be instantiated as const") }
    fn gnd(&mut self, _g: Group) { panic!("Grounded atoms cannot be instantiated as const") }
    fn expr_delimiter(&mut self) { self.base.expr_delimiter() }
    fn get_token_stream(&mut self) -> TokenStream { self.base.get_token_stream() }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum State {
    Start,
    ExprStart(usize),
    Expression(usize),
    Final,
}

struct MettaConverter<P: Printer> {
    state: State,
    input: Tokenizer,
    output: P,
}

impl<P: Printer> MettaConverter<P> {
    fn new(input: TokenStream, output: P) -> Self {
        Self{
            state: State::Start,
            input: Tokenizer::new(input),
            output,
        }
    }

    fn run(&mut self) -> TokenStream {
        loop {
            if self.state == State::Final {
                break
            }
            self.next_state();
        }
        self.output.get_token_stream()
    }

    fn next_state(&mut self) {
        let token = self.input.next();

        if matches!(token, Token::End) {
            if !matches!(self.state, State::Start) {
                panic!("Unexpected expression end");
            }
            self.state = State::Final;
            return;
        }

        if matches!(self.state, State::Expression(_))
            && !matches!(token, Token::ExprEnd) {
                self.output.expr_delimiter();
        }

        let mut next_state = self.state;

        match token {
            Token::Symbol(s) => self.output.symbol(&s),
            Token::Variable(v) => self.output.variable(&v),
            Token::Int(s) => self.output.integer(s.parse::<i64>().unwrap()),
            Token::Float(s) => self.output.float(s.parse::<f64>().unwrap()),
            Token::Str(s) => self.output.str(&s[1..s.len() - 1]),
            Token::Bool(s) => self.output.bool(&s),
            Token::Gnd(g) => self.output.gnd(g),

            Token::ExprStart => {
                self.output.expr_start();
                next_state = match self.state {
                    State::Start => State::ExprStart(1),
                    State::ExprStart(n) => State::ExprStart(n + 1),
                    State::Expression(n) => State::ExprStart(n + 1),
                    State::Final => unreachable!(),
                };
            },
            Token::ExprEnd => {
                next_state = match self.state {
                    State::Start => panic!("Unexpected end of expression"),
                    State::ExprStart(1) => State::Start,
                    State::ExprStart(n) => State::Expression(n - 1),
                    State::Expression(1) => State::Start,
                    State::Expression(n) => State::Expression(n - 1),
                    State::Final => unreachable!(),
                };
                self.output.expr_end();
            },

            Token::End => unreachable!(),
        }

        if let State::ExprStart(n) = self.state {
            if next_state == self.state {
                next_state = State::Expression(n);
            }
        }
        self.state = next_state;
    }
}
