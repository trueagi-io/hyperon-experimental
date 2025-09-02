use proc_macro::*;

#[proc_macro]
pub fn print_stream(input: TokenStream) -> TokenStream {
    for i in input.into_iter() {
        println!("{:?}", i);
    }
    TokenStream::new()
}

#[proc_macro]
pub fn metta(input: TokenStream) -> TokenStream {
    MettaConverter::new(input).run()
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

struct Printer {
    output: Vec<(Delimiter, TokenStream)>,
}

impl Printer {
    fn new() -> Self {
        Self {
            output: vec![(Delimiter::None, TokenStream::new())],
        }
    }

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

    fn symbol(&mut self, name: &str) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Symbol").group('(')
            .ident("hyperon_atom").punct("::").ident("SymbolAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("unique_string").punct("::").ident("UniqueString").punct("::").ident("Const").group('(')
            .string(name)
            .group(')').group(')').group(')');
    }

    fn variable(&mut self, name: &str) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Variable").group('(')
            .ident("hyperon_atom").punct("::").ident("VariableAtom").punct("::").ident("new_const").group('(')
            .ident("hyperon_common").punct("::").ident("unique_string").punct("::").ident("UniqueString").punct("::").ident("Const").group('(')
            .string(name)
            .group(')').group(')').group(')');
    }

    fn bool(&mut self, b: &str) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon").punct("::").ident("metta").punct("::").ident("runner").punct("::").ident("bool").punct("::").ident("Bool").group('(')
            .ident(b)
            .group(')').group(')');
    }

    fn integer(&mut self, n: i64) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon").punct("::").ident("metta").punct("::").ident("runner").punct("::").ident("number").punct("::").ident("Number").punct("::").ident("Integer").group('(')
            .literal(Literal::i64_suffixed(n))
            .group(')').group(')');
    }

    fn float(&mut self, f: f64) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon").punct("::").ident("metta").punct("::").ident("runner").punct("::").ident("number").punct("::").ident("Number").punct("::").ident("Float").group('(')
            .literal(Literal::f64_suffixed(f))
            .group(')').group(')');
    }

    fn str(&mut self, s: &str) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("gnd").group('(')
            .ident("hyperon").punct("::").ident("metta").punct("::").ident("runner").punct("::").ident("str").punct("::").ident("Str").punct("::").ident("from_str").group('(')
            .literal(Literal::string(s))
            .group(')').group(')');
    }

    fn gnd(&mut self, g: Group) {
        self.group('(').punct("&&").ident("hyperon_atom").punct("::").ident("Wrap").group('(')
            .push(TokenTree::Group(Group::new(Delimiter::Parenthesis, g.stream())))
            .group(')').group(')')
            .punct(".").ident("to_atom").group('(').group(')');
    }

    fn expr_start(&mut self) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Expression").group('(')
            .ident("hyperon_atom").punct("::").ident("ExpressionAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("collections").punct("::").ident("CowArray").punct("::").ident("from").group('(')
            .group('[');
    }

    fn const_expr_start(&mut self) {
        self.ident("hyperon_atom").punct("::").ident("Atom").punct("::").ident("Expression").group('(')
            .ident("hyperon_atom").punct("::").ident("ExpressionAtom").punct("::").ident("new").group('(')
            .ident("hyperon_common").punct("::").ident("collections").punct("::").ident("CowArray").punct("::").ident("Literal").group('(')
            .ident("const").group('{').punct("&").group('[');
    }

    fn expr_delimiter(&mut self) {
        self.punct(",");
    }

    fn expr_end(&mut self) {
        self.group(']')
            .group(')').group(')').group(')');
    }

    fn const_expr_end(&mut self) {
        self.group(']').group('}')
            .group(')').group(')').group(')');
    }
}

#[derive(PartialEq)]
enum State {
    Continue,
    Final,
}

enum ExprItem {
    Token(Token),
    TokenStream(TokenStream),
}

struct ExprState {
    is_const: bool,
    tokens: Vec<ExprItem>
}

impl ExprState {
    fn new() -> Self {
        Self {
            is_const: true,
            tokens: Vec::new(),
        }
    }
}

struct MettaConverter {
    input: Tokenizer,
    output: Printer,
    expr: Vec<ExprState>,
}

impl MettaConverter {
    fn new(input: TokenStream) -> Self {
        Self{
            input: Tokenizer::new(input),
            output: Printer::new(),
            expr: Vec::new(),
        }
    }

    fn run(&mut self) -> TokenStream {
        loop {
            if State::Final == self.next_state() {
                break;
            }
        }
        self.output.get_token_stream()
    }

    fn next_state(&mut self) -> State {
        let token = self.input.next();

        if matches!(token, Token::End) {
            if !self.expr.is_empty() {
                panic!("Unexpected expression end");
            }
            return State::Final;
        }

        match self.expr.last_mut() {
            None => 
                match token {
                    Token::Symbol(s) => self.output.symbol(&s),
                    Token::Variable(v) => self.output.variable(&v),
                    Token::Int(s) => self.output.integer(s.parse::<i64>().unwrap()),
                    Token::Float(s) => self.output.float(s.parse::<f64>().unwrap()),
                    Token::Str(s) => self.output.str(&s[1..s.len() - 1]),
                    Token::Bool(s) => self.output.bool(&s),
                    Token::Gnd(g) => self.output.gnd(g),
                    Token::ExprStart => self.expr.push(ExprState::new()),
                    Token::ExprEnd => panic!("Expression end without expression start"),
                    Token::End => return State::Final,
                },
            Some(expr) =>
                match token {
                    Token::Symbol(_) | Token::Variable(_) => expr.tokens.push(ExprItem::Token(token)),
                    Token::Int(_) | Token::Float(_) | Token::Str(_)
                        | Token::Bool(_) | Token::Gnd(_) => {
                            expr.tokens.push(ExprItem::Token(token));
                            expr.is_const = false;
                        },
                    Token::ExprStart => self.expr.push(ExprState::new()),
                    Token::ExprEnd => {
                        let expr = self.expr.pop().unwrap();
                        match self.expr.last_mut() {
                            Some(outer) => {
                                let mut printer = Printer::new();
                                let expr_is_const = expr.is_const;
                                Self::output_expression(expr, &mut printer);
                                outer.is_const = outer.is_const && expr_is_const;
                                outer.tokens.push(ExprItem::TokenStream(printer.get_token_stream()));
                            }
                            None => Self::output_expression(expr, &mut self.output),
                        }
                    },
                    Token::End => panic!("Unexpected atom end"),
                },
        }
        return State::Continue;
    }

    fn output_expression(expr: ExprState, printer: &mut Printer) {
        if expr.is_const {
            printer.const_expr_start();
        } else {
            printer.expr_start();
        }

        let mut delimiter = false;
        for item in expr.tokens {
            if delimiter {
                printer.expr_delimiter();
            } else {
                delimiter = true;
            }
            match item {
                ExprItem::Token(token) =>
                    match token {
                        Token::Symbol(s) => printer.symbol(&s),
                        Token::Variable(v) => printer.variable(&v),
                        Token::Int(s) => printer.integer(s.parse::<i64>().unwrap()),
                        Token::Float(s) => printer.float(s.parse::<f64>().unwrap()),
                        Token::Str(s) => printer.str(&s[1..s.len() - 1]),
                        Token::Bool(s) => printer.bool(&s),
                        Token::Gnd(g) => printer.gnd(g),
                        _ => unreachable!(),
                    }
                ExprItem::TokenStream(ts) => {
                    for tt in ts {
                        printer.push(tt);
                    }
                }
            }
        }

        if expr.is_const {
            printer.const_expr_end();
        } else {
            printer.expr_end();
        }
    }
}
