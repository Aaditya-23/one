    use Kind::*;


#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum Kind {
    // Comments
    LineComment,
    BlockComment,

    // Operators
    Star,
    StarEqual,

    Star2,
    Star2Equal,

    Plus,
    PlusEqual,

    Minus,
    MinusEqual,

    Plus2,
    Minus2,

    Slash,
    SlashEqual,

    Mod,
    ModEqual,

    Equal,
    Equal2,
    Equal3,

    Bang,

    Ampersand2,
    Ampersand2Equal,

    Pipe2,
    Pipe2Equal,

    LessThan,
    LessThanOrEqual,

    GreaterThan,
    GreaterThanOrEqual,

    NotEqual,
    NotEqual2,

    Ampersand,
    AmpersandEqual,

    Pipe,
    PipeEqual,

    LeftShift,
    LeftShiftEqual,

    RightShift,
    RightShiftEqual,

    UnsignedRightShift,
    UnsignedRightShiftEqual,

    Caret,
    CaretEqual,

    Tilde,

    ParenO,
    ParenC,

    BracesO,
    BracesC,

    BracketO,
    BracketC,

    Colon,

    Question,
    Question2,
    Question2Equal,
    QuestionDot,

    Identifier,
    Number,
    String,
    Null,
    Boolean,

    Comma,
    Semicolon,
    EOF,

    Dot,
    Dot3,

    // Keywords
    Await,
    Async,
    As,
    Assert,

    Break,

    Case,
    Catch,
    Class,
    Continue,
    Const,
    Constructor,

    Debugger,
    Delete,
    Do,
    Default,

    Export,
    Else,
    Extends,

    Function,
    For,
    Finally,
    From,

    Get,

    If,
    In,
    Instanceof,
    Import,

    Let,

    New,

    Return,

    Super,
    Switch,
    Static,
    Set,

    This,
    Typeof,
    Throw,
    Try,

    Var,
    Void,

    While,
    With,

    // Other
    Arrow,
    BackQuote,
    DollarCurly,
    RegexpPattern,
    RegexpFlag,
}

impl  Kind {
    pub fn as_str(&self) -> &str {
        match self {
            LineComment => "//",
            Star => "*",
            StarEqual => "*=",
            _ => panic!("not implemented")
        }
    }
}