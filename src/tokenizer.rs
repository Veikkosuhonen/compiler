use lazy_static::lazy_static;
use regex::{Match, Regex};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Not,
    Equals,
    NotEquals,
    LT,
    GT,
    LTE,
    GTE,
    And,
    Or,
    Assign,
}

impl Op {
    pub fn try_from_str(value: &str) -> Option<Op> {
        Some(match value {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "%" => Op::Mod,
            "**" => Op::Exp,
            "not" => Op::Not,
            "==" => Op::Equals,
            "!=" => Op::NotEquals,
            "<" => Op::LT,
            ">" => Op::GT,
            "<=" => Op::LTE,
            ">=" => Op::GTE,
            "and" => Op::And,
            "or" => Op::Or,
            "=" => Op::Assign,
            _ => { return None }
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    Keyword,
    IntegerLiteral,
    BooleanLiteral,
    Operator,
    Punctuation,
    None,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub location: SourceLocation
}

lazy_static! {
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s+").unwrap();
    static ref MULTILINE_COMMENT_REGEX: Regex = Regex::new(r"^/\*[\s\S]*?\*/").unwrap();
    static ref LINE_COMMENT_REGEX: Regex = Regex::new(r"^//.*(\n|$)").unwrap();
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    static ref INTEGER_LITERAL_REGEX: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref BOOLEAN_LITERAL_REGEX: Regex = Regex::new(r"^(true|false)").unwrap();
    static ref OPERATOR_REGEX: Regex = Regex::new(r"^(==|!=|<=|>=|\+|-|\*?\*|/|%|=|<|>|and|or|not)").unwrap();
    static ref PUNCTUATION_REGEX: Regex = Regex::new(r"^(\(|\)|\{|\}|,|;|:)").unwrap();
    static ref KEYWORD_REGEX: Regex = Regex::new(r"^(if|then|else|var)").unwrap();

    // Order is significant here. The first match is the one that will be used.
    static ref TOKEN_REGEX_TO_TYPE: Vec<(Regex, TokenType)> = vec![
        (WHITESPACE_REGEX.clone(), TokenType::None),
        (MULTILINE_COMMENT_REGEX.clone(), TokenType::None),
        (LINE_COMMENT_REGEX.clone(), TokenType::None),
        (KEYWORD_REGEX.clone(), TokenType::Keyword),
        (OPERATOR_REGEX.clone(), TokenType::Operator),
        (BOOLEAN_LITERAL_REGEX.clone(), TokenType::BooleanLiteral),
        (IDENTIFIER_REGEX.clone(), TokenType::Identifier),
        (INTEGER_LITERAL_REGEX.clone(), TokenType::IntegerLiteral),
        (PUNCTUATION_REGEX.clone(), TokenType::Punctuation),
    ];
}

fn match_token(source: &str) -> Option<(Match<'_>, TokenType)> {
    TOKEN_REGEX_TO_TYPE
        .iter()
        .enumerate()
        .find_map(|(i, token_regex_to_type)| {
            let (regex, _) = token_regex_to_type;
            regex.find(source).map(|m| (m, TOKEN_REGEX_TO_TYPE[i].1))
        })
}

fn count_line_column_changes(v: &str, line: usize, column: usize) -> (usize, usize) {
    let line_change = v.chars().filter(|s| s.eq(&'\n')).count();
    let column = match line_change {
        0 => column + v.len(),
        _ => v.len() - v.rfind('\n').unwrap_or(0),
    };

    (line + line_change, column)
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut line = 1;
    let mut column = 1;

    let mut current_start = 0;

    while current_start < source.len() {
        let slice = &source[current_start..];

        if let Some((m, token_type)) = match_token(slice) {
            if token_type != TokenType::None {
                let location = SourceLocation { line, column };
                tokens.push(Token { value: m.as_str().to_string(), location, token_type });
            }

            (line, column) = count_line_column_changes(m.as_str(), line, column);

            current_start += m.end();
        } else {
            panic!(
                "Cannot tokenize input at line {} column {}: {}",
                line, current_start, slice
            );
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    /*
    use super::*;
    
    #[test]
    fn test_keywords_integers_and_literals() {
        let source = "
if 3 wowowo
123 123
";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 5);

        assert!( if let TokenValue::StringLike(value) = tokens[0].value { value == "if" } else { false } );
        assert_eq!(tokens[0].location.line, 2);
        assert_eq!(tokens[0].location.column, 1);

        if let Token::IntegerLiteral { value, location,.. } = tokens[1] {
            assert_eq!(value, 3);
            assert_eq!(location.line, 2);
            assert_eq!(location.column, 4);
        } else { 
            panic!("Not an int");
        }

        match tokens[2] {
            Token::Identifier { value, location,.. } => {
                assert_eq!(value, "wowowo");
                assert_eq!(location.line, 2);
                assert_eq!(location.column, 6);
            },
            _ => panic!("Not an identifier")
        }

        match tokens[3] {
            Token::Keyword { value, location,.. } => {
                assert_eq!(value, "if");
                assert_eq!(location.line, 3);
                assert_eq!(location.column, 5);
            },
            _ => panic!("Not a keyword")
        }
    }

    #[test]
    fn test_operators() {
        let source = "+ - * / = == != < <= > >=";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 11);
        assert!(matches!(tokens[10], Token::Operator { .. }));

    }

    #[test]
    fn test_punctuation() {
        let source = "( ) { } , ;";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[5], Token::Punctuation { .. }));
    }

    #[test]
    fn test_comment() {
        let source = "( ) { } , ; // comment is fine";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[5], Token::Punctuation { .. }));
    }

    #[test]
    fn test_multiline_comment() {
        let mut source = "( ) { } , ; /* heyo */";
        let mut tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[7], Token::Punctuation { .. }));

        source = "( ) { } , ; /*
            heyo 123
        */ heyo_identifier";
        tokens = tokenize(source);

        assert_eq!(tokens.len(), 7);
        assert!(matches!(tokens[7], Token::Identifier { .. }));

        source = "( ) { } , ; /*
            heyo 123
        */ heyo_identifier /* another
        comment
         */ 123";
        tokens = tokenize(source);

        assert_eq!(tokens.len(), 8);
        assert!(matches!(tokens[7], Token::IntegerLiteral { .. }));
    }

    #[test]
    fn long_test() {
        let source = " // 1
            // Here is a function 2
            function f(int cool_number = 1) { // 3
                if (cool_number == 2) // Not default 4
                    print(wow); // 5
                } // 6
                return whatever; // 7
            } // 8

            // Lets invoke it: // 10
            f(2); // prints wow 11
        ";
        let tokens: Vec<Token> = tokenize(source);
        assert_eq!(tokens.len(), 30);
        if let Token::Punctuation { location, .. } = tokens[tokens.len() - 1] {
            assert_eq!(location.line, 11);
        } else {
            panic!("Last token not punctuation!");
        }
    }

    #[test]
    fn test_function_call() {
        let source = "f(1, 2, 3)";
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 8);
        assert!(matches!(tokens[0], Token::Identifier     {..}));
        assert!(matches!(tokens[1], Token::Punctuation    {..}));
        assert!(matches!(tokens[2], Token::IntegerLiteral {..}));
        assert!(matches!(tokens[3], Token::Punctuation    {..}));
        assert!(matches!(tokens[4], Token::IntegerLiteral {..}));
        assert!(matches!(tokens[5], Token::Punctuation    {..}));
        assert!(matches!(tokens[6], Token::IntegerLiteral {..}));
        assert!(matches!(tokens[7], Token::Punctuation    {..}));
    }
    */
}
