use lazy_static::lazy_static;
use regex::{Match, Regex};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Op {
    Add,
    Sub,
    UnarySub,
    Mul,
    Div,
    Mod,
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
    AddressOf,
    Deref,
    New,
    Delete,
    Member,
}

impl Op {
    pub fn from_str(value: &str) -> Result<Op, &str> {
        Op::logical_from_str(value)
            .or_else(|_| Op::binary_from_str(value))
            .or_else(|_| Op::unary_from_str(value))
    }

    pub fn logical_from_str(value: &str) -> Result<Op, &str> {
        Ok(match value {
            "and" => Op::And,
            "or" => Op::Or,
            _ => return Err("Unknown logical operator"),
        })
    }

    pub fn binary_from_str(value: &str) -> Result<Op, &str> {
        Ok(match value {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "%" => Op::Mod,
            "==" => Op::Equals,
            "!=" => Op::NotEquals,
            "<" => Op::LT,
            ">" => Op::GT,
            "<=" => Op::LTE,
            ">=" => Op::GTE,
            "=" => Op::Assign,
            "." => Op::Member,
            _ => return Err("Unknown binary operator"),
        })
    }

    pub fn unary_from_str(value: &str) -> Result<Op, &str> {
        Ok(match value {
            "-" => Op::UnarySub,
            "not" => Op::Not,
            "&" => Op::AddressOf,
            "*" => Op::Deref,
            "new" => Op::New,
            "delete" => Op::Delete,
            _ => return Err("Unknown unary operator"),
        })
    }

    pub fn to_string(&self) -> String {
        String::from(match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::UnarySub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Mod => "%",
            Op::Not => "not",
            Op::Equals => "==",
            Op::NotEquals => "!=",
            Op::LT => "<",
            Op::GT => ">",
            Op::LTE => "<=",
            Op::GTE => ">=",
            Op::And => "and",
            Op::Or => "or",
            Op::Assign => "=",
            Op::AddressOf => "&",
            Op::Deref => "*",
            Op::New => "new",
            Op::Delete => "delete",
            Op::Member => ".",
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn to_string(&self) -> String {
        format!("{}:{}", self.line, self.column)
    }

    pub fn at(line: usize, column: usize) -> Self {
        SourceLocation { line, column }
    }
}

#[derive(Debug)]
pub struct TokenisationError {
    pub message: String,
    pub location: SourceLocation,
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
    pub start: SourceLocation,
    pub end: SourceLocation,
}

lazy_static! {
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s+").unwrap();
    static ref MULTILINE_COMMENT_REGEX: Regex = Regex::new(r"^/\*[\s\S]*?\*/").unwrap();
    static ref LINE_COMMENT_REGEX: Regex = Regex::new(r"^(//|#).*(\n|$)").unwrap();
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    static ref INTEGER_LITERAL_REGEX: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref BOOLEAN_LITERAL_REGEX: Regex = Regex::new(r"^(true|false)").unwrap();
    static ref OPERATOR_REGEX: Regex = Regex::new(r"^(==|!=|<=|>=|\+|-|\*|/|%|=|<|>|and\b|or\b|not\b|&|new\b|delete\b|\.)").unwrap();
    static ref PUNCTUATION_REGEX: Regex = Regex::new(r"^(\(|\)|\{|\}|,|;|:)").unwrap();
    static ref KEYWORD_REGEX: Regex = Regex::new(r"^(while\b|do\b|if\b|then\b|else\b|var\b|fun\b|return\b|struct\b)").unwrap();

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

pub fn tokenize(source: &str) -> Result<Vec<Token>, TokenisationError> {
    // Hack: wrap the source in curly braces so it can be parsed as a single block.
    let mut tokens: Vec<Token> = vec![];
    tokens.push(Token {
        value: "{".to_string(),
        token_type: TokenType::Punctuation,
        start: SourceLocation::at(0, 0),
        end: SourceLocation::at(0, 0),
    });

    let mut line = 1;
    let mut column = 1;

    let mut current_start = 0;

    while current_start < source.len() {
        let slice = &source[current_start..];
        let start = SourceLocation { line, column };

        if let Some((m, token_type)) = match_token(slice) {
            if token_type != TokenType::None {
                let end = SourceLocation { line, column: column + m.end() };
                tokens.push(Token {
                    value: m.as_str().to_string(),
                    token_type,
                    start,
                    end,
                });
            }

            (line, column) = count_line_column_changes(m.as_str(), line, column);

            current_start += m.end();
        } else {
            return Err(TokenisationError {
                location: SourceLocation { line, column },
                message: format!(
                    "Cannot tokenize input at line {} column {}: {}",
                    line, current_start, slice
                ),
            });
        }
    }

    // Is only token the opening brace we added?
    if tokens.len() == 1 {
        return Err(TokenisationError {
            location: SourceLocation::at(0, 0),
            message: "Empty expression".to_string(),
        });
    }

    // Complete the wrap
    tokens.push(Token {
        value: "}".to_string(),
        token_type: TokenType::Punctuation,
        start: SourceLocation::at(line, column),
        end: SourceLocation::at(line, column),
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_expression() {
        let source = "";
        assert!(tokenize(source).is_err());
    }

    #[test]
    fn return_is_a_keyword() {
        let tokens = tokenize("
            return 1
        ").expect("Shoulve tokenized");
        assert!(matches!(tokens[1].token_type, TokenType::Keyword));
    }

    #[test]
    fn address_of_is_operator() {
        let tokens = tokenize("
            &1
        ").expect("Shoulve tokenized");
        assert!(matches!(tokens[1].token_type, TokenType::Operator));
    }

    #[test]
    fn deref_is_operator() {
        let tokens = tokenize("
            *i
        ").expect("Shoulve tokenized");
        assert!(matches!(tokens[1].token_type, TokenType::Operator));
    }

    #[test]
    fn test_function_call() {
        let source = "f(1, 2, 3)";
        let tokens = tokenize(source).expect("Should've been able to tokenize");
        assert_eq!(tokens.len(), 10);
        assert!(matches!(tokens[1].token_type, TokenType::Identifier    ));
        assert!(matches!(tokens[2].token_type, TokenType::Punctuation   ));
        assert!(matches!(tokens[3].token_type, TokenType::IntegerLiteral));
        assert!(matches!(tokens[4].token_type, TokenType::Punctuation   ));
        assert!(matches!(tokens[5].token_type, TokenType::IntegerLiteral));
        assert!(matches!(tokens[6].token_type, TokenType::Punctuation   ));
        assert!(matches!(tokens[7].token_type, TokenType::IntegerLiteral));
        assert!(matches!(tokens[8].token_type, TokenType::Punctuation   ));
    }

    #[test]
    fn test_type_definition() {
        let source = "var x: Int = 1";
        let tokens = tokenize(source).expect("Should've been able to tokenize");
        assert_eq!(tokens.len(), 8);
        assert!(matches!(tokens[1].token_type, TokenType::Keyword       ));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier    ));
        assert!(matches!(tokens[3].token_type, TokenType::Punctuation   ));
        assert!(matches!(tokens[4].token_type, TokenType::Identifier    ));
        assert!(matches!(tokens[5].token_type, TokenType::Operator      ));
        assert!(matches!(tokens[6].token_type, TokenType::IntegerLiteral));
    }

    #[test]
    fn identifier_can_start_with_keyword() {
        let tokens = tokenize("
            double_big_mac
            iffy
            whiley
            funny
            variable
            andy
            oreo
            nothing
            newcastle
            deleted
        ").expect("Shoulve tokenized");
        for t in tokens[1..6].iter() {
            assert!(matches!(t.token_type, TokenType::Identifier));
        }
    }

    #[test]
    fn new_and_delete() {
        let tokens = tokenize("
            var x: Int* = new Int(123);
            delete x;
        ").expect("Shoulve tokenized");

        assert_eq!(tokens[7].token_type, TokenType::Operator);
        assert_eq!(tokens[13].token_type, TokenType::Operator);   
    }

    #[test]
    fn struct_declaration_tokenizes() {
        let tokens = tokenize("
            struct Point { x: Int, y: Int }
        ").expect("Shoulve tokenized");

        assert_eq!(tokens[1].token_type, TokenType::Keyword);   
    }

    #[test]
    fn member_variable_access() {
        let tokens = tokenize("
            p.v.x;
        ").expect("Shoulve tokenized");

        assert_eq!(tokens[2].token_type, TokenType::Operator);   
    }

    #[test]
    fn single_line_comment() {
        let tokens = tokenize("
            # Hello!
            1
            // Hello
        ").expect("Shoulve tokenized");

        assert_eq!(tokens.len(), 3); // 3 = "{", "1", "}"   
    }
}
