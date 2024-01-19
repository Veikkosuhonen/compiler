use regex::Regex;

#[derive(PartialEq, Eq, Debug)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TokenType {
    Identifier,
    IntegerLiteral,
    Operator,
    Punctuation,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub location: SourceLocation,
}

fn count_line_changes(v: &str) -> usize {
    v.chars().filter(|s| s.eq(&'\n')).count()
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let whitespace_regex: Regex = Regex::new(r"^\s+").unwrap();
    let line_comment_regex: Regex = Regex::new(r"^//.*(\n|$)").unwrap();
    let identifier_regex: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let integer_literal_regex: Regex = Regex::new(r"^[0-9]+").unwrap();
    let operator_regex: Regex = Regex::new(r"^(==|!=|<=|>=|\+|-|\*|/|=|<|>|&)").unwrap();
    let punctuation_regex: Regex = Regex::new(r"^(\(|\)|\{|\}|,|;|:)").unwrap();

    let mut tokens: Vec<Token> = Vec::new();

    let mut line = 1;

    let mut current_start = 0;

    while current_start < source.len() {
        let slice = &source[current_start..];

        if let Some(m) = whitespace_regex.find(slice) {
                    current_start += m.end();
            line += count_line_changes(m.as_str());

        } else if let Some(m) = line_comment_regex.find(slice) {
            current_start += m.end();
            line += count_line_changes(m.as_str());

        } else if let Some(m) = identifier_regex.find(slice) {
            tokens.push(Token {
                value: m.as_str().to_owned(),
                token_type: TokenType::Identifier,
                location: SourceLocation {
                    column: 0,
                    line,
                }
            });
            current_start += m.end();

        } else if let Some(m) = integer_literal_regex.find(slice) {
            tokens.push(Token {
                value: m.as_str().to_owned(),
                token_type: TokenType::IntegerLiteral,
                location: SourceLocation {
                    column: 0,
                    line,
                }
            });
            current_start += m.end();

        } else if let Some(m) = operator_regex.find(slice) {
            tokens.push(Token {
                value: m.as_str().to_owned(),
                token_type: TokenType::Operator,
                location: SourceLocation {
                    column: 0,
                    line,
                }
            });
            current_start += m.end();

        } else if let Some(m) = punctuation_regex.find(slice) {
            tokens.push(Token {
                value: m.as_str().to_owned(),
                token_type: TokenType::Punctuation,
                location: SourceLocation {
                    column: 0,
                    line,
                }
            });
            current_start += m.end();
        } else {
            panic!("Unmatched string {}", slice);
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integers_and_literals() {
        let source = "
            if 3 while
            123 123
        ";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].value, "if");

        assert_eq!(tokens[1].token_type, TokenType::IntegerLiteral);
        assert_eq!(tokens[1].value, "3");

        assert_eq!(tokens[2].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].value, "while");

        assert_eq!(tokens[0].location.line, 2);
        assert_eq!(tokens[0].location.column, 0);

        assert_eq!(tokens[1].location.line, 2);
        assert_eq!(tokens[1].location.column, 0);

        assert_eq!(tokens[2].location.line, 2);
        assert_eq!(tokens[2].location.column, 0);

        assert_eq!(tokens[3].location.line, 3);
        assert_eq!(tokens[3].location.column, 0);

        assert_eq!(tokens[4].location.line, 3);
        assert_eq!(tokens[4].location.column, 0);
    }

    #[test]
    fn test_operators() {
        let source = "+ - * / = == != < <= > >=";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 11);

        assert_eq!(tokens[10].token_type, TokenType::Operator);
    }

    #[test]
    fn test_punctuation() {
        let source = "( ) { } , ;";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 6);

        assert_eq!(tokens[5].token_type, TokenType::Punctuation);
    }

    #[test]
    fn test_comment() {
        let source = "( ) { } , ; // comment is fine";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 6);

        assert_eq!(tokens[5].token_type, TokenType::Punctuation);
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
        assert_eq!(tokens[tokens.len() - 1].location.line, 11);
    }

}
