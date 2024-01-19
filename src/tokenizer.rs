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

pub fn tokenize(source: &str) -> Vec<Token> {
    let identifier_regex: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
    let integer_literal_regex: Regex = Regex::new(r"^[0-9]+$").unwrap();
    let operator_regex: Regex = Regex::new(r"==|!=|<=|>=|\+|-|\*|/|=|<|>").unwrap();
    let punctuation_regex: Regex = Regex::new(r"(|)|\{|\}|,|;").unwrap();

    let mut tokens: Vec<Token> = Vec::new();

    let mut current_token = String::new();

    let mut line = 1;
    let mut column = 0;
    let mut token_start_column = 0;

    for c in source.chars() {
        column += 1;
        if c.is_whitespace() {
            if current_token.len() > 0 {
                tokens.push(Token {
                    token_type: get_token_type(&current_token, &identifier_regex, &integer_literal_regex, &operator_regex, &punctuation_regex),
                    value: current_token.clone(),
                    location: SourceLocation {
                        line,
                        column: token_start_column,
                    },
                });
                current_token.clear();
            }

            if c == '\n' {
                line += 1;
                column = 0;
            }

        } else {
            // If current token is empty, set its start column here
            if current_token.is_empty() {
                token_start_column = column;
            }

            current_token.push(c);
        }
    }

    if current_token.len() > 0 {
        tokens.push(Token {
            token_type: get_token_type(&current_token, &identifier_regex, &integer_literal_regex, &operator_regex, &punctuation_regex),
            value: current_token.clone(),
            location: SourceLocation {
                line,
                column: token_start_column,
            },
        });
    }

    tokens
}

fn get_token_type(
    token: &str, 
    identifier_regex: &Regex, 
    integer_literal_regex: &Regex,
    operator_regex: &Regex,
    punctuation_regex: &Regex,
) -> TokenType {
    if identifier_regex.is_match(token) {
        TokenType::Identifier
    } else if integer_literal_regex.is_match(token) {
        TokenType::IntegerLiteral
    } else if operator_regex.is_match(token) {
        TokenType::Operator
    } else if punctuation_regex.is_match(token) {
        TokenType::Punctuation
    } else {
        panic!("Unrecognized token: {}", token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integers_and_literals() {
        let source = "if 3 while\n123 123";
        let tokens: Vec<Token> = tokenize(source);

        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].value, "if");

        assert_eq!(tokens[1].token_type, TokenType::IntegerLiteral);
        assert_eq!(tokens[1].value, "3");

        assert_eq!(tokens[2].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].value, "while");

        assert_eq!(tokens[0].location.line, 1);
        assert_eq!(tokens[0].location.column, 1);

        assert_eq!(tokens[1].location.line, 1);
        assert_eq!(tokens[1].location.column, 4);

        assert_eq!(tokens[2].location.line, 1);
        assert_eq!(tokens[2].location.column, 6);

        assert_eq!(tokens[3].location.line, 2);
        assert_eq!(tokens[3].location.column, 1);

        assert_eq!(tokens[4].location.line, 2);
        assert_eq!(tokens[4].location.column, 5);
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
}
