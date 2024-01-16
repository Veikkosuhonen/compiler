use regex::Regex;

#[derive(PartialEq, Eq, Debug)]
pub enum TokenType {
    Identifier,
    IntegerLiteral,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let identifier_regex: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
    let integer_literal_regex: Regex = Regex::new(r"^[0-9]+$").unwrap();

    let mut tokens: Vec<Token> = Vec::new();

    let mut current_token = String::new();

    for c in source.chars() {
        if c.is_whitespace() {
            if current_token.len() > 0 {
                tokens.push(Token {
                    token_type: get_token_type(&current_token, &identifier_regex, &integer_literal_regex),
                    value: current_token.clone(),
                });
                current_token.clear();
            }
        } else {
            current_token.push(c);
        }
    }

    if current_token.len() > 0 {
        tokens.push(Token {
            token_type: get_token_type(&current_token, &identifier_regex, &integer_literal_regex),
            value: current_token.clone(),
        });
    }

    tokens
}

fn get_token_type(
    token: &str, 
    identifier_regex: &Regex, 
    integer_literal_regex: &Regex
) -> TokenType {
    if identifier_regex.is_match(token) {
        TokenType::Identifier
    } else if integer_literal_regex.is_match(token) {
        TokenType::IntegerLiteral
    } else {
        panic!("Unrecognized token: {}", token);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let source = "if 3 while";
        let tokens = tokenize(source);

        assert_eq!(tokens.len(), 3);

        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].value, "if");

        assert_eq!(tokens[1].token_type, TokenType::IntegerLiteral);
        assert_eq!(tokens[1].value, "3");

        assert_eq!(tokens[2].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].value, "while");
    }
}