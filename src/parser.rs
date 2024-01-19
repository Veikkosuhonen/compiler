pub struct Expression {
    value: String,
}

pub struct Literal {
    pub value: String,
}

pub struct Identifier {
    pub value: String,
}

pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}
