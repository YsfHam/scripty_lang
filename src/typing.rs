#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ExpressionType {
    Int,
    Bool,
    Void,
    Unresolved,
}

impl ExpressionType {
    pub fn to_string(self) -> String {
        let type_as_str = match self {
            ExpressionType::Int => "int",
            ExpressionType::Bool => "bool",
            ExpressionType::Void => "void",
            ExpressionType::Unresolved => "?",
        };

        type_as_str.to_string()
    }
}