#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Null,
}
