use crate::{
    error::{ Error, ErrorKind },
    token::Token,
    parse::{
        Function,
        TypeKind
    }
};

pub struct Class<'s, 't> {
    pub name: &'t Token<'s>,
    pub public_fields: Vec<Field<'s, 't>>,
    pub private_fields: Vec<Field<'s, 't>>,
    pub public_methods: Vec<Function<'s, 't>>,
    pub private_methods: Vec<Function<'s, 't>>
}


impl<'s, 't> Class<'s, 't> {
    pub fn new
}


#[derive(Debug)]
pub struct Field<'s, 't> {
    pub field_name: &'t Token<'s>,
    pub field_type: TypeKind<'s, 't>
}

impl<'s, 't> Field<'s, 't> {
    fn from_tokens(tokens: &'t [Token<'s>]) -> Result<(Field<'s, 't>, bool), Vec<Error>> {
        /* Parses an argument */

        let offset = if tokens[0].to_string() == "pub" { 1 } else { 0 };

        let field_name = &tokens[0 + offset];

        if let Token::Identifier {..} = field_name { } else {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0 + offset].position())
                        .set_message("Expected an argument name")
                        .into();
        }

        if tokens.len() < 3 + offset || tokens[1 + offset].to_string() != ":" {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0 + offset].position())
                        .set_message("Expected syntax <argument name> : <argument type>")
                        .into();
        }

        let field_type = TypeKind::from_tokens(&tokens[2..])?;

        Ok((Field { field_name, field_type }, offset == 1))
    }
}

