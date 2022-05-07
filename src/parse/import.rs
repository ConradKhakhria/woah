use crate::{
    error::{ Error, ErrorKind },
    line::Line,
    token::Token
};

use derive_getters::Getters;

#[derive(Getters)]
pub struct Import<'s> {
    path: Vec<&'s str>,
    mod_name: &'s str,
}


impl<'s> Import<'s> {
    pub fn from_line<'t>(line: &Line<'s, 't>) -> Result<Self, Vec<Error>> {
        /* Parses an import from a line */

        let path_tokens = &line.line_tokens[1..];
        let mut path = vec![];
        let mut index = 0;

        // lands on a new path element each time
        loop {
            if let Token::Identifier { string, .. } = path_tokens[index] {
                path.push(string);
                index += 1;
            } else {
                return Error::new(ErrorKind::SyntaxError)
                            .set_position(path_tokens[index].position())
                            .set_message("Expected identifier in module path")
                            .into();
            }

            if index == path_tokens.len() {
                break;
            } else if path_tokens[index].to_string() != "." {
                return Error::new(ErrorKind::SyntaxError)
                            .set_position(path_tokens[index].position())
                            .set_message("Expected '.' delimiter in module path")
                            .into();
            } else {
                index += 1;
            }
        }

        if path.len() == 0 {
            Error::new(ErrorKind::SyntaxError)
                .set_position(path_tokens[0].position())
                .set_message("received empty import")
                .into()
        } else if line.line_derivs.len() != 0 {
            Error::new(ErrorKind::SyntaxError)
                .set_position(path_tokens[0].position())
                .set_message("An import cannot have a block")
                .into()
        } else {
            Ok(Import {
                mod_name: path.last().unwrap(),
                path
            })
        }
    }
}
