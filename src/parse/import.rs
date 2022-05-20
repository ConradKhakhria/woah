use crate::{
    message::{ Message, MsgKind },
    line::Line,
    parse::Module,
    token::Token
};

#[derive(Debug)]
pub struct Import {
    pub path: Vec<String>,
    pub mod_name: String,
    pub module: Module
}


impl Import {
    pub fn from_line(line: &Line) -> Result<Self, Vec<Message>> {
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
                return Message::new(MsgKind::SyntaxError)
                            .set_position(path_tokens[index].position())
                            .set_message("Expected identifier in module path")
                            .into();
            }

            if index == path_tokens.len() {
                break;
            } else if path_tokens[index].to_string() != "." {
                return Message::new(MsgKind::SyntaxError)
                            .set_position(path_tokens[index].position())
                            .set_message("Expected '.' delimiter in module path")
                            .into();
            } else {
                index += 1;
            }
        }

        if path.len() == 0 {
            Message::new(MsgKind::SyntaxError)
                .set_position(path_tokens[0].position())
                .set_message("received empty import")
                .into()
        } else if line.line_derivs.len() != 0 {
            Message::new(MsgKind::SyntaxError)
                .set_position(path_tokens[0].position())
                .set_message("An import cannot have a block")
                .into()
        } else {
            Message::new(MsgKind::UnimplementedError)
                .set_position(line.line_tokens[0].position())
                .set_message("I haven't implemented imports yet")
                .into()
        }
    }
}
