use std::string::ToString;

#[derive(Clone, Debug)]
pub struct Message {
    message: Option<String>,
    position: Option<(usize, usize)>,
    msg_kind: MsgKind,
    filename: Option<String>,
    formatted_code_line: Option<String>
}

#[derive(Clone, Copy, Debug)]
pub enum MsgKind {
    ModuleError,
    NameError,
    SyntaxError,
    TypeError,
    UnimplementedError
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = format!("woah:: {} ", self.msg_kind);

        string = format!("{}{}", string,
            match (&self.filename, &self.position) {
                (Some(fname), Some((l, c))) => format!("in {}:{}:{}\n", fname, l, c),
                (Some(fname), None) => format!("in {}\n", fname),
                (None, Some((l, c))) => format!("in <unknown file>:{}:{}\n", l, c),
                _ => String::from("\n")
            }
        );

        if let Some(msg) = &self.message {
            string = format!("{}{}\n", string, msg);
        }

        if let Some(line) = &self.formatted_code_line {
            string = format!("{}{}\n", string, line);
        }

        write!(f, "{}", string)
    }
}


/* Conversions */

impl Into<Vec<Message>> for Message {
    fn into(self) -> Vec<Message> {
        vec![ self ]
    }
}


impl<T> Into<Result<T, Message>> for Message {
    fn into(self) -> Result<T, Message> {
        Err(self)
    }
}


impl<T> Into<Result<T, Vec<Message>>> for Message {
    fn into(self) -> Result<T, Vec<Message>> {
        Err(vec![ self ])
    }
}


/* Display */

impl std::fmt::Display for MsgKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            MsgKind::ModuleError        => "Module Error",
            MsgKind::NameError          => "Name Error",
            MsgKind::SyntaxError        => "Syntax Error",
            MsgKind::TypeError          => "Type Error",
            MsgKind::UnimplementedError => "Unimplemented Error"
        })
    }
}


impl Message {
    pub fn new(msg_kind: MsgKind) -> Message {
        /* Returns an initialised message */

        Message {
            message: None,
            position: None,
            msg_kind,
            filename: None,
            formatted_code_line: None
        }
    }


    pub fn res<T>(self) -> Result<T, Message> {
        /* Moves self into a Result */
        Err(self)
    }


    /* Set attributes */

    pub fn set_filename<T: ToString>(mut self, filename: &T) -> Self {
        /* Sets the error's filename */

        if let None = self.filename {
            self.filename = Some(filename.to_string().clone());
        }

        self
    }


    pub fn set_line<T: ToString>(mut self, lines: &Vec<T>) -> Self {
        /* Sets the line the message occurs on, with formatting */

        if let None = self.formatted_code_line {
            if let Some((line_number, col_number)) = self.position {
                if line_number < lines.len() + 1 {
                    let line = &lines[line_number - 1];
                    let line_number_string: String = line_number.to_string();
                    let line_number_buffer: String = vec![' '; line_number_string.len()].into_iter().collect();
                
                    let indicator_buffer: String = vec![' '; col_number].into_iter().collect();

                    self.formatted_code_line = Some(format!(
                       " {} |\n \
                         {} | {}\n \
                         {} |{}^^^",
                        line_number_buffer,
                        line_number_string, line.to_string(),
                        line_number_buffer, indicator_buffer
                    ));
                }
            }
        }

        self
    }


    pub fn set_message<T: std::string::ToString>(mut self, msg: T) -> Self {
        /* Sets the message string */

        if let None = self.message {
            self.message = Some(msg.to_string());
        }

        self
    }


    pub fn set_position(mut self, pos: (usize, usize)) -> Self {
        /* Sets the message's position */

        if let None = self.position {
            self.position = Some(pos);
        }

        self
    }
}

