#[derive(Clone, Debug)]
pub struct Error {
    message: Option<String>,
    position: Option<(usize, usize)>,
    error_kind: ErrorKind,
    filename: Option<String>,
    formatted_code_line: Option<String>
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    NameError,
    SyntaxError,
    TypeError,
    UnimplementedError
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = format!("woah:: {} ", self.error_kind);

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

impl Into<Vec<Error>> for Error {
    fn into(self) -> Vec<Error> {
        vec![ self ]
    }
}


impl<T> Into<Result<T, Error>> for Error {
    fn into(self) -> Result<T, Error> {
        Err(self)
    }
}


impl<T> Into<Result<T, Vec<Error>>> for Error {
    fn into(self) -> Result<T, Vec<Error>> {
        Err(vec![ self ])
    }
}


/* Display */

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ErrorKind::NameError          => "Name Error",
            ErrorKind::SyntaxError        => "Syntax Error",
            ErrorKind::TypeError          => "Type Error",
            ErrorKind::UnimplementedError => "Unimplemented Error"
        })
    }
}


impl Error {
    pub fn new(error_kind: ErrorKind) -> Error {
        /* Returns an initialised error */

        Error {
            message: None,
            position: None,
            error_kind,
            filename: None,
            formatted_code_line: None
        }
    }


    pub fn res<T>(self) -> Result<T, Error> {
        /* Moves self into a Result */
        Err(self)
    }


    /* Set attributes */

    pub fn set_filename<T: std::string::ToString>(mut self, filename: &T) -> Self {
        /* Sets the error's filename */

        if let None = self.filename {
            self.filename = Some(filename.to_string().clone());
        }

        self
    }


    pub fn set_line(mut self, lines: &Vec<String>) -> Self {
        /* Sets the line the error occurs on, with formatting */

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
                        line_number_string, line,
                        line_number_buffer, indicator_buffer
                    ));
                }
            }
        }

        self
    }


    pub fn set_message<T: std::string::ToString>(mut self, msg: T) -> Self {
        /* Sets the error's message */

        if let None = self.message {
            self.message = Some(msg.to_string());
        }

        self
    }


    pub fn set_position(mut self, pos: (usize, usize)) -> Self {
        /* Sets the error's position */

        if let None = self.position {
            self.position = Some(pos);
        }

        self
    }
}

