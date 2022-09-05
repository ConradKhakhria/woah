use crate::error::{ Error, ErrorKind };
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Clone, Debug)]
pub enum Token<'s> {
    Identifier {
        string: &'s str,
        position: (usize, usize)
    },

    Number {
        string: &'s str,
        position: (usize, usize)
    },

    String {
        string: &'s str,
        position: (usize, usize)
    },

    Symbol {
        string: &'s str,
        position: (usize, usize)
    },

    Block {
        open_delim: &'s str,
        contents: Vec<Token<'s>>,
        position: (usize, usize)
    },

    NewlineIndent(usize)
}

#[allow(dead_code)]
impl<'s> Token<'s> {
    /* User interface */

    pub fn atom(&self) -> bool {
        /* Returns whether the token is atomic */

        if let Token::Block {..} = self { false } else { true }
    }


    pub fn delim(&self) -> String {
        /* Returns whether the block's delimiter is equal to a supplied string */

        match self {
            Token::Block { open_delim, .. } => open_delim.to_string(),
            _ => String::new()
        }
    }


    pub fn name(&self) -> String {
        /* returns the string of an ident or an empty string */

        match self {
            Token::Identifier { string, .. } => string.to_string(),
            _ => String::new()
        }
    }


    pub fn position(&self) -> (usize, usize) {
        /* Returns the token's position */

        match self {
            Token::Block      { position, .. } => *position,
            Token::Identifier { position, .. } => *position,
            Token::Number     { position, .. } => *position,
            Token::String     { position, .. } => *position,
            Token::Symbol     { position, .. } => *position,
            Token::NewlineIndent(_)                          => (0, 0) // irrelevant as these will be removed
        }
    }


    pub fn split_tokens<'t>(tokens: &'t [Self], pred: fn(&'t Self) -> bool) -> Vec<(usize, usize)> {
        /* Returns a list of slice indices split according to a predicate */

        let mut slices = Vec::new();
        let mut pred_sat_indices = Vec::new();

        for i in 0..tokens.len() {
            if pred(&tokens[i]) {
                pred_sat_indices.push(i);
            }
        }

        pred_sat_indices.push(tokens.len());

        if pred_sat_indices.is_empty() {
            return slices;
        }

        if pred_sat_indices[0] != 0 {
            slices.push((0, pred_sat_indices[0]));
        }

        for i in 1..pred_sat_indices.len() {
            slices.push((pred_sat_indices[i - 1] + 1, pred_sat_indices[i]));
        }

        slices
    }
}


impl<'s> std::fmt::Display for Token<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier { string, .. } => write!(f, "{}", string),
            Token::NewlineIndent(_)                => write!(f, "{}", "<newline indent>"),
            Token::Number     { string, .. } => write!(f, "{}", string),
            Token::String     { string, .. } => write!(f, "{}", string),
            Token::Symbol     { string, .. } => write!(f, "{}", string),
            Token::Block      { open_delim, contents, ..} => {
                let mut string = format!("{}", open_delim);

                for token in contents {
                    string += &token.to_string();
                }

                string += match *open_delim {
                    "(" => ")",
                    "{" => "}",
                     _  => "]"
                };
            
                write!(f, "{}", string)
            }
        }
    }
}


/* The lexer struct
 *
 * This class exists to provide utility functions for the tokenise(). This is not a very clean
 * pattern and will be removed in later versions of the compiler.
 */

struct Lexer<'f> {
    filename: &'f str,
    line_no: usize,
    col_no: usize
}

impl<'f> Lexer<'f> {
    fn build_nested<'s>(&mut self, source: &'s str, token_strings: impl Iterator<Item = &'s str>) -> Result<Vec<Token<'s>>, Vec<Error>> {
        /* Builds nested tokens */
        let mut token_stack = vec![
            Token::Block {
                open_delim:  &source[..0],
                contents: Vec::new(),
                position: (self.line_no, self.col_no)
            }
        ];

        for token_string in token_strings {
            self.match_token_string(token_string, &mut token_stack)
                .map_err(|err| vec![ err ])?;
        }

        Ok(token_stack)
    }


    fn create_list_token<'s>(&mut self, open_delim: &'s str, token_stack: &mut Vec<Token<'s>>) -> Result<(), Error> {
        /* Creates a Token::List and adds it to the token stack */

        token_stack.push(Token::Block {
            open_delim,
            contents: Vec::new(),
            position: (self.line_no, self.col_no)
        });

        Ok(())
    }


    fn create_token_strings<'s>(&self, source: &'s str) -> impl Iterator<Item = &'s str> {
        /* Splits a source string according to a regex */
        lazy_static! {
            static ref REGEX: Regex = Regex::new(concat!(
                "#.*?\n( )*|",
                r"[a-zA-Z_][a-zA-Z0-9_]*|",
                r"[0-9_]*[0-9]\.[0-9][0-9_]*|",
                r"0[bB][01_]+|0[xX][0-9a-fA-F_]+|[0-9][0-9_]*|",
                "\".*?\"|\'.*?\'|\n( )*|",
                r"=>|\->|<=|>=|<|>|;|:|,|\.|@w\s|@|\{|\}|\(|\)|\[|\]|",
                r"==|!=|<=|>=|\+=|\-=|\*=|/=|%=|\*\*|\++|\+|\-|\*|/|%|=|\s"
            )).unwrap();
        }

        REGEX.find_iter(source)
             .map(|m| m.as_str())
             .filter(|s| s.len() > 0)
    }


    fn end_list_token<'s>(&mut self, close_delim: &'s str, token_stack: &mut Vec<Token<'s>>) -> Result<(), Error> {
        /* Pushes the top of the token_stack onto the one below */

        let corresponding_open_delim = match close_delim {
            "}" => "{",
            ")" => "(",
            _   => "["
        };

        let stack_top = token_stack.pop().unwrap();
        let delim = match &stack_top {
            Token::Block { open_delim, .. } => open_delim,
            _ => unreachable!()
        };

        if *delim != corresponding_open_delim {
            let error_message = if *delim == "" {
                "Found closing bracket without corresponding open bracket".into()
            } else {
                format!("Mismatches braces: expected '{}', received '{}", corresponding_open_delim, delim)
            };

            Error::new(ErrorKind::SyntaxError)
                .set_filename(&self.filename)
                .set_position((self.line_no, self.col_no))
                .set_message(error_message)
                .res()?
        }

        match token_stack.last_mut() {
            Some(Token::Block { contents, .. }) => contents.push(stack_top),
            _ => unreachable!()
        };

        Ok(())
    }


    fn match_general_token<'s>(&mut self, token_string: &'s str, token_stack: &mut Vec<Token<'s>>) -> Result<(), Error> {
        /* Matches all non-delimiting tokens */

        let new_token = match token_string.chars().next().unwrap() {
            'a'..='z'|'A'..='Z'|'_' => Token::Identifier {
                string: token_string,
                position: (self.line_no, self.col_no)
            },

            '0'..='9' => Token::Number {
                string: token_string,
                position: (self.line_no, self.col_no)
            },

            '"'|'\'' => Token::String {
                string: token_string,
                position: (self.line_no, self.col_no)
            },

            '#'|'\n' => {
                self.line_no += 1;
                self.col_no   = 1;

                let mut token = Token::NewlineIndent(0); // placeholder

                for (i, c) in token_string.char_indices() {
                    if c == '\n' {
                        token = Token::NewlineIndent(token_string[i+1..].len());
                        self.col_no = token_string[i+1..].len();
                        break;
                    }
                }

                token
            }

            _ => Token::Symbol {
                string: token_string,
                position: (self.line_no, self.col_no)
            }
        };

        if let Token::Block { contents, .. } = token_stack.last_mut().unwrap() {
            contents.push(new_token);
            Ok(())
        } else {
            unreachable!()
        }
    }


    fn match_token_string<'s>(&mut self, token_string: &'s str, token_stack: &mut Vec<Token<'s>>) -> Result<(), Error> {
        /* Matches a token string and moves the lexer along */
        self.col_no += token_string.len();

        match token_string {
            "{"|"("|"[" => self.create_list_token(token_string, token_stack),
            "}"|")"|"]" => self.end_list_token(token_string, token_stack),
            " " => Ok(()),
            _   => self.match_general_token(token_string, token_stack)
        }
    }
}

pub fn tokenise<'s, 'f>(source: &'s str, filename: &'f str, pos: (usize, usize)) -> Result<Vec<Token<'s>>, Vec<Error>> {
    /* Tokenises a source string */

    let mut lexer = Lexer { filename, line_no: pos.0, col_no: pos.1 };
    let token_strings = lexer.create_token_strings(source);
    let mut nested_tokens = lexer.build_nested(source, token_strings)?;

    match &mut nested_tokens[..] {
        [Token::Block { contents, .. }] => {
            for inner_token in contents.iter_mut() {
                remove_internal_newlines(inner_token);
            }

            remove_duplicate_newlines(contents);

            adjust_newline_step(contents)?;

            Ok(contents.clone())
        },
        _ => unreachable!()
    }
}


fn adjust_newline_step<'s>(tokens: &mut Vec<Token<'s>>) -> Result<(), Vec<Error>> {
    /* Sets the newline step to 1 in each token */

    let mut newline_step = 0; // will always be a step of one

    for token in tokens.iter() {
        if let Token::NewlineIndent(indent) = token {
            if *indent > 0 {
                newline_step = *indent;
                break;
            }
        }
    }

    if newline_step == 0 {
        return Ok(());
    }

    for token in tokens.iter_mut() {
        if let Token::NewlineIndent(indent) = token {
            if *indent % newline_step == 0 {
                *indent /= newline_step;
            } else {
                return Error::new(ErrorKind::SyntaxError)
                        .set_position(token.position())
                        .set_message("Indentation doesn't match the step of previous indentation levels")
                        .into();
            }
        }
    }

    Ok(())
}


fn remove_duplicate_newlines<'s>(tokens: &mut Vec<Token<'s>>) {
    /* Removes duplicate newlines in a list of tokens */

    let mut is_duplicate = false;

    for i in (0..tokens.len()).rev() {
        match tokens[i] {
            Token::NewlineIndent(_) => {
                if is_duplicate {
                    tokens.remove(i);
                } else {
                    is_duplicate = true;
                }
            },

            _ => is_duplicate = false
        }
    }
}


fn remove_internal_newlines<'s>(token: &mut Token<'s>) {
    /* Recursively removes newlines from nested tokens */

    if let Token::Block { contents, .. } = token {
        for i in (0..contents.len()).rev() {
            match contents[i] {
                Token::Block {..}       => { remove_internal_newlines(&mut contents[i]); },
                Token::NewlineIndent(_) => { contents.remove(i); },
                _ => {}
            }
        }
    }
}
