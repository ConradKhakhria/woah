use crate::{
    error::{ Error, ErrorKind },
    token::Token
};
use lazy_static::lazy_static;
use regex::Regex;

struct Lexer<'f> {
    filename: &'f str,
    line_no: usize,
    col_no: usize
}

impl<'f> Lexer<'f> {
    fn create_list_token<'s>(&mut self, open_delim: &'s str, token_stack: &mut Vec<Token<'s>>) -> Result<(), Error> {
        /* Creates a Token::List and adds it to the token stack */
        token_stack.push(Token::Block {
            open_delim,
            contents: Vec::new(),
            position: (self.line_no, self.col_no)
        });

        Ok(())
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
        if token_string.chars().next().unwrap() == '#' || token_string == "\n" {
            self.line_no += 1;
            self.col_no   = 1;
            return Ok(());
        }

        self.col_no += token_string.len();

        match token_string {
            "{"|"("|"[" => self.create_list_token(token_string, token_stack),
            "}"|")"|"]" => self.end_list_token(token_string, token_stack),
            " " => Ok(()),
            _   => self.match_general_token(token_string, token_stack)
        }
    }


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


    fn split_string<'s>(&self, source: &'s str) -> impl Iterator<Item = &'s str> {
        /* Splits a source string according to a regex */
        lazy_static! {
            static ref REGEX: Regex = Regex::new(concat!(
                "#.*?\n|",
                r"[a-zA-Z_][a-zA-Z0-9_]*|",
                r"[0-9_]*[0-9]\.[0-9][0-9_]*|",
                r"0[bB][01_]+|0[xX][0-9a-fA-F_]+|[0-9][0-9_]*|",
                "\".*?\"|\'.*?\'|\n|",
                r"=>|\->|<=|>=|<|>|;|:|,|\.|@w\s|@|\{|\}|\(|\)|\[|\]|",
                r"==|!=|<=|>=|\+=|\-=|\*=|/=|%=|\*\*|\++|\+|\-|\*|/|%|=|\s"
            )).unwrap();
        }

        REGEX.find_iter(source)
             .map(|m| m.as_str())
             .filter(|s| s.len() > 0)
    }
}

pub fn tokenise<'s, 'f>(source: &'s str, filename: &'f str, pos: (usize, usize)) -> Result<Vec<Token<'s>>, Vec<Error>> {
    /* Tokenises a source string */
    let mut tokeniser = Lexer { filename, line_no: pos.0, col_no: pos.1 };
    let token_strings = tokeniser.split_string(source);
    let nested_tokens = tokeniser.build_nested(source, token_strings)?;

    match &nested_tokens[..] {
        [Token::Block { contents, .. }] => Ok(contents.clone()),
        _ => unreachable!()
    }
}
