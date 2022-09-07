use crate::token::Token;

#[derive(Debug)]
pub struct Line<'s, 't> {
    pub line_tokens: &'t [Token<'s>],
    pub line_derivs: Vec<Line<'s, 't>>
}


impl<'s, 't> Line<'s, 't> {
    pub fn first_position(&self) -> (usize, usize) {
        /* Gets the position of the first token in the line */

        self.line_tokens.first().unwrap().position()
    }


    pub fn last_position(&self) -> (usize, usize) {
        /* Gets the position of the last token in the line */

        match self.line_derivs.last() {
            Some(last_deriv) => last_deriv.last_position(),
            None => self.line_tokens.last().unwrap().position()
        }
    }
}


fn create_indentation_tuples<'s, 't>(tokens: &'t [Token<'s>]) -> Vec<(&'t [Token<'s>], usize)> {
    /* Creates a list of (line, indent) tuples */

    let mut tuples = vec![];
    let mut prev = 0;
    let mut current_indent = 0;

    for (i, token) in tokens.iter().enumerate() {
        if let Token::NewlineIndent(next_indent) = token {
            let line = if prev == 0 {
                &tokens[prev..i]
            } else {
                &tokens[prev + 1..i]
            };

            tuples.push((line, current_indent));

            prev = i;
            current_indent = *next_indent;
        }
    }

    tuples
}


fn create_nested_lines<'s, 't>(tuples: &[(&'t [Token<'s>], usize)]) -> Vec<Line<'s, 't>> {
   /* Turns a list of indentation tuples into a list of Line's */

    let mut lines = vec![];
    let mut index = 0;

    // lands on each line of the indent-level of the first line
    while index < tuples.len() {
        let mut next_level_line_index = index + 1;

        while next_level_line_index < tuples.len() {
            if tuples[next_level_line_index].1 == tuples[index].1 {
                break;
            } else {
                next_level_line_index += 1;
            }
        }

        let inner_lines = create_nested_lines(&tuples[index + 1..next_level_line_index]);
        lines.push(
            Line {
                line_tokens: tuples[index].0,
                line_derivs: inner_lines
            }
        );

        index = next_level_line_index;
    }

    lines
}


pub fn create_lines<'s, 't>(tokens: &'t [Token<'s>]) -> Vec<Line<'s, 't>> {
    /* Creates a list of lines from a list of tokens */

    let indentation_tuples = create_indentation_tuples(tokens);

    create_nested_lines(&indentation_tuples[..])
}


