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
    }
}

impl<'s> Token<'s> {
    pub fn str_equals(&self, s: &str) -> bool {
        /* Returns whether the token's string is equal to a supplied string */

        match self {
            Token::Identifier    { string, .. } => *string == s,
            Token::Number        { string, .. } => *string == s,
            Token::String        { string, .. } => *string == s,
            Token::Symbol        { string, .. } => *string == s,
            _ => false
        }
    }


    pub fn delim_equals(&self, s: &str) -> bool {
        /* Returns whether the block's delimiter is equal to a supplied string */

        if let Token::Block { open_delim, .. } = self {
            *open_delim == s
        } else {
            false
        }
    }


    pub fn atom(&self) -> bool {
        /* Returns whether the token is atomic */

        if let Token::Block {..} = self {
            false
        } else {
            true
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
        let s = match self {
            Token::Identifier { string, .. } => string,
            Token::Number     { string, .. } => string,
            Token::String     { string, .. } => string,
            Token::Symbol     { string, .. } => string,
            Token::Block {..} => ""
        };
    
        write!(f, "{}", s)
    }
}
