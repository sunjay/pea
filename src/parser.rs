mod scanner;
mod token;
mod lexer;

use crate::{
    source_files::FileSource,
    diagnostics::Diagnostics,
};

use lexer::Lexer;
use scanner::Scanner;
use token::{Token, TokenKind};

pub fn collect_tokens(source: FileSource, diag: &Diagnostics) -> Vec<Token> {
    let scanner = Scanner::new(source);
    let mut lexer = Lexer::new(scanner, diag);

    let mut tokens = Vec::new();
    loop {
        let token = lexer.next();
        if token.kind == TokenKind::Eof {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    tokens
}
