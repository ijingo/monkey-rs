use std::io;
use crate::lexer::Lexer;
use crate::lexer::token::Token;

pub fn start<R, W>(mut reader: R, mut writer: W) -> io::Result<()> 
    where R: io::BufRead,
          W: io::Write,
{
    loop {
        writer.write(b">> ")?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;

        let mut lexer = Lexer::new(&line);
        loop {
            let tok = lexer.next_token();
            if tok == Token::EOF {
                break;
            } else {
                let output = format!("{}\n", tok);
                writer.write(output.as_bytes())?;
                writer.flush()?;
            }
        }
    }

}