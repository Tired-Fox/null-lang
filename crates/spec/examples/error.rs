use spec::{error::Result, lexer::Tokenizer};

static SOURCE: &str = r#" 
 >="#;

fn main() -> Result<()> {
    let mut tokenizer = Tokenizer::new(SOURCE.bytes());

    println!("{:?}", tokenizer.next_token()?);

    Ok(())
}
