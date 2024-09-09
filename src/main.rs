use null::token::Tokenizer;

fn main() {
    let tokenizer = Tokenizer::new(" \n  \r\nident");
    for token in tokenizer {
        println!("{token:?}");
    }
}
