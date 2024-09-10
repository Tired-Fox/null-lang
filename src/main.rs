use null::token::Tokenizer;

fn main() {
    let tokenizer = Tokenizer::new(r#"
main :: fn() {
    message := "hello, world!";
    print(message)
}
"#);
    for token in tokenizer {
        println!("{token:?}");
    }
}
