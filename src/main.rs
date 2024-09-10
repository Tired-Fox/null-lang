use null::{lex::Tokenizer, source};

fn main() {
    let src = r#"
main :: fn() {
    // This is invalid... must contain a value.
    ``
    rune := '';
    message := "hello, world!";
    print(message)
}
"#;

    let tokenizer = Tokenizer::new(src);

    for token in tokenizer {
        match token {
            Ok(t) => println!("{t:?}"),
            Err(e) => eprintln!("{:?}", e.with_source_code(source!("main.nl", src))),
        }
    }
}
