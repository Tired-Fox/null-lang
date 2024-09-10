use null::{error::Error, lex::Tokenizer, source};

fn main() -> Result<(), Error> {
    let src = r#"
main :: fn() {
    // This is invalid... must contain a value.
    _ := "`";
    rune := '\u{1F60AAA}';
    message := "hello, world! \u{1F60A}";
    print(message)
}
"#;

    let tokenizer = Tokenizer::new(src);

    for token in tokenizer {
        println!("{:?}", token.map_err(|e| e.with_source_code(source!(src)))?)
    }
    Ok(())
}
