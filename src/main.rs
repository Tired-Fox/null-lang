use null::{Error, lex::Tokenizer, source};

fn main() -> Result<(), Error> {
    let src = r#"
/**
*  This is a doc comment for `main`
*/
main :: fn() {
    /// This is invalid... must contain a value.
    _ := "`";
    rune := '\u{1F60AAA}';
    message := "hello, world! \u{1F60A}";
    print(message)
}
"#;

    let tokenizer = Tokenizer::new(src);

    for token in tokenizer {
        match token {
            Ok(token) => println!("{:?}", token),
            Err(e) => println!("{:?}", e.with_source_code(source!(src))),
        }
    }
    Ok(())
}
