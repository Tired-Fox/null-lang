use null::{lex::Tokenizer, source, Error};

/*
/**
*  This is a doc comment for `main`
*/
main :: fn() {
    /// This is invalid... must contain a value.
    _ := "`";
    rune := '\u{1F60A}';
    message := "hello, world! \u{1F60A}";
    print(message)
}
*/

fn main() -> Result<(), Error> {
    //let source = r#"321.12e-10"#.to_string();

    let path = std::path::PathBuf::from("assets/null/goal.nl");
    let source = std::fs::read_to_string(&path).unwrap();

    let tokenizer = Tokenizer::new(source.as_str());

    for token in tokenizer {
        match token {
            Ok(token) => println!("{:?}", token),
            Err(e) => println!("{:?}", e.with_source_code(source!(&path, &source))),
        }
    }
    Ok(())
}
