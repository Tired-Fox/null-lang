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
