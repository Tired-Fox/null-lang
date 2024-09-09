// Cannot re-assign value and cannot be changed. Allowed in global scope

// Types:
// i8 i16 i32 i64 i128
// u8 u16 u32 u64 u128
//    f16 f32 f64
// string
// null
// error

// [Static] static variable: type = undefined;
// [Immutable] variable : type : undefined;
//             variable :: undefined
// [Mutable] variable : type = undefined
//           variable := undefined

/*
    Operators:

        Math:

            + - * / % %%
            | ~ & &~ << >>

        Comparison

            == != < <= > >= && || !

        Address
            & ^

        Ternary
           true ? true : false
*/

const x: int = 0
x : int : 0
x :: 0
// Error cannot mutate immutable value
x = 3

let x: i32 = 0
x : i32 = 0
x := 0
x = 4

/*
    Multiline comment `/* markdown code escapes closing symbols */`
*/

/**
  Doc comment: Supports markdown syntax
*/
pub enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
}

/*
    Play with the idea of enums with values
    or unions. Doesn't make sense to have both since both
    would be the same thing.
 */

pub union Option {
    Day: Weekday,
    Week: u32,
    Month: u8,
    Year: u16
}

pub error Error {
    UnkownError,
    OptionError: String
}

union_enum :: fn() Error!?String {
    // Can re-assign value
    let x: i32 = 0;
    
    // Can mutate value
    let mut x: i32 = 0;
    let my_option = Option { Day: Weekday::Monday }
}

pub interface SayHello {
    say_hello :: fn(self: &Self)
}

Data :: struct {
    name: String,

    name :: fn(&self) &String {
        &self.name
    }

    hello :: fn() {
        print("Hello")
    }
}

say_hello :: fn(self: *Data) {
    let suffix = '!';
    print("Hello, {}{}", self.name, suffix)
}

math :: fn(name: String, args: ..String) {
  let result: int = 13*4+6/5%((4**4)//3)-3;

  if true && false || !false {
  }
}

optional :: (code?: i32) bool {
    if (code != null) {}
    if (code) {}
    false
}

main :: fn() {
    print("Hello, world!");
    print("Hello, {}!", Sub::NAME);
    // Slice that is not defined yet
    let value = [3]string{ "Some", "values", "here" };
    let slice: []string = value[..3];
}
