// Cannot re-assign value and cannot be changed. Allowed in global scope

// Types:
// i8 i16 i32 i64 i128 (int == ptr to the best fit size, similar to isize in rust)
// u8 u16 u32 u64 u128 (uint == ptr to the best fit size, similar to usize in rust)
//    f16 f32 f64      (float == ptr to the best fit size)
// string
// any
// undefined

// Static: static variable: type = undefined
// Immutable: const variable: type : undefined
//      variable :: undefined
// Mutable: let variable: type = undefined
//      variable := undefined

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

pub enum Error {
    UnkownError,
    OptionError(String)
}

union_enum :: fn() Error!?String {
    // Can re-assign value
    let x: i32 = 0;
    
    // Can mutate value
    let x: i32 = 0;
    let my_option = Option { Day: Weekday.Monday };
}

pub template SayHello {
    say_hello::fn(self: &Self);
}

struct Data {
    name: String

    _::fn(self) {
        /* Deconstructor: Called on dropping self */
    }

    name::fn(&self) &String {
        &self.name
    }

    hello::fn() {
        print("Hello")
    }
}

extend Data with SayHello {
    say_hello::fn(&self) {
        let suffix = '!'
        print("Hello, {}{}", self.name, suffix)
    }
}

math :: fn(name: String, ..args: []String) {
  let result: int = 13*4+6/5%((4**4)//3)-3;

  if true and false or !false {
  }
}

optional :: (code: i32?) bool {
    false
}

main :: fn() {
    print("Hello, world!");
    print("Hello, {}!", Sub::NAME);
    let value: []string = [];
    let slice: &[]string = value[..3];
}
