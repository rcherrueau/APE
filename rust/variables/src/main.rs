use std::char;
use std::char::EscapeDefault;

fn main() {
    let x = 5;
    println!("The value of x is {}", x);

    // You can shadow an immutable variable by a mutable one
    let mut x = x;
    println!("The value of x is {}", x);
    x = 6;
    println!("The value of x is {}", x);

    // You can shadow an immutable variable
    let x = x;
    let x = x * 2;
    println!("The value of x is {}", x);

    // Constant cannot be set to the result of a function
    const MAX_POINT: u32 = 100_000;
    println!("A constant such as {} can be defined everywhere", MAX_POINT);

    // tuple constructor/destructor
    let tup = (500, 6.4, '🐵');
    let (x, y, z) = tup;
    println!("A tuple destructed as ({}, {}, {})", x, y, z);

    // // Functions
    // another_funtion(z, {
    //     let z_iter: EscapeDefault = z.escape_default();
    //     let z_iter: IntoIterator = z_iter;
    //     //z_iter.collect::<String>().iter();
    //     // let mut z_iter = std::char::decode_utf8(z.escape_default());
    //     // let z = z_iter.next();
    //     // z.unwrap_or('🐵')
    //     'a'
    // });
}

fn another_funtion(x: char, y: char) {
    println!("{} Another function {}", x, y);
}
