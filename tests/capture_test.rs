#![allow(dead_code)]

use mike::capture;

pub struct MyBody {
    alive: bool,
    name: String
}

#[capture]
fn greet(name: String) -> String {
    format!("greet {}", name)
}

#[capture]
fn custom_greet(greeting: String, name: String) -> String {
    format!("{} {}", greeting, name)
}

#[capture]
fn add_ten(before: usize) -> usize {
    before + 10
}

#[capture]
fn hello_world() {
    println!("Hello World");
}

#[capture]
fn structure_me(name: String) -> MyBody {
    MyBody {
        alive: true,
        name: name
    }
}

#[capture]
fn optional() -> Option<String> {
    None
}

#[capture]
fn refrence(example: &str) {
    println!("{}", example);
}