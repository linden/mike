#![allow(dead_code)]

use mike::{capture, expand};

pub struct MyBody {
    alive: bool,
    name: String
}

#[expand]
impl MyBody {
    fn new(name: String, alive: bool) -> MyBody {
        MyBody {
            name: name,
            alive: alive
        }
    }

    fn get_alive(&self) -> bool {
        self.alive
    }

    fn what_name(&self) -> String {
        self.name.clone()
    }
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