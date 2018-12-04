extern crate regex;

mod chronal01;
mod inventory02;
mod fabric03;

fn main() {
    let day = 3;

    match day {
        1 => {
            chronal01::chronal_part1();
            chronal01::chronal_part2();
        }
        2 => {
            inventory02::inventory_part1();
            inventory02::inventory_part2();
        }
        3 => {
            fabric03::fabric();
        }
        _ => println!("Not done yet :)"),
    }
}

