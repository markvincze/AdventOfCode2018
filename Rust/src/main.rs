extern crate regex;

mod chronal01;
mod inventory02;
mod fabric03;
mod repose04;

fn main() {
    let day = 4;

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
        4 => {
            repose04::repose();
        }
        _ => println!("Not done yet :)"),
    }
}

