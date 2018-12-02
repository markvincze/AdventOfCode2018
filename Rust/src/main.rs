mod chronal01;
mod inventory02;

fn main() {
    let day = 2;

    match day {
        1 => {
            chronal01::chronal_part1();
            chronal01::chronal_part2();
        }
        2 => {
            inventory02::inventory_part1();
            inventory02::inventory_part2();
        }
        _ => println!("Not done yet :)"),
    }
}

