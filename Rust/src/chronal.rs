use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;

fn parse_num(s: &str) -> i32 {
    let result: i32 = s.parse().expect("Invalid number");
    result
}

fn read_numbers() -> std::vec::Vec<i32> {
    let mut f = File::open("01-chronal-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    Vec::from_iter(contents.lines().map(parse_num))
}

pub fn chronal_part1() {
    let numbers = read_numbers();

    let mut frequency = 0;

    for change in numbers {
        frequency = frequency + change;
    }

    println!("Part1 result: {}", frequency);
}

pub fn chronal_part2() {
    let numbers = read_numbers();
    let mut frequencies_seen: HashSet<i32> = HashSet::new();

    let mut frequency = 0;

    loop {
        for change in &numbers {
            if frequencies_seen.contains(&frequency) {
                println!("Part2 result: {}", frequency);
                return;
            }

            frequencies_seen.insert(frequency);

            frequency = frequency + change;
        }
    }
}
