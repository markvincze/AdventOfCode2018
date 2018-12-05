use std::fs::File;
use std::io::prelude::*;
use std::vec::Vec;

fn read_input() -> std::string::String {
    let mut f = File::open("05-alchemical-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    // I don't know why this is needed, but without this there will be 2 extra spaces at the end of the string.
    contents.chars().take(50000).collect()
}

pub fn alchemical() {
    let input_string = read_input();

    let mut units: Vec<char> = input_string.chars().collect();

    // Part 1
    trigger_until_can(&mut units);
    println!("Part 1 result: {}", units.len());

    // Part 2
    let unit_types = "abcdefghijklmnopqrstuvwxyz".chars();

    let result2 = unit_types
        .map(|unit_to_exclude| {
            let mut filtered_units: Vec<char> = input_string
                .chars()
                .filter(|u| *u != unit_to_exclude && opposite(*u) != unit_to_exclude)
                .collect();

            trigger_until_can(&mut filtered_units);

            filtered_units.len()
        }).min()
        .unwrap();

    println!("Part 2 result: {}", result2);
}

fn trigger_until_can(units: &mut Vec<char>) {
    let mut current_index = 0;

    loop {
        if units.len() == 0 || current_index == units.len() - 1 {
            break;
        } else if units[current_index] == opposite(units[current_index + 1]) {
            units.drain(current_index..current_index + 2);
            if current_index > 0 {
                current_index -= 1;
            }
        } else {
            current_index += 1;
        }
    }
}

fn opposite(c: char) -> char {
    if c.is_lowercase() {
        c.to_ascii_uppercase()
    } else {
        c.to_ascii_lowercase()
    }
}

