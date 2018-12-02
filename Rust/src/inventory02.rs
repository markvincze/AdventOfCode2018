use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;

fn read_lines() -> std::vec::Vec<std::string::String> {
    let mut f = File::open("02-inventory-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    Vec::from_iter(contents.lines().map(|s| String::from(s)))
}

pub fn inventory_part1() {
    let lines = read_lines();

    let mut twos = 0;
    let mut threes = 0;

    for line in &lines {
        let mut letter_count: HashMap<char, i32> = HashMap::new();

        for char in line.chars() {
            letter_count
                .entry(char)
                .and_modify(|c| *c += 1)
                .or_insert(1);
        }

        if letter_count.values().any(|v| *v == 2) {
            twos += 1;
        }

        if letter_count.values().any(|v| *v == 3) {
            threes += 1;
        }
    }

    println!("Part1 result: {}", twos * threes);
}

pub fn inventory_part2() {
    let lines = read_lines();

    for line1 in &lines {
        for line2 in &lines {
            let diff = count_diff(line1, line2);

            if diff == 1 {
                let solution = common_part(line1, line2);
                println!("Part 2 result: {}", solution);
                return
            }
        }
    }
}

fn count_diff(s1: &str, s2: &str) -> usize {
    s1.chars()
        .zip(s2.chars())
        .filter(|(c1, c2)| c1 != c2)
        .count()
}

fn common_part(s1: &str, s2: &str) -> std::string::String {
    s1.chars()
        .zip(s2.chars())
        .filter(|(c1, c2)| c1 == c2)
        .map(|(c1, _)| c1)
        .collect::<String>()
}

