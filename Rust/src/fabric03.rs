use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use regex::Regex;

struct Claim {
    id: u32,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

type SquareOccupancy = u8;

const OCCUPANCY_NONE: u8 = 0;
const OCCUPANCY_ONE: u8 = 1;
const OCCUPANCY_MORE: u8 = 2;

fn parse_line (line: &str) -> Claim {
    let re = Regex::new(r"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$").unwrap();

    let cap = re.captures(line).expect("Regex did not match");

    Claim {
        id: cap[1].parse().expect("Incorrect value"),
        x: cap[2].parse().expect("Incorrect value"),
        y: cap[3].parse().expect("Incorrect value"),
        width: cap[4].parse().expect("Incorrect value"),
        height: cap[5].parse().expect("Incorrect value"),
    }
}

fn read_claims() -> std::vec::Vec<Claim> {
    let mut f = File::open("03-fabric-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    Vec::from_iter(contents.lines().map(parse_line))
}

pub fn fabric() {
    let claims = read_claims();

    let mut fabric = vec![vec![OCCUPANCY_NONE; 1000]; 1000];

    for claim in &claims {
        mark_claim(&mut fabric, claim);
    }

    let result1: usize = fabric.iter()
        .map(|column| {
            column.iter().filter(|&o| *o == OCCUPANCY_MORE).count()
        })
        .sum();

    println!("Part1 result: {}", result1);
    
    let result2 = claims.iter().find(|c| is_claim_free(&fabric, c)).expect("Free claim not found");

    println!("Part2 result: {}", result2.id);
}

fn mark_claim(fabric: &mut std::vec::Vec<std::vec::Vec<SquareOccupancy>>, claim: &Claim) {
    for x in claim.x..(claim.x + claim.width) {
        for y in claim.y..(claim.y + claim.height) {
            fabric[x as usize][y as usize] = increase_occupancy(fabric[x as usize][y as usize]);
        }
    }
}

fn increase_occupancy(occupancy: SquareOccupancy) -> SquareOccupancy {
    match occupancy {
        OCCUPANCY_NONE => OCCUPANCY_ONE,
        OCCUPANCY_ONE => OCCUPANCY_MORE,
        _ => OCCUPANCY_MORE,
    }
}

fn is_claim_free(fabric: &std::vec::Vec<std::vec::Vec<SquareOccupancy>>, claim: &Claim) -> bool {
    for x in claim.x..(claim.x + claim.width) {
        for y in claim.y..(claim.y + claim.height) {
            if fabric[x as usize][y as usize] == OCCUPANCY_MORE {
                return false
            }
        }
    }

    return true;
}

