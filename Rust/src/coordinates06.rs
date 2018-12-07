use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::vec::Vec;
use std::option::Option;

fn parse_coord(line: &str) -> (i32, i32) {
    let line_regex = Regex::new(r"^(\d+), (\d+)$").unwrap();

    let cap = line_regex.captures(line).expect("Regex did not match");

    (cap[1].parse().expect("Invalid value"), cap[2].parse().expect("Invalid value"))
}

fn read_coords() -> std::vec::Vec<(i32, i32)> {
    let mut f = File::open("06-coordinates-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    Vec::from_iter(contents.lines().map(parse_coord))
}

#[derive(Clone)]
enum Cell {
    Coord (i32),
    Closest (i32),
    Tie,
    Unknown
}

pub fn coordinates() {
    let coords_original = read_coords();

    let count = coords_original.len();
    let min_x = coords_original.iter().map(|(x, _)| x).min().unwrap();
    let min_y = coords_original.iter().map(|(_, y)| y).min().unwrap();
    let max_x = coords_original.iter().map(|(x, _)| x).max().unwrap();
    let max_y = coords_original.iter().map(|(_, y)| y).max().unwrap();

    let width = max_x - min_x + 1;
    let height = max_y - min_y + 1;

    let coords: std::vec::Vec<(i32, i32)> = coords_original.iter().map(|(x, y)| (x - min_x, y - min_y)).collect();

    let mut grid = vec![vec![Cell::Unknown; height as usize]; width as usize];

    // Part 1

    for (i, (x, y)) in coords.iter().enumerate() {
        grid[*x as usize][*y as usize] = Cell::Coord(i as i32);
    }

    for x in 0..width {
        for y in 0..height {
            fill_cell(&coords, &mut grid, x, y);
        }
    }

    let mut infinites: Vec<Option<i32>> = (0..width).map(|x| coord_or_closest(&grid[x as usize][0])).collect();
    let mut infinites2: Vec<Option<i32>> = (0..width).map(|x| coord_or_closest(&grid[x as usize][(height - 1) as usize])).collect();
    let mut infinites3: Vec<Option<i32>> = (0..height).map(|y| coord_or_closest(&grid[0][y as usize])).collect();
    let mut infinites4: Vec<Option<i32>> = (0..height).map(|y| coord_or_closest(&grid[(width - 1) as usize][y as usize])).collect();

    infinites.append(&mut infinites2);
    infinites.append(&mut infinites3);
    infinites.append(&mut infinites4);

    let result1 = (0..count)
        .filter(|i| !infinites.contains(&Some(*i as i32)))
        .map(|i| {
            let mut count = 0;
            for x in 0..width {
                for y in 0..height {
                    match &grid[x as usize][y as usize] {
                        Cell::Coord(j) => {
                            if i == *j as usize {
                                count += 1;
                            }
                        },
                        Cell::Closest(j) => {
                            if i == *j as usize {
                                count += 1;
                            }
                        },
                        Cell::Tie => (),
                        Cell::Unknown => panic!("Unknown cell")
                    }
                }
            }

            count
        })
        .max().unwrap();

    println!("Part 1 result: {}", result1);

    // Part 2
    let mut total_distances: Vec<i32> = vec![];

    for x in 0..width {
        for y in 0..height {
            total_distances.push(coords.iter().map(|c| manhattan(*c, (x, y))).sum());
        }
    }

    let result2 = total_distances.iter().filter(|d| **d < 10000).count();

    println!("Part 2 result: {}", result2);
}

fn coord_or_closest(cell: &Cell) -> Option<i32> {
    match cell {
        Cell::Coord(i) => Some(*i),
        Cell::Closest(i) => Some(*i),
        _ => None
    }
}

fn manhattan(c1: (i32, i32), c2: (i32, i32)) -> i32 {
    let (x1, y1) = c1;
    let (x2, y2) = c2;

    ((x1 - x2).abs() + (y1 - y2).abs())
}

fn fill_cell(coords: &Vec<(i32, i32)>, grid: &mut Vec<Vec<Cell>>, x: i32, y:i32) {
    match grid[x as usize][y as usize] {
        Cell::Coord (_) => (),
        Cell::Unknown => grid[x as usize][y as usize] = get_closest(&coords, x, y),
        _ => panic!("All empty cells should be Unknown.")
    }
}

fn get_closest(coords: &Vec<(i32, i32)>, x: i32, y: i32) -> Cell {
    let min_dist = coords.iter().map(|c| manhattan(*c, (x, y))).min().unwrap();
    let ties: Vec<(usize, &(i32, i32))> = coords.iter().enumerate().filter(|(_, c)| manhattan(**c, (x, y)) == min_dist).collect();
    if ties.len() == 1 {
        let (index, _) = ties.first().unwrap();
        Cell::Closest(*index as i32)
    } else {
        Cell::Tie
    }
}

