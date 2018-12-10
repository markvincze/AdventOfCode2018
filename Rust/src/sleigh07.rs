use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::vec::Vec;
//use std::collections::BTreeSet;
//use std::collections::HashMap;
//use std::cmp::Ordering;

fn parse_line(line: &str) -> (char, char) {
    let line_regex = Regex::new(r"^Step (.) must be finished before step (.) can begin.$").unwrap();

    let cap = line_regex.captures(line).expect("Regex did not match");

    (
        cap[1].chars().next().unwrap(),
        cap[2].chars().next().unwrap(),
    )
}

fn read_deps() -> std::vec::Vec<(char, char)> {
    let mut f = File::open("07-sleigh-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    Vec::from_iter(contents.lines().map(parse_line))
}

//#[derive(Eq)]
struct Step {
    name: char,
    deps: Vec<char>,
    is_done: bool,
}

//impl Ord for Step {
//fn cmp(&self, other: &Step) -> Ordering {
//self.name.cmp(&other.name)
//}
//}

//impl PartialOrd for Step {
//fn partial_cmp(&self, other: &Step) -> Option<Ordering> {
//Some(self.cmp(other))
//}
//}

//impl PartialEq for Step {
//fn eq(&self, other: &Step) -> bool {
//self.name == other.name
//}
//}

fn build_cache(deps: std::vec::Vec<(char, char)>) -> Vec<Step> {
    let mut cache: Vec<Step> = Vec::new();

    for (a, b) in deps {
        {
            let mut found = false;

            {
                let step = cache.iter_mut().filter(|s| s.name == b).nth(0);

                match step {
                    Some(s) => {
                        found = true;
                        s.deps.push(a);
                    }
                    None => (),
                }
            }

            if !found {
                cache.push(Step {
                    name: b,
                    deps: vec![a],
                    is_done: false,
                });
            }
        }

        if !cache.iter().any(|s| s.name == a) {
            cache.push(Step {
                name: a,
                deps: Vec::new(),
                is_done: false,
            });
        }
    }

    cache

        //if cache.contains_key(&b) {
        //let step = cache.get_mut(&b).unwrap();
        //step.deps.push(a);
        //} else {
        //cache.insert(
        //b,
        //Step {
        //name: b,
        //deps: vec![a],
        //is_done: false
        //});
        //}

        //if !cache.contains_key(&a) {
        //cache.insert(a, Step {
        //name: a,
        //deps: Vec::new(),
        //is_done: false
        //});
        //}
}

pub fn sleigh() {
    let deps = read_deps();

    let mut cache = build_cache(deps);
    let mut order = String::new();
    
    loop {
        println!("Running iteration");
        let next_step = try_pick_next(&mut cache);

        match next_step {
            Some(s) => { order.push(s); },
            None => { break; }
        }
    }

    println!("Part1 result: {}", order);
}

fn try_pick_next(steps: &mut Vec<Step>) -> Option<char> {
    let next: Option<char>;

    {
        steps.sort_by_key(|s| s.name);
        match steps.iter()
            .filter(|s| s.deps.len() == 0)
            .nth(0) {
            Some(n) => { next = Some(n.name); },
            None => { next = None; }
        }
    }

    match next {
        Some(n) => {
            for step in steps.iter_mut() {
                match step.deps.iter().position(|d| *d == n) {
                    Some(pos) => { step.deps.remove(pos); },
                    None => ()
                }
            }
        },
        None => ()
    }

    match next {
        Some(n) => {
            let pos = steps.iter().position(|s| s.name == n).unwrap();
            steps.remove(pos);
        }
        None => ()
    }

    next
}
