use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::vec::Vec;

enum LogEntry {
    BeginsShift(i32),
    FallsAsleep,
    WakesUp,
}

struct LogTimestamp {
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
}

struct LogLine {
    timestamp: LogTimestamp,
    entry: LogEntry,
}

fn parse_line(line: &str) -> LogLine {
    let line_regex = Regex::new(r"^\[1518-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.*)$").unwrap();
    let begins_shift_regex = Regex::new(r"^Guard #(\d+) begins shift$").unwrap();

    let cap = line_regex.captures(line).expect("Regex did not match");

    LogLine {
        timestamp: LogTimestamp {
            month: cap[1].parse().expect("Incorrect value"),
            day: cap[2].parse().expect("Incorrect value"),
            hour: cap[3].parse().expect("Incorrect value"),
            minute: cap[4].parse().expect("Incorrect value"),
        },
        entry: match &cap[5] {
            "falls asleep" => LogEntry::FallsAsleep,
            "wakes up" => LogEntry::WakesUp,
            _ => {
                let cap2 = begins_shift_regex
                    .captures(&cap[5])
                    .expect("Regex did not match");
                let guard_id: i32 = cap2[1].parse().expect("Incorrect value");
                LogEntry::BeginsShift(guard_id)
            }
        },
    }
}

fn read_loglines() -> std::vec::Vec<LogLine> {
    let mut f = File::open("04-repose-input.txt").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    Vec::from_iter(contents.lines().map(parse_line))
}

pub fn repose() {
    let mut loglines = read_loglines();

    // Part 1
    loglines.sort_by(|a, b| {
        if a.timestamp.month > b.timestamp.month
            || a.timestamp.month == b.timestamp.month && a.timestamp.day > b.timestamp.day
            || a.timestamp.month == b.timestamp.month
                && a.timestamp.day == b.timestamp.day
                && a.timestamp.hour > b.timestamp.hour
            || a.timestamp.month == b.timestamp.month
                && a.timestamp.day == b.timestamp.day
                && a.timestamp.hour == b.timestamp.hour
                && a.timestamp.minute > b.timestamp.minute
        {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    });

    let mut stats: HashMap<i32, Vec<[bool; 60]>> = HashMap::new();

    let mut current_guard_id = match &loglines[0].entry {
        LogEntry::BeginsShift(id) => *id,
        _ => panic!("First line has to be BeginShift"),
    };

    let mut fell_asleep_minute = 0;

    let mut current_hour = [false; 60];

    for line in &loglines {
        match &line.entry {
            LogEntry::FallsAsleep => {
                fell_asleep_minute = line.timestamp.minute;
            }
            LogEntry::WakesUp => {
                mark_sleep(
                    &mut current_hour,
                    fell_asleep_minute as usize,
                    line.timestamp.minute as usize,
                );
            }
            LogEntry::BeginsShift(id) => {
                stats
                    .entry(current_guard_id)
                    .and_modify(|hours| hours.push(current_hour))
                    .or_insert(vec![current_hour]);

                current_guard_id = *id;
                current_hour = [false; 60];
            }
        }
    }

    let (laziest_guard_id, laziest_guard_hours) = stats
        .iter()
        .max_by_key::<i32, _>(|(_, hours)| {
            hours
                .iter()
                .map(|hour| hour.iter().filter(|x| **x).count() as i32)
                .sum()
        }).unwrap();

    let most_slept_minute = (0..60)
        .max_by_key(|minute| {
            laziest_guard_hours
                .iter()
                .filter(|hour| hour[*minute])
                .count()
        }).unwrap();

    let result1 = *laziest_guard_id * most_slept_minute as i32;

    println!("Part 1 result: {}", result1);

    // Part 2
    let extra_collection = stats
        .iter()
        .map(|(id, hours)| {
            let (sleepiest_minute, sleep_count) = get_sleepiest_minute_and_sleep_count(hours);
            (*id, sleepiest_minute, sleep_count)
        }).collect::<Vec<(i32, i32, i32)>>();

    let (guard_id, sleepiest_minute, _) = extra_collection.iter()
        .max_by_key(|(_, _, sleep_count)| sleep_count)
        .unwrap();

    let result2 = guard_id * sleepiest_minute;

    println!("Part 2 result: {}", result2);
}

fn mark_sleep(hour: &mut [bool; 60], from: usize, to: usize) {
    for i in from..to {
        hour[i] = true;
    }
}

fn get_sleepiest_minute_and_sleep_count(hours: &Vec<[bool; 60]>) -> (i32, i32) {
    let range_vec: std::vec::Vec<i32> = (0..60).collect();

    *range_vec
        .iter()
        .map(|minute| {
            let sleep_count = hours.iter().filter(|hour| hour[*minute as usize]).count();
            (*minute, sleep_count as i32)
        }).collect::<Vec<(i32, i32)>>()
        .iter()
        .max_by_key(|(_, sleep_count3)| sleep_count3)
        .unwrap()
}
