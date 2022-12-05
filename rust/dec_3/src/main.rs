mod utils;

use std::fs::File;
use std::path::Path;
use std::io::{prelude::*, BufReader};

fn read_buf_lines(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("No such file.");
    let reader = BufReader::new(file);
    reader.lines().map(|l| l.expect("Could not parse line")).collect()
}

fn main() {

    let lines = read_buf_lines("input.txt");
    let mut cnt_1: i32 = 0;
    let mut cnt_2: i32 = 0;
    let mut mod_cnt: i32 = 1;
    let mut line_copy: String;
    let mut rucksacks: Vec<String> = vec![];
    for line in lines {
        line_copy = line.clone();
        let (start, end) = utils::deconstruct_components(&line);
        let common_entries = utils::find_common_components(start, end);
        cnt_1 += utils::calculate_value(common_entries);
        match mod_cnt == 3 {
            false => {
                rucksacks.push(line_copy);
                mod_cnt += 1
            }
            true => {
                rucksacks.push(line_copy);
                let shared_common_entries = utils::find_common_rucksacks(rucksacks);
                cnt_2 += utils::calculate_value(shared_common_entries);
                rucksacks = vec![];
                mod_cnt = 1
            }
        }
    }
    println!("task 1: {}", cnt_1);
    println!("task 2: {}", cnt_2)
}
