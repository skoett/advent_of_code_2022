use std::fs::File;
use std::path::Path;
use std::io::{prelude::*, BufReader};

fn read_buf_lines(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("No such file.");
    let reader = BufReader::new(file);
    reader.lines().map(|l| l.expect("Could not parse line")).collect()
}

fn main() {
    let input_vec = read_buf_lines("src/input.txt");
    let mut max: i32 = 0;
    let mut top_3: Vec<i32> = vec!(0, 0, 0);
    let mut cnt: i32 = 0;
    for line in input_vec {
        match line.parse::<i32>() {
            Ok(num) => cnt += num,
            Err(_) => {
                let min_val: &i32 = top_3.iter().min().unwrap();
                let min_index = top_3.iter().position(|&r| r == *min_val).unwrap();
                if cnt > max {
                    max = cnt;
                };
                if cnt > *min_val {
                    top_3[min_index] = cnt
                };
                cnt = 0
            },
        }
    }
    let sum: i32 = top_3.iter().sum();
    println!("Elf with most calories has: {} calories.", max);
    println!("Top three elves has the following calories sum: {}", sum);
}