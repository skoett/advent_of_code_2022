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
    let mut cnt_a: u32 = 0;
    let mut cnt_b: u32 = 0;
    for line in lines {
        let tup: Vec<&str> = line.split(" ").collect();
        match tup[..] {
            ["A", "X"] => cnt_a += 4, // draw, rock    = 3 + 1
            ["A", "Y"] => cnt_a += 8, // win,  paper   = 6 + 2
            ["A", "Z"] => cnt_a += 3, // lost, scissor = 0 + 3
            ["B", "X"] => cnt_a += 1, // lost, rock    = 0 + 1
            ["B", "Y"] => cnt_a += 5, // draw, paper   = 3 + 2
            ["B", "Z"] => cnt_a += 9, // win,  scissor = 6 + 3
            ["C", "X"] => cnt_a += 7, // win,  rock    = 6 + 1
            ["C", "Y"] => cnt_a += 2, // lost, paper   = 0 + 2
            ["C", "Z"] => cnt_a += 6, // draw, scissor = 3 + 3
            _ => continue,
        };

        match tup[..] {
            ["A", "X"] => cnt_b += 3, // lost, scissor = 0 + 3
            ["A", "Y"] => cnt_b += 4, // draw, rock    = 3 + 1
            ["A", "Z"] => cnt_b += 8, // win,  scissor = 6 + 2
            ["B", "X"] => cnt_b += 1, // lost, rock    = 0 + 1
            ["B", "Y"] => cnt_b += 5, // draw, paper   = 3 + 2
            ["B", "Z"] => cnt_b += 9, // win,  scissor = 6 + 3
            ["C", "X"] => cnt_b += 2, // lost, rock    = 0 + 2
            ["C", "Y"] => cnt_b += 6, // draw, scissor = 3 + 3
            ["C", "Z"] => cnt_b += 7, // win,  scissor = 6 + 1
            _ => continue,
        };
    }
    println!("Sum of first game simulation is: {}", cnt_a);
    println!("Sum of second game simulation is: {}", cnt_b)
}
