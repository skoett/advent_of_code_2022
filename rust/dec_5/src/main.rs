use std::collections::HashMap;

mod utils;

fn main() {
    let filename: &str = "src/input.txt";
    let mut hm: HashMap<i32, Vec<char>> = HashMap::new();
    let lines: Vec<String> = utils::read_buf_lines(filename);
    let mut crates: Vec<String> = Vec::new();
    let mut moves: Vec<String> = Vec::new();
    let mut stack: Vec<char> = Vec::new();
    for line in lines {
        if line.is_empty() || &line.trim()[0..1] != "m" {
            crates.push(line);
            continue
        }
        moves.push(line)
    }
    utils::fill_crates(&mut hm, &crates);
    for command in  &moves {
        utils::move_crate(&mut hm, command);
    }
    println!("{:?}", hm);
    for v in 0..(hm.keys().len() as i32) {
        match hm.get(&v) {
            None => continue,
            Some(t) => {stack.push(*t.last().unwrap())}
        }
    }
    let stack: String = stack.into_iter().collect();
    println!("{}", stack)
}
