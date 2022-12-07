use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::fs::File;
use std::path::Path;
use std::io::{prelude::*, BufReader};
use std::str::FromStr;

pub fn read_buf_lines(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("No such file.");
    let reader = BufReader::new(file);
    reader.lines().map(|l| l.expect("Could not parse line")).collect()
}

pub fn parse_crates(line: &String) -> Vec<(i32, char)> {
    let mut crates: Vec<(i32, char)> = Vec::new();
    let mut cnt: i32 = 0;
    for c in line.chars().enumerate() {
        if cnt % 4 == 1 {
            crates.push((cnt / 4 + 1, c.1))
        }
        cnt +=1
    }
    crates
}

pub fn fill_crates(hm: &mut HashMap<i32, Vec<char>>, lines: &Vec<String>) -> () {
        for line in lines {
        if !line.is_empty() && &line.trim()[0..1] == "[" {
            let a = parse_crates(&line);
            for (i, c) in a {
                if !c.is_whitespace() {
                    let cnt = hm.borrow_mut().entry(i).or_insert(Vec::new());
                    cnt.push(c)
                }
            }
        } else if line.is_empty() {
            for (_k, v) in hm.borrow_mut() {
                v.reverse();
            }
        }
    }
}

pub fn move_crate(hm: &mut HashMap<i32, Vec<char>>, command: &String) -> () {
    let command: Vec<&str> = command.split( " ").collect::<Vec<&str>>();
    if command.len() == 6 {
        let move_num = i32::from_str(command[1]).unwrap();
        let from_crate = i32::from_str(command[3]).unwrap();
        let to_crate = i32::from_str(command[5]).unwrap();
        for _ in 0..move_num {
            let mut item: Vec<char> = hm.remove(&from_crate).unwrap();
            let val = item.remove(item.len() - 1);
            hm.borrow_mut().entry(from_crate).or_insert(item);
            hm.borrow_mut()
                .entry(to_crate)
            .or_insert(Vec::new()).push(val);
        }
    }
    else {
        println!("Command is malformed. No move happens.")
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::str::FromStr;
    use crate::utils::{
        read_buf_lines,
        fill_crates,
        move_crate,
    };

    #[test]
    fn test_read_file() {
        let fp = "src/test.txt";
        let input = read_buf_lines(fp);
        assert!(input.len() > 0)
    }

    #[test]
    fn test_parse_crates() {
        let filename = "src/test.txt";
        let mut hm: HashMap<i32, Vec<char>> = HashMap::new();
        let lines = read_buf_lines(filename);
        fill_crates(&mut hm, &lines[0..5].to_vec());
        assert_eq!(hm.get(&1), &['Z', 'N']);
        assert_eq!(hm.get(&2), &['M', 'C', 'D']);
        assert_eq!(hm.get(&3), &['P'])
    }

    #[test]
    fn test_move_crates() {
        let filename = "src/test.txt";
        let mut hm: HashMap<i32, Vec<char>> = HashMap::new();
        let lines = read_buf_lines(filename);
        fill_crates(&mut hm, &lines[0..5].to_vec());
        for command in  &lines[5..] {
            move_crate(&mut hm, command);
        }
        assert_eq!(hm.get(&1), &['C']);
        assert_eq!(hm.get(&2), &['M']);
        assert_eq!(hm.get(&3), &['P', 'D', 'N', 'Z'])
    }
}
