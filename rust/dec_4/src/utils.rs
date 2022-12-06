use std::fs::File;
use std::path::Path;
use std::io::{prelude::*, BufReader};
use std::str::FromStr;

pub fn read_buf_lines(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("No such file.");
    let reader = BufReader::new(file);
    reader.lines().map(|l| l.expect("Could not parse line")).collect()
}

pub fn get_min_max(line: &str) -> (u32, u32, u32, u32) {
    let (a1, a2) = line.split_once(",").expect("One comma is expected.");
    let (x1, y1) = a1.split_once("-").expect("One dash is expected");
    let (x2, y2) = a2.split_once("-").expect("One dash is expected");
    let x1 = u32::from_str(x1).expect("Expected a u32");
    let x2 = u32::from_str(x2).expect("Expected a u32");
    let y1 = u32::from_str(y1).expect("Expected a u32");
    let y2 = u32::from_str(y2).expect("Expected a u32");
    (x1, x2, y1, y2)
}

pub fn validate_a_in_b(x1: u32, x2: u32, y1: u32, y2: u32) -> u32 {
    if (x1 <= x2 && y1 >= y2) || (x1 >= x2 && y1 <= y2) {
        return 1
    }
    0
}

pub fn validate_overlapping(x1: u32, x2: u32, y1: u32, y2: u32) -> u32 {
    if (x1 <= x2 && y1 >= x2) || (x1 >= x2 && y2 >= x1) {
        return 1
    }
    0
}

#[cfg(test)]
mod tests {
    use crate::utils::{
        read_buf_lines,
        get_min_max,
        validate_a_in_b,
        validate_overlapping
    };

    #[test]
    fn test_read_file() {
        let fp = "src/test.txt";
        let input = read_buf_lines(fp);
        assert!(input.len() > 0)
    }

    #[test]
    fn test_min_max() {
        let line = "2-5,6-8".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        assert_eq!(x1, 2);
        assert_eq!(x2, 6);
        assert_eq!(y1, 5);
        assert_eq!(y2, 8)
    }

    #[test]
    fn test_validate_a_in_b_1() {
        let line = "2-5,5-8".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_a_in_b(x1, x2 ,y1, y2);
        assert_eq!(num, 0)
    }

    #[test]
    fn test_validate_a_in_b_2() {
        let line = "2-7,3-5".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_a_in_b(x1, x2 ,y1, y2);
        assert_eq!(num, 1)
    }

    #[test]
    fn test_validate_overlapping_1() {
        let line = "2-4,6-8".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_overlapping(x1, x2 ,y1, y2);
        assert_eq!(num, 0)
    }

    #[test]
    fn test_validate_overlapping_2() {
        let line = "2-3,4-5".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_overlapping(x1, x2 ,y1, y2);
        assert_eq!(num, 0)
    }

    #[test]
    fn test_validate_overlapping_3() {
        let line = "5-7,7-9".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_overlapping(x1, x2 ,y1, y2);
        assert_eq!(num, 1)
    }

    #[test]
    fn test_validate_overlapping_4() {
        let line = "2-8,3-7".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_overlapping(x1, x2 ,y1, y2);
        assert_eq!(num, 1)
    }

    #[test]
    fn test_validate_overlapping_5() {
        let line = "6-6,4-6".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_overlapping(x1, x2 ,y1, y2);
        assert_eq!(num, 1)
    }

    #[test]
    fn test_validate_overlapping_6() {
        let line = "2-6,4-8".to_string();
        let (x1, x2, y1, y2) = get_min_max(&line);
        println!("x1: {} x2: {} y1: {} y2: {}", x1, x2, y1, y2);
        let num = validate_overlapping(x1, x2 ,y1, y2);
        assert_eq!(num, 1)
    }

    #[test]
    fn integration_test_1() {
        let line = "32-34,35-38";
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_a_in_b(x1, x2 ,y1, y2);
        assert!(num == 0)
    }

    #[test]
    fn integration_test_2() {
        let line = "36-38,35-38";
        let (x1, x2, y1, y2) = get_min_max(&line);
        let num = validate_a_in_b(x1, x2 ,y1, y2);
        assert!(num == 1)
    }

    #[test]
    fn integration_test_3() {
        let filename = "src/test.txt";
        let mut cnt: u32 = 0;
        let lines:Vec<String> = read_buf_lines(filename);
        for line in lines {
            let (x1, x2, y1, y2) = get_min_max(&line);
            cnt += validate_a_in_b(x1, x2 ,y1, y2)
        }
        assert_eq!(cnt, 2)
    }

    #[test]
    fn integration_test_6() {
        let filename = "src/test.txt";
        let mut cnt: u32 = 0;
        let lines:Vec<String> = read_buf_lines(filename);
        for line in lines {
            let (x1, x2, y1, y2) = get_min_max(&line);
            cnt += validate_overlapping(x1, x2 ,y1, y2)
        }
        assert_eq!(cnt, 4)
    }
}
