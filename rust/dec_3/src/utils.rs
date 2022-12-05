use std::collections::HashMap;

pub fn deconstruct_components(input_vec: &str) -> (&str, &str) {
    input_vec.split_at(input_vec.len() / 2)
}

pub fn find_common_components(start: &str, end: &str) -> Vec<char> {
    let mut common_components: Vec<char> = Vec::new();
    for cmp in start.chars() {
        match end.contains(cmp) && !common_components.contains(&cmp) {
            true => common_components.push(cmp),
            false => continue,
        }
    };
    common_components
}

pub fn find_common_rucksacks(rucksacks: Vec<String>) -> Vec<char> {
    let mut common_components: Vec<char> = Vec::new();
    let mut common_elements: HashMap<char, Vec<i32>> = HashMap::new();
    let mut cnt: i32 = 0;
    for rucksack in rucksacks {
        for c in rucksack.chars() {
            let count = common_elements.entry(c).or_insert(Vec::from([0, 0, 0]));
            count[cnt as usize] += 1;
        }
        cnt += 1;
    };
    for (k, v) in &common_elements {
        match v.contains(&0) {
            true => continue,
            false => common_components.push(*k)
        }
    }
    common_components
}

pub fn calculate_value(common_entries: Vec<char>) -> i32 {
    let mut cnt: i32 = 0;
    let chars: Vec<char> = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars().collect();
    for char in common_entries {
        let idx = (chars.iter().position(|&x| x == char).unwrap() + 1) as i32;
        cnt += idx;
    };
    cnt
}
