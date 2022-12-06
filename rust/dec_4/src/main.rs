mod utils;

fn main() {
    let mut cnt_1: u32 = 0;
    let mut cnt_2: u32 = 0;
    let lines = utils::read_buf_lines("src/input.txt");
    for line in lines {
        let (x1, x2, y1, y2) = utils::get_min_max(&line);
        cnt_1 += utils::validate_a_in_b(x1, x2 ,y1, y2);
        cnt_2 += utils::validate_overlapping(x1, x2 ,y1, y2)
    }
    println!("{cnt_1} assignments are included in another");
    println!("{cnt_2} assignments are overlapping.")

}
