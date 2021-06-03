use std::fs;

const SAMPLE: &str = "
";

type Input<'a> = &'a str;

fn run_p1(nums: &Vec<Input>) {
    println!("{:?}", nums)
}

fn run_p2(nums: &Vec<Input>) {
    println!("p2")
}

fn parse(s: &str) -> Vec<Input> {
    s.trim()
        .split('\n')
        // .filter_map(|n| n.parse::<i32>().ok())
        .collect()
}

pub fn main() {
    let contents = fs::read_to_string("input/day01.txt").expect("Couldn't read file");
    let sample = parse(SAMPLE);
    let data = parse(&contents);
    run_p1(&sample);
    // run_p1(&data);
    // run_p2(&sample);
    // run_p2(&data);
}
