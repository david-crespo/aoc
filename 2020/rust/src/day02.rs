use regex::Regex;
use std::fs;

#[path = "./util.rs"]
mod util;

const SAMPLE: &str = "
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
";

#[derive(Debug)]
struct Input {
    low: usize,
    high: usize,
    letter: char,
    target: String,
}

fn run_p1(inputs: &Vec<Input>) {
    // util::print_vec(inputs);
    let count = inputs
        .iter()
        .filter(|i| {
            let count = i.target.chars().filter(|&c| c == i.letter).count();
            i.low <= count && count <= i.high
        })
        .count();
    println!("valid rows: {:?}", count);
}

fn run_p2(inputs: &Vec<Input>) {
    let count = inputs
        .iter()
        .filter(|i| {
            let a = i.target.chars().nth(i.low - 1).unwrap() == i.letter;
            let b = i.target.chars().nth(i.high - 1).unwrap() == i.letter;
            a ^ b
        })
        .count();
    println!("valid rows: {:?}", count);
}

fn parse(s: &str) -> Vec<Input> {
    let re = Regex::new("(\\d+)-(\\d+) ([a-z]): ([a-z]+)").unwrap();
    s.trim()
        .split('\n')
        .map(|s| {
            let caps = re.captures(s).unwrap();
            Input {
                low: caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
                high: caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
                letter: String::from(caps.get(3).unwrap().as_str())
                    .chars()
                    .nth(0)
                    .unwrap(),
                target: String::from(caps.get(4).unwrap().as_str()),
            }
        })
        .collect()
}

pub fn main() {
    let contents = fs::read_to_string("input/day02.txt").expect("Couldn't read file");
    let sample = parse(SAMPLE);
    let data = parse(&contents);

    println!("part 1\n================================");
    run_p1(&sample);
    run_p1(&data);

    println!("\npart 2\n================================");
    run_p2(&sample);
    run_p2(&data);
}
