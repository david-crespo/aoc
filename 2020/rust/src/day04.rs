use std::collections::HashMap;
use std::fs;

#[path = "./util.rs"]
mod util;

const SAMPLE: &str = "
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
";

type Input<'a> = HashMap<&'a str, &'a str>;

fn run_p1(inputs: &Vec<Input>) {
    let req = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"];
    // util::print_vec(inputs);
    let valid_count = inputs
        .iter()
        .filter(|p| req.iter().all(|k| p.contains_key(k)))
        .count();
    println!("{}", valid_count);
}

fn run_p2(inputs: &Vec<Input>) {
    let ecls = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
    println!("p2")
}

fn parse(s: &str) -> Vec<Input> {
    s.trim()
        .split("\n\n")
        .map(|s| {
            s.split_whitespace()
                .map(|si| si.split_at(3))
                .map(|(k, v)| (k, &v[1..]))
                .collect()
        })
        .collect()
}

pub fn main() {
    let contents = fs::read_to_string("input/day04.txt").expect("Couldn't read file");
    let sample = parse(SAMPLE);
    let data = parse(&contents);

    println!("part 1\n================================");
    run_p1(&sample);
    run_p1(&data);

    // println!("\npart 2\n================================");
    // run_p2(&sample);
    // run_p2(&data);
}
