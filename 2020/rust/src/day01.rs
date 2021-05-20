use itertools::Itertools;
use std::fs;

// checking that the values aren't equal instead of the indices only works
// because the values happen to be unique

fn run_p1(nums: &Vec<i32>) -> i32 {
    for a in nums.iter() {
        for b in nums.iter() {
            if a != b && a + b == 2020 {
                return a * b;
            }
        }
    }
    0
}

fn run_p1_alt(nums: &Vec<i32>) -> i32 {
    nums.iter()
        .cartesian_product(nums.iter())
        .find(|(&a, &b)| a != b && a + b == 2020)
        .map_or(0, |(a, b)| a * b)
}

fn run_p2(nums: &Vec<i32>) -> i32 {
    for a in nums.iter() {
        for b in nums.iter() {
            for c in nums.iter() {
                if a != b && b != c && a != c && a + b + c == 2020 {
                    return a * b * c;
                }
            }
        }
    }
    0
}

fn run_p2_alt(nums: &Vec<i32>) -> i32 {
    for (a, b, c) in itertools::iproduct!(nums.iter(), nums.iter(), nums.iter()) {
        if a != b && b != c && a != c && a + b + c == 2020 {
            return a * b * c;
        }
    }
    0
}

fn run_p2_alt2(nums: &Vec<i32>) -> i32 {
    itertools::iproduct!(nums.iter(), nums.iter(), nums.iter())
        .find(|(&a, &b, &c)| a != b && b != c && a != c && a + b + c == 2020)
        .map_or(0, |(a, b, c)| a * b * c)
}

fn parse(s: String) -> Vec<i32> {
    s.split('\n')
        .filter_map(|n| n.parse::<i32>().ok())
        .collect()
}

#[test]
fn p1_sample() {
    let nums = vec![1721, 979, 366, 299, 675, 1456];
    assert_eq!(run_p1(&nums), 514579);
    assert_eq!(run_p1_alt(&nums), 514579);
}

#[test]
fn p1() {
    let contents = fs::read_to_string("input/day01.txt").expect("Couldn't read file");
    let nums = parse(contents);
    assert_eq!(run_p1(&nums), 539851);
    assert_eq!(run_p1_alt(&nums), 539851);
}
#[test]
fn p2_sample() {
    let nums = vec![1721, 979, 366, 299, 675, 1456];
    assert_eq!(run_p2(&nums), 241861950);
    assert_eq!(run_p2_alt2(&nums), 241861950);
}

#[test]
fn p2() {
    let contents = fs::read_to_string("input/day01.txt").expect("Couldn't read file");
    let nums = parse(contents);
    assert_eq!(run_p2(&nums), 212481360);
    assert_eq!(run_p2_alt(&nums), 212481360);
    assert_eq!(run_p2_alt2(&nums), 212481360);
}
