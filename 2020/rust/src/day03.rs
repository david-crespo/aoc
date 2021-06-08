use std::fs;

#[path = "./util.rs"]
mod util;

const SAMPLE: &str = "
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
";

type Input<'a> = &'a str;
type Point = (usize, usize);
type Grid<'a> = Vec<Input<'a>>;
type Slope = Point;

fn count_trees(grid: &Grid, slope: Slope) -> i32 {
    let mut pt: Point = (0, 0);
    let mut trees = 0;
    while pt.1 < grid.len() {
        if grid[pt.1].chars().nth(pt.0).unwrap() == '#' {
            trees += 1
        }
        pt = ((pt.0 + slope.0) % grid[0].len(), pt.1 + slope.1);
    }
    trees
}

fn run_p1(grid: &Grid) {
    println!("{}", count_trees(grid, (3, 1)))
}

fn run_p2(grid: &Vec<Input>) {
    let slopes = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    let mut result = 1;
    for slope in slopes {
        result *= count_trees(grid, slope);
    }
    println!("{}", result)
}

fn parse(s: &str) -> Vec<Input> {
    s.trim().split('\n').collect()
}

pub fn main() {
    let contents = fs::read_to_string("input/day03.txt").expect("Couldn't read file");
    let sample = parse(SAMPLE);
    let data = parse(&contents);

    println!("part 1\n================================");
    run_p1(&sample);
    run_p1(&data);

    println!("\npart 2\n================================");
    run_p2(&sample);
    run_p2(&data);
}
