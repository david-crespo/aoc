import { readFile, ptToStr, strToPt } from "./util";
import { maxBy, flatten, groupBy, sortBy } from "lodash";

const testInput = `
.#..#
.....
#####
....#
...##
`.trim();

const testInput2 = `
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
`.trim();

type AsteroidMap = {
  width: number;
  height: number;
  asteroids: Point[];
};

function parseMap(input: string): AsteroidMap {
  const rows = input.split("\n");
  const width = rows[0].length;
  const height = rows.length;
  const asteroids: Point[] = [];
  for (let i = 0; i < width; i++) {
    for (let j = 0; j < height; j++) {
      if (rows[j][i] === "#") {
        asteroids.push({ x: i, y: j });
      }
    }
  }
  return { width, height, asteroids };
}

const calcAngle = (p1: Point) => (p2: Point) =>
  (Math.atan2(p2.y - p1.y, p2.x - p1.x) * 180) / Math.PI;

function getOtherAsteroids(map: AsteroidMap, station: Point) {
  const otherAsteroidsSet = new Set(map.asteroids.map(ptToStr));
  otherAsteroidsSet.delete(ptToStr(station));
  return Array.from(otherAsteroidsSet).map(strToPt);
}

function asteroidsInSight(map: AsteroidMap, station: Point): number {
  const g = groupBy(getOtherAsteroids(map, station), calcAngle(station));
  return Object.keys(g).length;
}

const dist = (p1: Point, p2: Point) => (p2.y - p1.y) ^ (2 + (p2.x - p1.x)) ^ 2;

const sortAngle = (angle: number) => {
  const rot = (angle + 90) % 360;
  return rot < 0 ? rot + 360 : rot;
};

function getVapeOrder(map: AsteroidMap, station: Point): Array<any> {
  const byAngle = groupBy(getOtherAsteroids(map, station), calcAngle(station));

  let order = flatten(
    Object.entries(byAngle).map(([angle, pts]) =>
      sortBy(pts, p => -dist(station, p)).map((p, idx) => {
        const ang = sortAngle(parseFloat(angle));
        return {
          sort: ang + idx * 360,
          angle: ang,
          dist: dist(station, p),
          pt: ptToStr(p),
        };
      }),
    ),
  );

  return sortBy(order, ["sort"]);
}

const testInput3 = `
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
`.trim();

export default function() {
  const map = parseMap(readFile("day10-input.txt"));
  // const map = parseMap(testInput3);

  const bestAsteroid = maxBy(map.asteroids, a => asteroidsInSight(map, a));
  if (bestAsteroid) {
    console.log(bestAsteroid);
    console.log(asteroidsInSight(map, bestAsteroid));
    const vapeOrder = getVapeOrder(map, bestAsteroid);

    console.log("200th:", vapeOrder[199]);
  } else {
    console.log("no best asteroid");
  }
}
