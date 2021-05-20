import { readFile } from "./util";

type Point = { x: number; y: number };

type Dir = "L" | "R" | "U" | "D";

const intersection = (a: Set<string>, b: Set<string>) =>
  new Set([...a].filter(x => b.has(x)));

const dirs = {
  L: ({ x, y }: Point) => ({ x: x - 1, y }),
  R: ({ x, y }: Point) => ({ x: x + 1, y }),
  U: ({ x, y }: Point) => ({ x: x, y: y + 1 }),
  D: ({ x, y }: Point) => ({ x: x, y: y - 1 }),
};

const ptStr = ({ x, y }: Point) => `${x},${y}`;

function pathToPoints(path: string[]): Point[] {
  let currPoint = { x: 0, y: 0 };
  const points: Point[] = [];
  path.forEach(move => {
    const dir = move[0] as Dir;
    const dirFunc = dirs[dir];
    const dist = parseInt(move.slice(1), 10);
    for (let i = 0; i < dist; i++) {
      currPoint = dirFunc(currPoint);
      points.push(currPoint);
    }
  });
  return points;
}

function pathToPointsSet(path: string[]): Set<string> {
  const points = pathToPoints(path);
  return new Set(points.map(ptStr));
}

const strToPoint = (s: string) => {
  const [x, y] = s.split(",");
  return { x: parseInt(x, 10), y: parseInt(y, 10) };
};

const manhattan = ({ x, y }: Point) => Math.abs(x) + Math.abs(y);

function closestIntersection(path1: string[], path2: string[]) {
  const intersections = intersection(pathToPointsSet(path1), pathToPointsSet(path2));
  const dists = Array.from(intersections)
    .map(strToPoint)
    .map(manhattan);
  return Math.min(...dists);
}

const eq = (p1: Point) => (p2: Point) => p1.x === p2.x && p1.y === p2.y;

function distToPoint(p: Point, path: string[]) {
  const points = pathToPoints(path);
  return points.map(eq(p)).indexOf(true) + 1;
}

function fewestStepsIntersection(path1: string[], path2: string[]) {
  const intersections = intersection(pathToPointsSet(path1), pathToPointsSet(path2));
  const dists = Array.from(intersections)
    .map(strToPoint)
    .map(p => distToPoint(p, path1) + distToPoint(p, path2));
  return Math.min(...dists);
}

export default function() {
  const [path1, path2] = readFile("day3-input.txt")
    .split("\n")
    .filter(x => x)
    .map(s => s.split(","));

  // const path1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",");
  // const path2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(",");
  // const path3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(',');
  // const path4 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",");
  // console.log(closestIntersection(path1, path2));
  // console.log(closestIntersection(path3, path4));

  console.log(closestIntersection(path1, path2));
  console.log(fewestStepsIntersection(path1, path2));
}
