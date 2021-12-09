import type { Pt } from "./util.ts";
import { getInput, ptKey } from "./util.ts";

const sampleInput = `
2199943210
3987894921
9856789892
8767896789
9899965678
`.trim();

const realInput = (await getInput(import.meta.url)).trim();

function* neighbors(
  x: number,
  y: number,
  maxx: number,
  maxy: number,
): Generator<Pt> {
  if (x > 0) yield [x - 1, y];
  if (y > 0) yield [x, y - 1];
  if (x < maxx) yield [x + 1, y];
  if (y < maxy) yield [x, y + 1];
}

const parse = (input: string) => {
  const lines = input
    .split("\n")
    .map((s) => s.split("").map((n) => parseInt(n, 10)));
  return lines;
};

function getLowPoints(board: number[][]) {
  const maxx = board[0].length - 1;
  const maxy = board.length - 1;

  const lowPoints: Pt[] = [];
  for (let y = 0; y <= maxy; y++) {
    for (let x = 0; x <= maxx; x++) {
      const value = board[y][x];
      const ns = Array.from(neighbors(x, y, maxx, maxy)).map(
        ([x, y]) => board[y][x],
      );
      if (ns.every((v) => v > value)) {
        lowPoints.push([x, y]);
      }
    }
  }
  return lowPoints;
}

function run(input: string) {
  const inp = parse(input);
  console.log(
    getLowPoints(inp)
      .map(([x, y]) => inp[y][x] + 1)
      .reduce((a, b) => a + b),
  );
}

// from a point, fan out to all the higher non-9 points until you run out
function getBasin([x, y]: Pt, board: number[][]): Pt[] {
  const maxx = board[0].length - 1;
  const maxy = board.length - 1;
  const ns = Array.from(neighbors(x, y, maxx, maxy));
  const larger = ns.filter(
    ([nx, ny]) => board[ny][nx] > board[y][x] && board[ny][nx] != 9,
  );
  return [[x, y], ...larger.flatMap((n) => getBasin(n, board))];
}

function run2(input: string) {
  const inp = parse(input);
  const result = getLowPoints(inp)
    .map((pt) => getBasin(pt, inp))
    .map((b) => new Set(b.map(ptKey))) // dedupe
    .sort((a, b) => b.size - a.size)
    .slice(0, 3)
    .map((b) => b.size)
    .reduce((a, b) => a * b);
  console.log(result);
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
