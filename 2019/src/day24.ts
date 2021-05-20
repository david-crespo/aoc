import {
  getEdges,
  getCell,
  mapBoard,
  drawBoard,
  add,
  getCol,
  newBoard,
} from "./util";
import { flatten, chain, remove, range } from "lodash";

type BoardStack = { [i: string]: Boord };

const input = `
#####
.#.#.
.#..#
....#
..###
`.trim();

const test = `
....#
#..#.
#..##
..#..
#....
`.trim();

const test2 = `
.....
.....
.....
#....
.#...
`.trim();

const test3 = `
....#
#..#.
#..##
..#..
#....
`.trim();

const draw = (b: Boord) => drawBoard(b, v => ".#"[v ? 1 : 0]);
const draw3 = (b: Boord) =>
  drawBoard(b, (v, b, x, y) => (x === 2 && y === 2 ? "?" : ".#"[v ? 1 : 0]));

function drawBoards(bs: BoardStack) {
  chain(bs)
    .entries()
    .sortBy(([k, b]) => parseInt(k, 10))
    .value()
    .forEach(([k, b]) => {
      console.log("Depth", k, "\n");
      draw3(b);
    });
}

const parse = (input: string): Boord =>
  input.split("\n").map(r => Array.from(r).map(c => c === "#"));

const bugNeighbors = (board: Boord, pt: Point): number =>
  getEdges(board, pt).filter(e => getCell(board, e)).length;

const nextCellVal = (v: boolean, bugsAround: number) =>
  v ? bugsAround === 1 : bugsAround === 1 || bugsAround === 2;

const step = (board: Boord): Boord =>
  mapBoard(board, (v, b, x, y) => nextCellVal(v, bugNeighbors(b, { x, y })));

const calcBioD = (board: Boord): number =>
  flatten(board)
    .map((v, i) => (v ? Math.pow(2, i) : 0))
    .reduce(add, 0);

function part1() {
  let board = parse(input);

  const seen = new Set<number>();
  while (true) {
    board = step(board);
    const bioD = calcBioD(board);
    if (seen.has(bioD)) {
      console.log("first repeated bioD:", bioD);
      return;
    }
    seen.add(bioD);
  }
}

const countBugs = (board: Boord): number =>
  flatten(board).filter(x => x).length;

const countAllBugs = (bs: BoardStack): number =>
  Object.values(bs)
    .map(countBugs)
    .reduce(add, 0);

function superStep(boards: BoardStack): BoardStack {
  const newStack: BoardStack = {};
  for (const k in boards) {
    const board = boards[k];
    const level = parseInt(k, 10);
    const outer = boards[level - 1];
    const inner = boards[level + 1];
    newStack[k] = mapBoard(board, (v, b, x, y) => {
      if (x === 2 && y === 2) {
        return false;
      }

      const edges = getEdges(board, { x, y });

      // middle cell doesn't count
      remove(edges, ({ x, y }) => x === 2 && y === 2);

      let bugCount = edges.filter(e => getCell(board, e)).length;
      if (x === 0 && outer && outer[2][1]) {
        bugCount += 1;
      } else if (x === 4 && outer && outer[2][3]) {
        bugCount += 1;
      }
      if (y === 0 && outer && outer[1][2]) {
        bugCount += 1;
      } else if (y === 4 && outer && outer[3][2]) {
        bugCount += 1;
      }

      if (x === 1 && y === 2 && inner) {
        bugCount += getCol(inner, 0).filter(v => v).length;
      } else if (x === 2 && y === 1 && inner) {
        bugCount += inner[0].filter(v => v).length;
      } else if (x === 3 && y === 2 && inner) {
        bugCount += getCol(inner, 4).filter(v => v).length;
      } else if (x === 2 && y === 3 && inner) {
        bugCount += inner[4].filter(v => v).length;
      }

      return nextCellVal(v, bugCount);
    });
  }

  return newStack;
}

export default function() {
  const nSteps = 200;

  let board = parse(input);
  let boards: BoardStack = { 0: board };

  range(1, nSteps).forEach(i => {
    boards[i] = newBoard(5, 5, false);
    boards[-i] = newBoard(5, 5, false);
  });

  for (let i = 0; i < nSteps; i++) {
    boards = superStep(boards);
  }

  drawBoards(boards);

  console.log(countAllBugs(boards));
}

// part 1 guesses
// 60885114 too high -- turns out it was exactly double
