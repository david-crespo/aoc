import type { Pt } from "./util.ts";
import { ptKey, range } from "./util.ts";

const sampleInput = `
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
`.trim();

const realInput = await Deno.readTextFile("./input/day04.txt");

type Cell = {
  value: number;
  marked: boolean;
};

const ptSeq = (start: Pt, xInc: number, yInc: number, length: number) =>
  range(length).map((i) => [start[0] + i * xInc, start[1] + i * yInc] as Pt);

class Board {
  board: Record<string, Cell> = {};
  sq: number;

  constructor(input: string) {
    const rows = input
      .trim()
      .split("\n")
      .map((row) =>
        row
          .trim()
          .split(/ +/)
          .map((n) => parseInt(n, 10)),
      );
    this.sq = rows.length;
    rows.forEach((row, y) => {
      row.forEach((value, x) => {
        this.set([x, y], { value, marked: false });
      });
    });
  }

  mark(n: number) {
    Object.values(this.board)
      .filter((cell) => cell.value === n)
      .forEach((cell) => {
        cell.marked = true;
      });
  }

  get = (pt: Pt): Cell => this.board[ptKey(pt)];
  set(pt: Pt, cell: Cell) {
    this.board[ptKey(pt)] = cell;
  }

  unmarkedSum = (): number =>
    Object.values(this.board)
      .filter((c) => !c.marked)
      .map((c) => c.value)
      .reduce((a, b) => a + b, 0);

  bingo = (): boolean => {
    const isBingo = (seq: Pt[]) => seq.every((pt) => this.get(pt).marked);
    return [
      // rows
      ptSeq([0, 0], 1, 0, this.sq),
      ptSeq([0, 1], 1, 0, this.sq),
      ptSeq([0, 2], 1, 0, this.sq),
      ptSeq([0, 3], 1, 0, this.sq),
      ptSeq([0, 4], 1, 0, this.sq),
      // cols
      ptSeq([0, 0], 0, 1, this.sq),
      ptSeq([1, 0], 0, 1, this.sq),
      ptSeq([2, 0], 0, 1, this.sq),
      ptSeq([3, 0], 0, 1, this.sq),
      ptSeq([4, 0], 0, 1, this.sq),
    ].some(isBingo);
  };

  print() {
    for (let y = 0; y < this.sq; y++) {
      console.log(
        ptSeq([0, y], 1, 0, this.sq)
          .map((pt) => {
            const cell = this.get(pt);
            const num = cell.value.toString().padStart(2, "0");
            return (cell.marked ? "[" : " ") + num + (cell.marked ? "]" : " ");
          })
          .join(" "),
      );
    }
    console.log();
  }
}

const parse = (input: string): [number[], Board[]] => {
  const [draws, ...boards] = input.split("\n\n");
  return [
    draws.split(",").map((x) => parseInt(x, 10)),
    boards.map((b) => new Board(b)),
  ];
};

function run(input: string) {
  const [draws, boards] = parse(input);
  for (const d of draws) {
    for (const b of boards) {
      b.mark(d);
      if (b.bingo()) {
        b.print();
        console.log(b.unmarkedSum() * d);
        return;
      }
    }
  }
}

function run2(input: string) {
  const [draws, boards] = parse(input);
  let won = 0;
  for (const d of draws) {
    for (const b of boards) {
      if (!b.bingo()) {
        b.mark(d);
        if (b.bingo()) {
          won += 1;
          if (won === boards.length) {
            console.log(b.unmarkedSum() * d);
            return;
          }
        }
      }
    }
  }
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
