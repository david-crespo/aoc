import { readIntCodes, newBoard, drawBoard, findAllInBoard } from "./util";
import { IntCodeMachine } from "./intCodes";

const readline = require("readline-sync");

function scanArea(
  initX: number,
  initY: number,
  w: number,
  h: number,
  intCodes: number[],
) {
  const board = newBoard(w, h, 0);
  for (let i = 0; i < w; i++) {
    for (let j = 0; j < h; j++) {
      const machine = new IntCodeMachine(intCodes);
      const [status] = machine.runToNextInput([j + initY, i + initX]);
      board[j][i] = status;
    }
  }
  return board;
}

const draw = (board: Board) => drawBoard(board, v => ".x0="[v]);

function part1(intCodes: number[]) {
  const board = scanArea(0, 0, 100, 100, intCodes);
  draw(board);
  console.log("area affected", findAllInBoard(board, v => v === 1).length);
}

function canFit(board: Board, n: number, x: number, y: number) {
  const squaresInBeam = findAllInBoard(board, (v, b, i, j) => {
    const inSq = i >= x && i < x + n && j >= y && j < y + n;
    if (inSq) {
      b[j][i] = v === 1 ? 2 : 3;
    }

    return v === 1 && inSq;
  });
  return squaresInBeam.length === n * n;
}

type InKey = "w" | "a" | "s" | "d" | "q";
type MoveDir = "U" | "D" | "L" | "R";

function getDir(): MoveDir {
  const dir: InKey = readline.keyIn("", {
    hideEchoBack: true,
    limit: "wasdq",
    mask: "",
  });
  if (dir === "q") {
    throw Error("BOOP");
  }
  return { w: "U", a: "L", s: "D", d: "R" }[dir] as MoveDir;
}

export default function() {
  const intCodes = readIntCodes("day19-input.txt");
  // part1(intCodes);

  let baseX = 1348,
    baseY = 759,
    moveDir: MoveDir;

  while (true) {
    const board = scanArea(baseX, baseY, 110, 110, intCodes);
    const x = 5;
    const y = 5;
    const fit = canFit(board, 100, x, y);
    draw(board);
    console.log({
      x: x + baseX,
      y: y + baseY,
      result: (x + baseX) * 10000 + (y + baseY),
      fit,
    });
    moveDir = getDir();
    if (moveDir === "U") {
      baseY -= 1;
    } else if (moveDir === "D") {
      baseY += 1;
    } else if (moveDir === "L") {
      baseX -= 1;
    } else if (moveDir === "R") {
      baseX += 1;
    }
  }
}

// 7641353 is apparently too low
