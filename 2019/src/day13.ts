import { IntCodeMachine } from "./intCodes";
import { readIntCodes } from "./util";
import { sum } from "lodash";

const readline = require("readline-sync");

const H = 25,
  W = 40;

function updateBoard(board: Board, output: number[], score: number): number {
  for (let i = 0; i < output.length; i += 3) {
    const [x, y, tile] = output.slice(i, i + 3);
    if (x == -1 && y == 0 && tile !== 0) {
      score = tile;
    } else {
      board[y][x] = tile;
    }
  }
  return score;
}

function part1() {
  const intCodes = readIntCodes("day13-input.txt");
  const machine = new IntCodeMachine(intCodes);
  const output = machine.runToNextInput([]);

  const board = Array(H)
    .fill(0)
    .map(() => Array(W).fill(0));

  updateBoard(board, output, 0);
  console.log(sum(board.map(row => row.filter(t => t == 2).length)));
}

function draw(board: number[][], score: number) {
  board.forEach(row => {
    console.log(row.map(t => " X#-o"[t]).join(""));
  });
  console.log();
  console.log("Score:", score);
  console.log();
}

type InKey = "a" | "s" | "d";

const inputCodes = {
  a: -1,
  s: 0,
  d: 1,
};

function getBallAndPaddle(board: Board) {
  let ball = { x: 0, y: 0 },
    paddle = { x: 0, y: 0 };
  for (let j = 0; j < board.length; j++) {
    for (let i = 0; i < board[0].length; i++) {
      const tile = board[j][i];
      if (tile === 3) {
        paddle = { x: i, y: j };
      } else if (tile === 4) {
        ball = { x: i, y: j };
      }
    }
  }
  return { ball, paddle };
}

function getMoveInput() {
  const dir: InKey = readline.keyIn("", {
    hideEchoBack: true,
    limit: "asd",
    mask: "",
  });
  return inputCodes[dir];
}

function getMoveAI(board: Board) {
  const { ball, paddle } = getBallAndPaddle(board);
  if (ball.x > paddle.x) {
    return 1;
  } else if (ball.x < paddle.x) {
    return -1;
  } else {
    return 0;
  }
}

export default function() {
  const intCodes = readIntCodes("day13-input.txt");
  intCodes[0] = 2;

  const machine = new IntCodeMachine(intCodes);

  const board = Array(H)
    .fill(0)
    .map(() => Array(W).fill(0));

  let input: number[] = [];
  let score = 0;
  while (!machine.halted) {
    const output = machine.runToNextInput(input);
    score = updateBoard(board, output, score);
    draw(board, score);
    // input = [getMoveInput()];
    input = [getMoveAI(board)];
  }
}
