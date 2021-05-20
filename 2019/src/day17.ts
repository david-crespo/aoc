import { readIntCodes, ptToStr, strToPt, add, findAllInBoard } from "./util";
import { IntCodeMachine } from "./intCodes";
import { chunk } from "lodash";

function getEdges(board: Board, { x, y }: Point): Point[] {
  const edges: Point[] = [];
  const w = { x: x - 1, y };
  const e = { x: x + 1, y };
  const n = { x, y: y - 1 };
  const s = { x, y: y + 1 };
  if (x > 0) {
    edges.push(w);
  }
  if (x < board[0].length - 1) {
    edges.push(e);
  }
  if (y > 0) {
    edges.push(n);
  }
  if (y < board.length - 1) {
    edges.push(s);
  }
  return edges;
}

export default function() {
  const intCodes = readIntCodes("day17-input.txt");
  const machine = new IntCodeMachine(intCodes);
  const output = machine.runToNextInput();

  const lineLength = output.indexOf(10) + 1;

  const image = String.fromCharCode(...output);
  console.log(image);

  const board: Board = chunk(output, lineLength).map(l => l.slice(0, -1));

  const intersections = findAllInBoard(board, (v, b, x, y) => {
    // console.log(v);
    if (v !== 35) return false;

    const edges = getEdges(b, { x, y });
    // console.log({ x, y }, edges);
    if (edges.length !== 4) return false;

    return edges.every(p => b[p.y][p.x] === 35);
  });

  console.log(intersections);
  console.log(intersections.map(({ x, y }) => x * y).reduce(add));
}
