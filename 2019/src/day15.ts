import { readIntCodes, ptToStr, strToPt } from "./util";
import { IntCodeMachine } from "./intCodes";
const Graph = require("node-dijkstra");

const readline = require("readline-sync");

type MoveDir = 1 | 2 | 3 | 4;
type Answer = 0 | 1 | 2;

const H = 48,
  W = 60;

function draw(board: Board, pos: Point) {
  board.forEach((row, j) => {
    console.log(
      row
        .map((t, i) => (i === pos.x && j === pos.y ? "D" : "· #·X"[t]))
        .join(""),
    );
  });
}

function move({ x, y }: Point, moveDir: MoveDir): Point {
  switch (moveDir) {
    case 1:
      return { x, y: y - 1 };
    case 2:
      return { x, y: y + 1 };
    case 3:
      return { x: x - 1, y };
    case 4:
      return { x: x + 1, y };
  }
}

function updatePos(pos: Point, moveDir: MoveDir, answer: Answer): Point {
  if (answer === 0) {
    // hit wall
    return pos;
  }

  return move(pos, moveDir);
}

function updateBoard(
  board: Board,
  pos: Point,
  moveDir: MoveDir,
  answer: Answer,
) {
  const newPos = move(pos, moveDir);
  if (answer === 0) {
    board[newPos.y][newPos.x] = 2;
  } else if (answer === 1) {
    board[newPos.y][newPos.x] = 1;
  } else if (answer === 2) {
    board[newPos.y][newPos.x] = 4;
  }
}

const isWall = (board: Board, { x, y }: Point) => board[y][x] === 2;
const visited = (board: Board, { x, y }: Point) =>
  board[y][x] === 1 || board[y][x] === 2;

function getEdges(board: Board, { x, y }: Point): Point[] {
  const edges: Point[] = [];
  const w = { x: x - 1, y };
  const e = { x: x + 1, y };
  const n = { x, y: y - 1 };
  const s = { x, y: y + 1 };
  if (x > 0 && !isWall(board, w)) {
    edges.push(w);
  }
  if (x < board[0].length - 1 && !isWall(board, e)) {
    edges.push(e);
  }
  if (y > 0 && !isWall(board, n)) {
    edges.push(n);
  }
  if (y < board.length - 1 && !isWall(board, s)) {
    edges.push(s);
  }
  return edges;
}

function edgesToMap(edges: Point[]): Count {
  const map: Count = {};
  for (let edge of edges) {
    map[ptToStr(edge)] = 1;
  }
  return map;
}

function boardToGraph(board: Board): any {
  const graph: Graph = new Graph();
  for (let i = 0; i < W; i++) {
    for (let j = 0; j < H; j++) {
      if (board[j][i] !== 2) {
        const pt = { x: i, y: j };
        graph.addNode(ptToStr(pt), edgesToMap(getEdges(board, pt)));
      }
    }
  }
  return graph;
}

const ptDiff = (pos: Point, nextPos: Point): Point => ({
  x: nextPos.x - pos.x,
  y: nextPos.y - pos.y,
});

function posToDir(ptDiff: Point): MoveDir {
  const { x: dx, y: dy } = ptDiff;
  if (dx === 1 && dy === 0) {
    return 4;
  } else if (dx === -1 && dy === 0) {
    return 3;
  } else if (dx === 0 && dy === 1) {
    return 2;
  } else {
    return 1;
  }
}

function findInBoard(
  board: Board,
  pred: (n: number, b: Board, x: number, y: number) => boolean,
): Point | null {
  for (let j = 0; j < board.length; j++) {
    for (let i = 0; i < board[0].length; i++) {
      if (pred(board[j][i], board, i, j)) {
        return { x: i, y: j };
      }
    }
  }
  return null;
}

const getPath = (graph: Graph, a: Point, b: Point) =>
  graph.path(ptToStr(a), ptToStr(b));

const getUnvisitedReachable = (
  board: Board,
  graph: Graph,
  pos: Point,
): Point | null =>
  findInBoard(board, (n, board, x, y) => {
    if (n !== 0) {
      return false;
    }
    const path = getPath(graph, pos, { x, y });
    if (path) {
      return true;
    } else {
      board[y][x] = 1;
      return false;
    }
  });

const getTreasure = (board: Board): Point | null =>
  findInBoard(board, n => n === 4);

function nextMove(
  board: Board,
  graph: Graph,
  pos: Point,
  prevTarget: Point | null = null,
): MoveDir | null {
  let target = prevTarget;
  if (!target || !getPath(graph, pos, target)) {
    target = getUnvisitedReachable(board, graph, pos);
  }
  if (!target) {
    return null;
  }
  const path = graph.path(ptToStr(pos), ptToStr(target));
  if (!path) {
    return null;
  }
  console.log("target", target);
  const nextPos = strToPt(path[1]);
  return posToDir(ptDiff(pos, nextPos));
}

const randItem = (arr: any[]): any =>
  arr[Math.floor(Math.random() * arr.length)];

function nextMove2(board: Board, graph: Graph, pos: Point): MoveDir {
  const edges = getEdges(board, pos);
  const vis = edges.filter(p => visited(board, p));
  const nvis = edges.filter(p => !visited(board, p));
  // console.log(pos, vis, nvis);
  const visDirs = vis.map(p => posToDir(ptDiff(pos, p)));
  const nvisDirs = nvis.map(p => posToDir(ptDiff(pos, p)));
  return randItem(nvisDirs) || randItem(visDirs);
}

type InKey = "a" | "s" | "d" | "w" | "q";

const dirCodes: { [k: string]: MoveDir } = {
  w: 1,
  a: 3,
  s: 2,
  d: 4,
};

function nextMove3(board: Board, graph: Graph, pos: Point): MoveDir {
  const dir: InKey = readline.keyIn("", {
    hideEchoBack: true,
    limit: "wasdq",
    mask: "",
  });
  if (dir === "q") {
    throw Error("BOOP");
  }
  return dirCodes[dir];
}

// north (1), south (2), west (3), and east (4)

function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export default function() {
  const intCodes = readIntCodes("day15-input.txt");
  const machine = new IntCodeMachine(intCodes);

  const board: Board = Array(H)
    .fill(0)
    .map(() => Array(W).fill(0));

  const initPos: Point = { x: Math.floor(W / 2), y: Math.floor(H / 2) };

  let moveDir: MoveDir | null;
  let pos = initPos;
  let treasure: Point | null;
  let graph: Graph;

  while (true) {
    graph = boardToGraph(board);
    moveDir = nextMove(board, graph, pos);
    treasure = getTreasure(board);
    if (treasure) {
      const path = graph.path(ptToStr(initPos), ptToStr(treasure));
      console.log("shortest path", path.length - 1);
    }
    if (!moveDir) {
      break;
    }
    // console.log(pos, moveDir);
    const [answer] = machine.runToNextInput([moveDir]) as Answer[];
    updateBoard(board, pos, moveDir, answer);
    console.log("pos", pos);
    pos = updatePos(pos, moveDir, answer);
    console.log("treasure", treasure);
    draw(board, pos);
    // await sleep(0);
  }

  if (!treasure) {
    return;
  }

  // longest path to any reachable point from treasure
  let longest = 0;
  for (let i = 0; i < board[0].length; i++) {
    for (let j = 0; j < board.length; j++) {
      if (board[j][i] === 1) {
        const path = getPath(graph, treasure, { x: i, y: j });
        if (path) {
          const pathLength = path.length - 1;
          if (pathLength > longest) {
            longest = pathLength;
          }
        }
      }
    }
  }
  console.log("minutes to fill", longest);

  // console.log("FOUND IT", treasure);
  // const graph = boardToGraph(board);
  // const path = graph.path(ptToStr(initPos), ptToStr(treasure)).map(strToPt);
  // for (let p of path) {
  //   board[p.y][p.x] = 3;
  // }
  // draw(board, initPos);
  // // console.log(path);
  // console.log(path.length - 1);
}
