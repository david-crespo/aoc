import * as fs from "fs";
import * as path from "path";
import { range } from "lodash";
const Graph = require("node-dijkstra");

export const readFile = (fileName: string) =>
  fs.readFileSync(path.resolve(__dirname, fileName), "utf8");

export const parseIntCodes = (input: string) =>
  input
    .trim()
    .split(",")
    .map(s => parseInt(s, 10));

export const readIntCodes = (fileName: string) =>
  parseIntCodes(readFile(fileName));

export function perms<T>(items: Set<T>): T[][] {
  if (items.size === 1) {
    const item = Array.from(items)[0];
    return [[item]];
  }

  let result: T[][] = [];
  items.forEach(item => {
    const itemsWithout = new Set(items);
    itemsWithout.delete(item);
    result = [...result, ...perms(itemsWithout).map(p => [item, ...p])];
  });
  return result;
}

function isPoint3D(pt: Point | Point3D): pt is Point3D {
  return typeof (pt as Point3D).z !== "undefined";
}

export const ptToStr = (pt: Point | Point3D) =>
  isPoint3D(pt) ? `${pt.x},${pt.y},${pt.z}` : `${pt.x},${pt.y}`;

export const strToPt = (s: string): Point => {
  const [x, y] = s.split(",").map(z => parseInt(z, 10));
  return { x, y };
};

export const add = (a: number, b: number) => a + b;

export function lcm(x: number, y: number) {
  return !x || !y ? 0 : Math.abs((x * y) / gcd(x, y));
}

export function gcd(x: number, y: number) {
  x = Math.abs(x);
  y = Math.abs(y);
  while (y) {
    var t = y;
    y = x % y;
    x = t;
  }
  return x;
}

export const newBoard = <T>(w: number, h: number, fillValue: T): T[][] =>
  Array(h)
    .fill(fillValue)
    .map(() => Array(w).fill(fillValue));

export function assertEq<T>(a: T, b: T) {
  if (a !== b) {
    throw Error(`test failed: expected ${b} but got ${a}`);
  }
}

export function findInBoard<T>(
  board: T[][],
  pred: (val: T, b: T[][], x: number, y: number) => boolean,
): Point | undefined {
  for (let j = 0; j < board.length; j++) {
    for (let i = 0; i < board[0].length; i++) {
      if (pred(board[j][i], board, i, j)) {
        return { x: i, y: j };
      }
    }
  }
  return undefined;
}

export function findAllInBoard<T>(
  board: T[][],
  pred: (val: T, b: T[][], x: number, y: number) => boolean,
): Point[] {
  const out = [];
  for (let j = 0; j < board.length; j++) {
    for (let i = 0; i < board[0].length; i++) {
      if (pred(board[j][i], board, i, j)) {
        out.push({ x: i, y: j });
      }
    }
  }
  return out;
}

export function boardEach<T>(
  board: T[][],
  f: (b: T[][], x: number, y: number) => void,
): void {
  for (let j = 0; j < board.length; j++) {
    for (let i = 0; i < board[0].length; i++) {
      f(board, i, j);
    }
  }
}

export function mapBoard<T>(
  board: T[][],
  fn: (val: T, b: T[][], x: number, y: number) => T,
): T[][] {
  const out = [];
  for (let j = 0; j < board.length; j++) {
    const row = [];
    for (let i = 0; i < board[0].length; i++) {
      row.push(fn(board[j][i], board, i, j));
    }
    out.push(row);
  }
  return out;
}

export const copyBoard = <T>(board: T[][]) => mapBoard(board, v => v);

export const sliceBoard = <T>(
  board: T[][],
  startX: number,
  startY: number,
  endX: number,
  endY: number,
) => board.slice(startY, endY).map(row => row.slice(startX, endX));

export function drawBoard<T>(
  board: T[][],
  fn: (val: T, b: T[][], x: number, y: number) => string = v => v,
) {
  for (let j = 0; j < board.length; j++) {
    console.log(board[j].map((c, i) => fn(c, board, i, j)).join(""));
  }
  console.log();
}

// assumes arr is sorted w.r.t. fn, i.e., if fn(i) == true
// then fn(j) == true for all j < i and if fn(i) == false then
// fn(j) == false for all j > i
export function binSearch<T>(arr: T[], fn: (i: T) => boolean): T | null {
  console.log(arr);
  if (arr.length === 0) return null;
  if (arr.length === 1) return fn(arr[0]) ? arr[0] : null;

  const cutIdx = Math.floor(arr.length / 2);
  const head = arr.slice(0, cutIdx);
  const testItem = arr[cutIdx];
  const tail = arr.slice(cutIdx + 1);
  console.log(head, testItem, tail);
  console.log();
  if (fn(testItem)) {
    return binSearch(tail, fn);
  } else {
    return binSearch(head, fn);
  }
}

export const getCell = <T>(board: T[][]) => ({ x, y }: Point): T => board[y][x];

export function getEdges<T>(board: T[][], { x, y }: Point): Point[] {
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

export const getCol = <T>(board: T[][], x: number): T[] =>
  range(board.length).map(j => board[j][x]);

export function addEdge(g: Graph, a: string, b: string, weight = 1) {
  const startEdges = g.graph.get(a);
  if (startEdges) {
    startEdges.set(b, weight);
  } else {
    g.addNode(a, { [b]: weight });
  }
}

export function removeEdge(g: Graph, a: string, b: string) {
  const aEdges = g.graph.get(a);
  if (aEdges) {
    aEdges.delete(b);
  }
}

export class PtGraph<Pt extends Point | Point3D> {
  graph: Graph;

  constructor() {
    this.graph = new Graph();
  }

  addEdge(a: Pt, b: Pt, weight = 1) {
    addEdge(this.graph, ptToStr(a), ptToStr(b), weight);
  }

  removeEdge(a: Pt, b: Pt) {
    removeEdge(this.graph, ptToStr(a), ptToStr(b));
  }

  path(a: Pt, b: Pt, options: { avoid: Pt[] } = { avoid: [] }) {
    const p = this.graph.path(ptToStr(a), ptToStr(b), {
      avoid: options.avoid.map(ptToStr),
    });
    return p ? p.map(strToPt) : null;
  }
}

export const union = <T>(a: Set<T>, b: Set<T>): Set<T> => new Set([...a, ...b]);

export function intersection<T>(a: Set<T>, b: Set<T>): Set<T> {
  let _intersection = new Set<T>();
  for (let elem of b) {
    if (a.has(elem)) {
      _intersection.add(elem);
    }
  }
  return _intersection;
}

export function symDiff<T>(a: Set<T>, b: Set<T>): Set<T> {
  let _difference = new Set(a);
  for (let elem of b) {
    if (_difference.has(elem)) {
      _difference.delete(elem);
    } else {
      _difference.add(elem);
    }
  }
  return _difference;
}

export function diff<T>(a: Set<T>, b: Set<T>): Set<T> {
  let _difference = new Set(a);
  for (let elem of b) {
    _difference.delete(elem);
  }
  return _difference;
}

export const ptEq = (a: Point, b: Point) => a.x === b.x && a.y === b.y;
