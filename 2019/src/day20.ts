import {
  findAllInBoard,
  ptToStr,
  strToPt,
  readFile,
  getEdges,
  PtGraph,
} from "./util";
import { flatten, remove, sortBy } from "lodash";
const Graph = require("node-dijkstra");

const semiParse = (input: string): string[][] =>
  input.split("\n").map(r => Array.from(r));

const test = semiParse(
  `         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z     `,
);

function nextDoor(board: string[][], a: Point, b: Point) {
  const aEdges = new Set(getEdges(board, a).map(ptToStr));
  return aEdges.has(ptToStr(b));
}

function getLetterPairs(board: string[][], letters: Point[]): Point[][] {
  const pairs = [];
  while (letters.length > 0) {
    const a = letters.pop() as Point;
    const [b] = remove(letters, p => nextDoor(board, a, p));
    const pair = sortBy([a, b], p => p.x * 100000 + p.y);
    pairs.push(pair);
  }
  return pairs;
}

const getVal = (board: string[][], { x, y }: Point) => board[y][x];
const isOuter = (board: string[][], p: Point) =>
  p.x === 2 ||
  p.x === board[0].length - 3 ||
  p.y === 2 ||
  p.y === board.length - 3;

type Portals = { [k: string]: { inner: Point; outer: Point } };

function getPortals(board: string[][]) {
  const portals: { [k: string]: { inner?: Point; outer?: Point } } = {};
  let start: Point = { x: 0, y: 0 },
    end: Point = { x: 0, y: 0 };

  const letters = findAllInBoard(board, v => /[A-Z]/.test(v));
  const letterPairs = getLetterPairs(board, letters);
  for (let pair of letterPairs) {
    const label = pair.map(p => getVal(board, p)).join("");
    const nearestWalkway = flatten(pair.map(p => getEdges(board, p))).filter(
      pt => getVal(board, pt) === ".",
    )[0];

    if (label === "AA") {
      start = nearestWalkway;
      continue;
    }

    if (label === "ZZ") {
      end = nearestWalkway;
      continue;
    }

    if (!portals.hasOwnProperty(label)) portals[label] = {};
    const k = isOuter(board, nearestWalkway) ? "outer" : "inner";
    portals[label][k] = nearestWalkway;
  }

  return { portals: portals as Portals, start, end };
}

function simpleShortestPath(board: string[][]) {
  const { portals, start, end } = getPortals(board);

  const pg = new PtGraph<Point>();
  Object.values(portals).forEach(({ inner, outer }) => {
    pg.addEdge(inner, outer);
    pg.addEdge(outer, inner);
  });

  findAllInBoard(board, (v, b, i, j) => {
    if (v === ".") {
      const pt = { x: i, y: j };
      const walkwayNeighbors = getEdges(b, pt).filter(
        e => getVal(b, e) === ".",
      );
      walkwayNeighbors.forEach(n => pg.addEdge(pt, n));
    }
    return false;
  });

  const path = pg.path(start, end);
  return path ? path.length - 1 : null;
}

function complexShortestPath(board: string[][], maxLevel = 0) {
  const { portals, start, end } = getPortals(board);
  const pg = new PtGraph<Point3D>();

  for (let level = 0; level <= maxLevel; level++) {
    Object.values(portals).forEach(({ inner, outer }) => {
      const [inner3, outer3] = [
        { ...inner, z: level },
        { ...outer, z: level + 1 },
      ];
      pg.addEdge(inner3, outer3);
      pg.addEdge(outer3, inner3);
    });

    findAllInBoard(board, (v, b, i, j) => {
      if (v === ".") {
        const pt = { x: i, y: j, z: level };
        const walkwayNeighbors = getEdges(b, pt).filter(
          e => getVal(b, e) === ".",
        );
        walkwayNeighbors.forEach(n => pg.addEdge(pt, { ...n, z: level }));
      }
      return false;
    });
  }

  const path = pg.path({ ...start, z: 0 }, { ...end, z: 0 });
  return path ? path.length - 1 : null;
}

export default function() {
  const board = semiParse(readFile("day20-input.txt"));
  // console.log(simpleShortestPath(board));

  for (let maxLevel = 0; maxLevel < 25; maxLevel++) {
    console.log(maxLevel, complexShortestPath(board, maxLevel));
  }
}
