import {
  readFile,
  assertEq,
  boardEach,
  findAllInBoard,
  PtGraph,
  getCell,
  diff,
  ptEq,
  addEdge,
  drawBoard,
} from "./util";
import TinyQueue from "tinyqueue";
import { sortBy, range, flatMap, findLast, findIndex } from "lodash";
const Graph = require("node-dijkstra");

const test1 = `
#########
#b.A.@.a#
#########
`.trim();

const test2 = `
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
`.trim();

const test3 = `
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
`.trim();

const test4 = `
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
`.trim();

// the trick to constrain the set of paths is partial ordering:
// because i is in a dead end behind G, you know g -> i regardless
// of what else happens. you also have e -> k, f -> l, c -> m, etc.
// in practice you need a getBlockedBy() function for each letter that
// would let you generate this partial ordering

const test5 = `
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
`.trim();

function isAccessible(board: Bord, pt: Point, allowCaps = false) {
  const val = board[pt.y][pt.x];
  const re = allowCaps ? /[a-zA-Z]|\.|\@/ : /[a-z]|\.|\@/;
  return re.test(val);
}

function getEdges(board: Bord, { x, y }: Point, allowCaps = false): Point[] {
  const edges: Point[] = [];
  const w = { x: x - 1, y };
  const e = { x: x + 1, y };
  const n = { x, y: y - 1 };
  const s = { x, y: y + 1 };
  if (x > 0 && isAccessible(board, w, allowCaps)) {
    edges.push(w);
  }
  if (x < board[0].length - 1 && isAccessible(board, e, allowCaps)) {
    edges.push(e);
  }
  if (y > 0 && isAccessible(board, n, allowCaps)) {
    edges.push(n);
  }
  if (y < board.length - 1 && isAccessible(board, s, allowCaps)) {
    edges.push(s);
  }
  return edges;
}

function boardToGraphFull(board: Bord, allowCaps = false): PtGraph<Point> {
  const graph = new PtGraph();

  boardEach(board, (b, x, y) => {
    const pt = { x, y };
    if (isAccessible(b, pt, allowCaps)) {
      const edges = getEdges(b, pt, allowCaps);
      for (let edgePt of edges) {
        graph.addEdge(pt, edgePt);
      }
    }
  });

  return graph;
}

const parseMap = (input: string) =>
  input
    .trim()
    .split("\n")
    .map(s => Array.from(s));

type Edge = {
  start: string;
  end: string;
  weight: number;
};

function violatesOrder(path: string[], partialOrder: POMap) {
  const pathSet = new Set(path);
  for (let [start, end] of iterPO(partialOrder)) {
    if (
      (pathSet.has(end) && !pathSet.has(start)) ||
      (pathSet.has(start) &&
        pathSet.has(end) &&
        path.indexOf(end) < path.indexOf(start))
    ) {
      return true;
    }
  }
  return false;
}

const isLower = (v: string) => /[a-z]/.test(v);
const isLetter = (v: string) => /[a-zA-Z]/.test(v);
const isAt = (v: string) => v === "@";

type Slots = Set<string>[];

const getEndCount = (po: POMap, end: string) =>
  Array.from(po.values()).filter(ends => ends.has(end)).length;

const getStartCount = (po: POMap, start: string) => po.get(start)?.size || 0;

function getSlots(letters: string[], partialOrder: POMap): Slots {
  const slots: Slots = new Array(letters.length).fill(0).map(() => new Set());

  for (const letter of letters) {
    const minIdx = getEndCount(partialOrder, letter);
    const maxIdx = letters.length - getStartCount(partialOrder, letter);
    range(minIdx, maxIdx).forEach(i => {
      slots[i].add(letter);
    });
  }

  return slots;
}

const getWeight = (g: Graph, a: string, b: string): number =>
  g.graph.get(a)?.get(b) || 0;

function printPO(partialOrder: POMap) {
  const starts = sortBy(Array.from(partialOrder.keys()));
  console.log(
    flatMap(starts, start => {
      const startEdges = partialOrder.get(start)!;
      return Array.from(startEdges).map(end => start + end);
    }),
  );
}

function getPartialOrder(board: Bord): POMap {
  const starts = findAllInBoard(board, v => v === "@")!;
  const letterPts = findAllInBoard(board, v => /[a-z]/.test(v));
  const fullGraph = boardToGraphFull(board, true);

  const partialOrder: POMap = new Map();

  for (const start of starts) {
    for (const letterPt of letterPts) {
      const path = fullGraph.path(start, letterPt);
      if (!path) continue;
      const capsInWay = path.filter(pt => /[A-Z]/.test(getCell(board)(pt)));

      // look at both caps and lowers because even though lowers can't block,
      // their order can still be used to prune enormously
      let blockers = capsInWay;
      blockers = capsInWay.concat(letterPts);
      for (const pt of blockers) {
        if (!ptEq(pt, letterPt)) {
          const stillPath = fullGraph.path(start, letterPt, { avoid: [pt] });
          if (!stillPath) {
            const cap = getCell(board)(pt);
            const start = cap.toLowerCase();
            const end = getCell(board)(letterPt);
            if (!partialOrder.has(start)) {
              partialOrder.set(start, new Set());
            }
            if (!partialOrder.get(start)!.has(end)) {
              partialOrder.get(start)!.add(end);
            }
          }
        }
      }
    }
  }

  return partialOrder;
}

function boardToLetterGraph(
  board: Bord,
  letterToQ?: Map<string, number>,
): Graph {
  const starts = findAllInBoard(board, v => v === "@")!;
  const letterPts = findAllInBoard(board, isLower);
  const blockedGraph = boardToGraphFull(board);
  const openGraph = boardToGraphFull(board, true);
  const graph: Graph = new Graph();

  for (const start of starts) {
    for (const pt1 of letterPts) {
      const letter1 = getCell(board)(pt1);

      const path = openGraph.path(start, pt1);
      if (path) {
        const qIdx = letterToQ?.get(letter1);
        addEdge(
          graph,
          `@${typeof qIdx !== "undefined" ? qIdx : ""}`,
          letter1,
          path.length - 1,
        );
      }

      for (const pt2 of letterPts) {
        const letter2 = getCell(board)(pt2);
        if (!ptEq(pt1, pt2)) {
          const path = openGraph.path(pt1, pt2)!;
          if (path) {
            addEdge(graph, letter1, letter2, path.length - 1);
          }
        }
      }
    }
  }
  return graph;
}

type PartialPath = {
  path: string[];
  weight: number;
};

type POMap = Map<string, Set<string>>;

function* iterPO(po: POMap) {
  for (const [start, ends] of po.entries()) {
    for (const end of ends) {
      yield [start, end];
    }
  }
}

function run3(input: string) {
  const board = parseMap(input);
  const allLowers = new Set(findAllInBoard(board, isLower).map(getCell(board)));
  const letterGraph = boardToLetterGraph(board);
  const fullPathLength = allLowers.size + 1; // + 1 for @
  const partialOrder = getPartialOrder(board);
  const slots = getSlots(Array.from(allLowers), partialOrder);
  // printPO(partialOrder);
  // console.log(slots);

  const pathQueue = new TinyQueue<PartialPath>(
    [{ path: ["@"], weight: 0 }],
    (a, b) =>
      a.weight / Math.pow(a.path.length, 1.2) -
      b.weight / Math.pow(b.path.length, 1.2),
  );

  let bestFullPath: PartialPath | undefined = undefined;

  let i = 0;
  while (pathQueue.length > 0) {
    i += 1;
    const { path: currPath, weight: currWeight } = pathQueue.pop()!;
    const last = currPath[currPath.length - 1];
    const slotLetters = slots[currPath.length];
    if (i % 10000 === 0) {
      console.log(i, currWeight, currPath.join(""));
    }
    for (const nextLetter of diff(slotLetters, new Set(currPath))) {
      const path = [...currPath, nextLetter];
      if (!violatesOrder(path, partialOrder)) {
        const weight = currWeight + getWeight(letterGraph, last, nextLetter);
        if (
          path.length === fullPathLength &&
          (!bestFullPath || bestFullPath!.weight > weight)
        ) {
          bestFullPath = { path, weight };
          console.log("NEW BEST", i, bestFullPath);
        } else {
          pathQueue.push({ path, weight });
        }
      }
    }
  }
  // the use of priority queue by weight saves us the trouble of pruning. the problem
  // is, rather, when to stop the loop. presumably the first full path we find will not
  // be a shortest path, and we soon find shorter ones. maybe stop after N iterations
  // without finding a shorter path
  return bestFullPath!.weight;
}

function printBoard(board: Bord) {
  for (const row of board) {
    console.log(row.join(""));
  }
}

export function test(run: (input: string) => number | undefined) {
  assertEq(run(test1), 8);
  assertEq(run(test2), 86);
  assertEq(run(test3), 132);
  assertEq(run(test4), 136);
  assertEq(run(test5), 81);
  console.log("all tests passed");
}

const test6 = `
###############
#d.ABC.#.....a#
######@#@######
###############
######@#@######
#b.....#.....c#
###############
`.trim();

const test7 = `
#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############
`.trim();

const test8 = `
#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba@#@BcIJ#
#############
#nK.L@#@G...#
#M###N#H###.#
#o#m..#i#jk.#
#############
`.trim();

function quadify(board: Bord) {
  const [w, h] = [board[0].length, board.length];
  const [w2, h2] = [Math.floor(w / 2), Math.floor(h / 2)];

  board[h2][w2] = "#";
  board[h2 + 1][w2] = "#";
  board[h2 - 1][w2] = "#";
  board[h2][w2 + 1] = "#";
  board[h2][w2 - 1] = "#";

  board[h2 + 1][w2 + 1] = "@";
  board[h2 - 1][w2 + 1] = "@";
  board[h2 + 1][w2 - 1] = "@";
  board[h2 - 1][w2 - 1] = "@";

  // const q1 = sliceBoard(board, 0, 0, w2 + 1, h2 + 1);
  // const q2 = sliceBoard(board, w2, 0, w, h2 + 1);
  // const q3 = sliceBoard(board, w2, h2, w, h);
  // const q4 = sliceBoard(board, 0, h2, w2 + 1, h);
  // return [q1, q2, q3, q4];
  return board;
}

const getLetterToQ = (board: Bord): Map<string, number> => {
  const ats = findAllInBoard(board, isAt);
  const letterPts = findAllInBoard(board, isLower);
  const openGraph = boardToGraphFull(board, true);
  const map = new Map<string, number>();
  for (const letterPt of letterPts) {
    const letter = getCell(board)(letterPt);
    const q = findIndex(ats, at => !!openGraph.path(at, letterPt));
    map.set(letter, q);
  }
  return map;
};

function run4(input: string) {
  const board = quadify(parseMap(input));
  // drawBoard(board);
  const allLowers = new Set(findAllInBoard(board, isLower).map(getCell(board)));
  const fullPathLength = allLowers.size; // + 1 if we want to include @
  const letterToQ = getLetterToQ(board);
  // console.log(letterToQ);
  const letterGraph = boardToLetterGraph(board, letterToQ);
  // console.log(letterGraph);
  const po = getPartialOrder(board);

  const slots = getSlots(Array.from(allLowers), po);
  // console.log(slots);

  const pathQueue = new TinyQueue<PartialPath>(
    [{ path: [], weight: 0 }],
    (a, b) =>
      a.weight / Math.pow(a.path.length, 1.2) -
      b.weight / Math.pow(b.path.length, 1.2),
  );

  let bestFullPath: PartialPath | undefined = undefined;

  let i = 0;
  while (pathQueue.length > 0) {
    i += 1;
    const { path: currPath, weight: currWeight } = pathQueue.pop()!;
    const slotLetters = slots[currPath.length];
    if (i % 10000 === 0) {
      console.log(i, currWeight, currPath.join(""));
    }
    for (const nextLetter of diff(slotLetters, new Set(currPath))) {
      const path = [...currPath, nextLetter];
      if (!violatesOrder(path, po)) {
        const nextLetterQ = letterToQ.get(nextLetter)!;
        const lastForQ =
          findLast(currPath, l => letterToQ.get(l) === nextLetterQ) ||
          `@${nextLetterQ}`;
        const incWeight = getWeight(letterGraph, lastForQ, nextLetter);
        // console.log(" ", { nextLetter, nextLetterQ, incWeight, lastForQ });
        const weight = currWeight + incWeight;
        if (
          path.length === fullPathLength &&
          (!bestFullPath || bestFullPath!.weight > weight)
        ) {
          bestFullPath = { path, weight };
          console.log("NEW BEST after", i, bestFullPath);
        } else {
          pathQueue.push({ path, weight });
        }
      }
    }
  }

  console.log("best", bestFullPath);
  return bestFullPath!.weight;
}

export default function() {
  const input = readFile("day18-input.txt");

  // run3(test4);
  // test(run3);

  // run4(test6);
  // run4(test7);
  // run4(test8);

  run4(input);
}

// answer is lower than 5410
// right answer is 5406, after 1045115 iterations of run3

// part 2
// 5406 is too high (581806 iterations of run4)

// right answer:

// NEW BEST after 336298 {
//   path: [
//     's', 'l', 'a', 'o', 'w', 'e',
//     'b', 'i', 'c', 'q', 'j', 'h',
//     't', 'u', 'z', 'k', 'f', 'x',
//     'n', 'p', 'm', 'v', 'g', 'r',
//     'd', 'y'
//   ],
//   weight: 1938
// }
