import type { Pt } from "./util.ts";
import { getInput, ptKey } from "./util.ts";

const sampleInput = `
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
`.trim();

const ptSeq = ([x1, y1]: Pt, [x2, y2]: Pt) => {
  const result: Pt[] = [];
  if (x1 == x2) {
    const min = Math.min(y1, y2);
    const max = Math.max(y1, y2);
    for (let y = min; y <= max; y++) {
      result.push([x1, y]);
    }
  } else if (y1 == y2) {
    const min = Math.min(x1, x2);
    const max = Math.max(x1, x2);
    for (let x = min; x <= max; x++) {
      result.push([x, y1]);
    }
  } else {
    // comment this part out for part 1
    const yInc = Math.sign(y2 - y1);
    const xInc = Math.sign(x2 - x1);
    for (let i = 0; i <= Math.abs(x2 - x1); i += 1) {
      result.push([x1 + i * xInc, y1 + i * yInc]);
    }
  }
  return result;
};

const realInput = await getInput(import.meta.url);

const parse = (input: string) => {
  return input.split("\n").flatMap((line) => {
    const [start, end] = line.split(" -> ");
    const [x1, y1] = start.split(",");
    const [x2, y2] = end.split(",");
    return ptSeq(
      [parseInt(x1, 10), parseInt(y1, 10)],
      [parseInt(x2, 10), parseInt(y2, 10)],
    );
  });
};

function run(input: string) {
  const inp = parse(input);
  const pts: Record<string, number> = {};
  for (const pt of inp) {
    const key = ptKey(pt);
    if (!(key in pts)) {
      pts[key] = 0;
    }
    pts[key] += 1;
  }
  console.log(Object.values(pts).filter((v) => v > 1).length);
}

console.log("=======================");
run(sampleInput);
run(realInput);
