import { readFile } from "./util";
import { zip, last } from "lodash";

type OrbitMap = string[][];
type Pair = string[];
type Chain = string[];

const testMap = `
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L`;

const testMap2 = `
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN`;

const add = (a: number, b: number) => a + b;

function findIntersection(chain1: Chain, chain2: Chain): string {
  chain1 = [...chain1].reverse();
  chain2 = [...chain2].reverse();
  const firstFalse = zip(chain1, chain2).findIndex(([a, b]) => a !== b);
  return chain1[firstFalse - 1];
}

const distToCom = (pairs: OrbitMap) => (obj: string) =>
  chainToCom(pairs, obj).length - 1;

function chainToCom(pairs: OrbitMap, obj: string) {
  let chain = [obj];
  while (last(chain) != "COM") {
    const [left, right] = pairs.find(p => p[1] == last(chain)) as Pair;
    chain.push(left);
  }
  return chain;
}

const parseMap = (input: string) =>
  input
    .trim()
    .split("\n")
    .map(s => s.split(")"));

export default function() {
  const orbitMap = parseMap(readFile("day6-input.txt"));
  // const orbitMap = parseMap(testMap);
  // const orbitMap = parseMap(testMap2);

  const allObjects = new Set(Array.prototype.concat.apply([], orbitMap));
  allObjects.delete("COM");

  console.log(
    Array.from(allObjects)
      .map(distToCom(orbitMap))
      .reduce(add),
  );

  const you = chainToCom(orbitMap, "YOU");
  const san = chainToCom(orbitMap, "SAN");
  const interObj = findIntersection(you, san);
  const inter = chainToCom(orbitMap, interObj);
  console.log(you.length - 1 + (san.length - 1) - 2 * (inter.length - 1) - 2);
}
