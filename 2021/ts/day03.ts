const sampleInput = `
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
`.trim();

const realInput = await Deno.readTextFile("./input/day03.txt");

const count1s = (n: Bits) => n.filter((x) => x === "1").length;

const mostCommon = (n: Bits) => {
  const trues = n.filter((x) => x === "1").length;
  return trues > n.length / 2 ? "1" : "0";
};
const leastCommon = (n: Bits) => (mostCommon(n) === "1" ? "0" : "1");

const nthCol = (ns: Bits[], i: number) => ns.map((n) => n[i]);

type Bit = "1" | "0";
type Bits = Bit[];

const parse = (input: string): Bits[] =>
  input.split("\n").map((line) => line.split("") as Bits);

function run(input: string) {
  const ns = parse(input);
  const cols = ns[0].length;
  const gamma = parseInt(
    new Array(cols)
      .fill(0)
      .map((_, i) => mostCommon(nthCol(ns, i)))
      .join(""),
    2,
  );
  const epsilon = parseInt(
    new Array(cols)
      .fill(0)
      .map((_, i) => leastCommon(nthCol(ns, i)))
      .join(""),
    2,
  );

  console.log(gamma * epsilon);
}

function run2(input: string) {
  const ns = parse(input);
  const cols = ns[0].length;

  let ox = [...ns];
  for (let i = 0; i < cols; i++) {
    const n1s = count1s(nthCol(ox, i));
    if (n1s >= ox.length / 2) {
      ox = ox.filter((n) => n[i] === "1");
    } else {
      ox = ox.filter((n) => n[i] === "0");
    }
    if (ox.length === 1) break;
  }

  let co2 = [...ns];
  for (let i = 0; i < cols; i++) {
    const n1s = count1s(nthCol(co2, i));
    if (n1s >= co2.length / 2) {
      co2 = co2.filter((n) => n[i] === "0");
    } else {
      co2 = co2.filter((n) => n[i] === "1");
    }
    if (co2.length === 1) break;
  }

  const oxResult = parseInt(ox[0].join(""), 2);
  console.log(oxResult);

  const co2Result = parseInt(co2[0].join(""), 2);
  console.log(co2Result);
  console.log("result: ", oxResult * co2Result);
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
