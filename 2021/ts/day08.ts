import { getInput } from "./util.ts";

const sampleInput = `
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
`.trim();

const realInput = (await getInput(import.meta.url)).trim();

const parse = (input: string) => {
  const lines = input.split("\n").map((s) => {
    const [a, b] = s.split(" | ");
    return [a.split(" "), b.split(" ")];
  });
  return lines;
};

function run(input: string) {
  const inp = parse(input);
  console.log(
    inp
      .map(([a, b]) => b.filter((s) => [2, 3, 4, 7].includes(s.length)).length)
      .reduce((a, b) => a + b, 0),
  );
}

//   0:      1:      2:      3:      4:
//  aaaa    ....    aaaa    aaaa    ....
// b    c  .    c  .    c  .    c  b    c
// b    c  .    c  .    c  .    c  b    c
//  ....    ....    dddd    dddd    dddd
// e    f  .    f  e    .  .    f  .    f
// e    f  .    f  e    .  .    f  .    f
//  gggg    ....    gggg    gggg    ....

//   5:      6:      7:      8:      9:
//  aaaa    aaaa    aaaa    aaaa    aaaa
// b    .  b    .  .    c  b    c  b    c
// b    .  b    .  .    c  b    c  b    c
//  dddd    dddd    ....    dddd    dddd
// .    f  e    f  .    f  e    f  .    f
// .    f  e    f  .    f  e    f  .    f
//  gggg    gggg    ....    gggg    gggg

const diff = <T>(a: Set<T>, b: Set<T>) =>
  new Set([...a].filter((x) => !b.has(x)));

const inter = <T>(a: Set<T>, b: Set<T>) =>
  new Set([...a].filter((x) => b.has(x)));

const eq = <T>(a: Set<T>, b: Set<T>) =>
  a.size === b.size && inter(a, b).size === a.size;

const union = <T>(a: Set<T>, b: Set<T>) => new Set([...a, ...b]);

const decode = (mapping: Set<string>[], s: string) => {
  const sSet = new Set(s.split(""));
  for (let i = 0; i < 10; i++) {
    if (eq(mapping[i], sSet)) {
      return i;
    }
  }
};

function run2(input: string) {
  const inp = parse(input);
  // console.log(inp);
  let sum = 0;
  inp.forEach(([digits, out]) => {
    const m: Set<string>[] = new Array(10);
    m[1] = new Set(digits.find((d) => d.length === 2)?.split(""));
    m[4] = new Set(digits.find((d) => d.length === 4)?.split(""));
    m[7] = new Set(digits.find((d) => d.length === 3)?.split(""));
    m[8] = new Set(digits.find((d) => d.length === 7)?.split(""));

    const top = diff(m[7], m[1]);
    const bottomLeft = diff(diff(m[8], m[4]), top);
    const twoFiveThree = digits
      .filter((d) => d.length === 5)
      .map((s) => new Set(s.split("")));
    // two contains bottom left
    m[2] = twoFiveThree.find((n) => [...bottomLeft].every((l) => n.has(l)))!;
    const fiveThree = twoFiveThree.filter((n) => !eq(n, m[2]));

    m[9] = union(fiveThree[0], fiveThree[1]);

    const middle = diff(diff(diff(m[2], m[1]), top), bottomLeft);
    m[0] = diff(m[8], middle);

    if (diff(fiveThree[0], m[7]).size === 3) {
      m[5] = fiveThree[0];
      m[3] = fiveThree[1];
    } else {
      m[5] = fiveThree[1];
      m[3] = fiveThree[0];
    }

    m[6] = union(diff(m[8], m[1]), m[5]);

    sum += parseInt(out.map((s) => decode(m, s)!.toString()).join(""), 10);
  });
  console.log(sum);
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
