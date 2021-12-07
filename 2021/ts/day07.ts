import { getInput } from "./util.ts";

const sampleInput = `
16,1,2,0,4,2,7,1,2,14
`.trim();

const realInput = await getInput(import.meta.url);

const parse = (input: string) => {
  return input.split(",").map((n) => parseInt(n, 10));
};

function run(input: string) {
  const inp = parse(input);
  const max = Math.max(...inp);
  let minPos = 1;
  let min = 1000000;
  for (let i = 0; i <= max; i++) {
    const fuel = inp.map((n) => Math.abs(n - i)).reduce((a, b) => a + b);
    if (fuel < min) {
      min = fuel;
      minPos = i;
    }
  }
  console.log(min, minPos);
}

function run2(input: string) {
  const inp = parse(input);
  const max = Math.max(...inp);
  let minPos = 1;
  let min = 1000000000000;
  for (let i = 0; i <= max; i++) {
    const fuel = inp
      .map((n) => {
        const f = Math.abs(n - i);
        return (f * (f + 1)) / 2;
      })
      .reduce((a, b) => a + b);
    if (fuel < min) {
      min = fuel;
      minPos = i;
    }
  }
  console.log(min, minPos);
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
