// deno-lint-ignore-file
import { getInput } from "./util.ts";

const sampleInput = `
`.trim();

const realInput = (await getInput(import.meta.url)).trim();

const parse = (input: string) => {
  const lines = input.split("\n");
  return lines;
};

function run(input: string) {
  const inp = parse(input);
  console.log(inp);
}

function run2(input: string) {
  const inp = parse(input);
}

console.log("=======================");
run(sampleInput);
// run(realInput);

// console.log();
// run2(sampleInput);
// run2(realInput);
