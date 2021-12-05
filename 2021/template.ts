// deno-lint-ignore-file

const sampleInput = `
`.trim();

const realInput = await Deno.readTextFile("./day04/input.txt");

const parse = (input: string) => {
  const lines = input.split("\n");
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
