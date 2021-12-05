import { getInput } from "./util.ts";

const sampleInput = `
forward 5
down 5
forward 8
up 3
down 8
forward 2
`.trim();

const realInput = await getInput(import.meta.url);

type Cmd = [string, number];

const parse = (input: string) =>
  input.split("\n").map((line) => {
    const [dir, n] = line.split(" ");
    return [dir, parseInt(n, 10)] as Cmd;
  });

function run(input: string) {
  const cmds = parse(input);
  let x = 0;
  let y = 0;
  cmds.forEach(([dir, n]) => {
    switch (dir) {
      case "forward":
        x += n;
        break;
      case "up":
        y -= n;
        break;
      case "down":
        y += n;
        break;
    }
  });
  console.log(x * y);
}

function run2(input: string) {
  const cmds = parse(input);
  let x = 0;
  let y = 0;
  let aim = 0;
  cmds.forEach(([dir, n]) => {
    switch (dir) {
      case "forward":
        x += n;
        y += aim * n;
        break;
      case "up":
        aim -= n;
        break;
      case "down":
        aim += n;
        break;
    }
  });
  console.log(x * y);
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
