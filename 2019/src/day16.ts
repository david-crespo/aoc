import { readFile } from "./util";

const basePattern = [0, 1, 0, -1];

function getMaskDigit(mult: number, i: number) {
  const idx = Math.floor((i % (mult * basePattern.length)) / mult);
  return basePattern[idx];
}

function sumMult(input: number[], inputIdx: number) {
  let sum = 0;
  for (let i = 0; i < input.length; i++) {
    const mask = getMaskDigit(inputIdx + 1, i + 1);
    // console.log(input[i], mask);
    if (mask) {
      sum += input[i] * mask;
    }
  }
  return sum;
}

function runPhase(input: number[]) {
  const out = [];
  for (var i = 0; i < input.length; i++) {
    out.push(Math.abs(sumMult(input, i) % 10));
  }
  return out;
}

const parseInput = (input: string) =>
  Array.from(input).map(s => parseInt(s, 10));

function test() {
  let input = parseInput("12345678");
  const outputs = ["48226158", "34040438", "03415518", "01029498"];
  for (let i = 0; i < 4; i++) {
    input = runPhase(input);
    const out = input.slice(0, 8).join("");
    if (out !== outputs[i]) {
      return false;
    }
  }

  return true;
}

export default function() {
  const realInput = parseInput(readFile("day16-input.txt").trim());
  const testInput2 = parseInput("80871224585914546619083218645595");

  if (!test()) {
    console.log("TEST FAILED");
    return;
  }

  let input = realInput;
  // let input = repeat(realInput, realInput.length * 10);

  for (let i = 0; i < 100; i++) {
    input = runPhase(input);
    console.log(input.slice(0, 8).join(""));
  }
}
