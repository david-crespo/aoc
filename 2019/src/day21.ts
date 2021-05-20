import { readIntCodes } from "./util";
import { IntCodeMachine } from "./intCodes";

type Prog = string[];

const walkProgram = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
];

const runProgram = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
  "AND E T",
  "OR H T",
  "AND T J",
];

function go(intCodes: number[], program: Prog, walk = true) {
  const machine = new IntCodeMachine(intCodes);

  console.log(program.join("\n") + "\n");

  machine.runToNextInput();
  const ascii = [...program, walk ? "WALK" : "RUN"].join("\n") + "\n";
  const output = machine.runAscii(ascii);

  if (typeof output === "string") {
    console.log(output);
  } else {
    console.log("hull damage:", output[output.length - 1]);
  }
}

export default function() {
  const intCodes = readIntCodes("day21-input.txt");
  go(intCodes, walkProgram);
  go(intCodes, runProgram, false);
}
