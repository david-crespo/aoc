import { IntCodeMachine } from "./intCodes";
import { readIntCodes, parseIntCodes } from "./util";

function test(intCodes: number[], inputStack: number[] = []) {
  const machine = new IntCodeMachine(intCodes);
  const outputs = machine.runToNextInput(inputStack);
  console.log(outputs);
}

function part1() {
  test(
    parseIntCodes("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"),
  );
  test(parseIntCodes("1102,34915192,34915192,7,4,7,99,0"));
  test(parseIntCodes("104,1125899906842624,99"));

  const intCodes = readIntCodes("day9-input.txt");
  test(intCodes, [1]);
}

export default function() {
  // part1();

  const intCodes = readIntCodes("day9-input.txt");
  test(intCodes, [2]);
}
