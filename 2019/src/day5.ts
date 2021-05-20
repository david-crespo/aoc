import { readFile } from "./util";
import { runIntCodes } from "./intCodes";

export default function() {
  const intCodes = readFile("day5-input.txt")
    .trim()
    .split(",")
    .map(s => parseInt(s, 10));

  runIntCodes(intCodes);
}
