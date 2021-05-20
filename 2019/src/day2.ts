import { readFile } from "./util";
import { runIntCodes } from "./intCodes";

export default function() {
  const intCodes = readFile("day2-input.txt")
    .trim()
    .split(",")
    .map(s => parseInt(s, 10));

  // console.log(runIntCodes([1,9,10,3,2,3,11,0,99,30,40,50]));
  // console.log(runIntCodes([1,0,0,0,99]));
  // console.log(runIntCodes([2,3,0,3,99]));
  // console.log(runIntCodes([2,4,4,5,99,0]));
  // console.log(runIntCodes([1,1,1,4,99,5,6,0,99]));

  // intCodes[1] = 12;
  // intCodes[2] = 2;

  for (let i = 0; i <= 99; i++) {
    for (let j = 0; j <= 99; j++) {
      const tmpIntCodes = [...intCodes];
      tmpIntCodes[1] = i;
      tmpIntCodes[2] = j;
      if (runIntCodes(tmpIntCodes) === 19690720) {
        console.log("noun", i, "verb", j, "answer", 100 * i + j);
      }
    }
  }

  return null;
}
