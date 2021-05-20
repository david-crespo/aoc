import { readFile } from "./util";

const getMasses = () =>
  readFile("day1-input.txt")
    .split("\n")
    .filter(x => x.trim())
    .map(s => parseInt(s, 10));

const calcFuel1 = (m: number) => Math.floor(m / 3) - 2;

const calcFuel2 = (m: number) => {
  let fuel = calcFuel1(m);
  fuel = fuel < 0 ? 0 : fuel;
  if (fuel > 0) {
    fuel += calcFuel2(fuel);
  }
  return fuel;
};

export default function() {
  return [
    getMasses()
      .map(calcFuel1)
      .reduce((a, b) => a + b),
    getMasses()
      .map(calcFuel2)
      .reduce((a, b) => a + b),
  ];
}
