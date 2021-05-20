import { groupBy } from "lodash";

const monotonic = (pw: string) =>
  pw ===
  Array.from(pw)
    .sort()
    .join("");

const hasConsec1 = (pw: string) =>
  Object.values(groupBy(pw)).some(g => g.length >= 2);

const hasConsec2 = (pw: string) =>
  Object.values(groupBy(pw)).some(g => g.length === 2);

const isValid = (pw: string) => monotonic(pw) && hasConsec2(pw);

export default function() {
  let count = 0;
  for (let i = 359282; i <= 820401; i++) {
    if (isValid(i.toString())) {
      count += 1;
    }
  }
  console.log(count);
}
