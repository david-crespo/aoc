import { getInput } from "./util.ts";

const sampleInput = `
3,4,3,1,2
`.trim();

const realInput = await getInput(import.meta.url);

const parse = (input: string) => input.split(",").map((n) => parseInt(n, 10));

function run(input: string) {
  let list = parse(input);
  for (let i = 0; i < 80; i++) {
    const newList: number[] = [];
    const end: number[] = [];

    list.forEach((n) => {
      if (n > 0) {
        newList.push(n - 1);
      } else {
        newList.push(6);
        end.push(8);
      }
    });
    list = [...newList, ...end];
  }
  console.log(list.length);
}

function run2(input: string) {
  const list = parse(input);
  let slots = new Array(9).fill(0);
  list.forEach((n) => {
    slots[n] += 1;
  });

  for (let i = 0; i < 256; i++) {
    const newSlots = new Array(9).fill(0);
    newSlots[8] = slots[0];
    newSlots[6] = slots[0];
    for (let i = 1; i <= 8; i++) {
      newSlots[i - 1] += slots[i];
    }

    slots = newSlots;
  }
  console.log(slots.reduce((a, b) => a + b));
}

console.log("=======================");
run(sampleInput);
run(realInput);

console.log();
run2(sampleInput);
run2(realInput);
