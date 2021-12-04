const sampleInput = `
199
200
208
210
200
207
240
269
260
263
`.trim();

const realInput = await Deno.readTextFile("./day01/input.txt");

function count(input: string): number {
  const ns = input.split("\n").map((n) => parseInt(n, 10));
  let c = 0;
  for (let i = 1; i < ns.length; i++) {
    if (ns[i] > ns[i - 1]) {
      c++;
    }
  }
  return c;
}

console.log("=======================");
console.log(count(sampleInput));
console.log(count(realInput));

function count2(input: string): number {
  const ns = input.split("\n").map((n) => parseInt(n, 10));
  let c = 0;
  for (let i = 1; i < ns.length - 2; i++) {
    if (
      /* ns[i] + ns[i + 1] + */ ns[i + 2] > ns[i - 1] /* + ns[i] + ns[i + 1] */
    ) {
      c++;
    }
  }
  return c;
}

console.log();
console.log(count2(sampleInput));
console.log(count2(realInput));
