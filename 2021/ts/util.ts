export type Pt = [number, number];
export const ptKey = ([x, y]: Pt) => `${x},${y}`;

export const repeat = (value: number, n: number) => new Array(n).fill(value);
export const range = (n: number) => new Array(n).fill(0).map((_, i) => i);
export const zip = <A, B>(as: A[], bs: B[]) =>
  as.map((a, i) => [a, bs[i]] as [A, B]);

// needs URL of calling file to extract dayXX from it
export async function getInput(url: string) {
  const segments = url.split("/");
  const day = segments[segments.length - 1].split(".")[0];
  return await Deno.readTextFile(`./input/${day}.txt`);
}
