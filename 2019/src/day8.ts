import { readFile } from "./util";

const imgStrToInts = (input: string) =>
  Array.from(input).map(s => parseInt(s, 10));

function parseImage(pixels: number[], width: number, height: number) {
  const layerSize = width * height;

  const layers = [];
  for (let i = 0; i < pixels.length; i += layerSize) {
    layers.push(pixels.slice(i, i + layerSize));
  }

  return { layers };
}

function flatten(layers: number[][]) {
  const len = layers[0].length;
  const output = [];
  for (let i = 0; i < len; i++) {
    output[i] = layers.map(l => l[i]).find(n => n !== 2) || 2;
  }
  return output;
}

const numToColor = (n: number) => {
  if (n === 0) {
    return " ";
  } else if (n === 1) {
    return "█";
  } else if (n === 2) {
    return "░";
  } else {
    return "╳";
  }
};

function printLayer(layer: number[], width: number, height: number) {
  for (let rowIdx = 0; rowIdx < height; rowIdx++) {
    const row = layer
      .slice(rowIdx * width, (rowIdx + 1) * width)
      .map(numToColor)
      .join("");
    console.log(row);
  }
}

export default function() {
  const input = imgStrToInts(readFile("day8-input.txt").trim());

  const { layers } = parseImage(input, 25, 6);

  // const countNum = (ns: number[], target: number) =>
  //   ns.filter(n => n === target).length;
  //
  // layers.sort((a, b) => countNum(a, 0) - countNum(b, 0));
  // console.log(countNum(layers[0], 1) * countNum(layers[0], 2));

  printLayer(flatten(layers), 25, 6);
}
