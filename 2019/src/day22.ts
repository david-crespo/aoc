import { readFile } from "./util";
import { reverse } from "lodash";

const test = `
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
`;

type Deck = number[];

const nDeck = (size: number) => [...Array(size).keys()];

function cut(deck: Deck, n: number): Deck {
  const i = n > 0 ? n : deck.length + n;
  const top = deck.slice(0, i);
  const bottom = deck.slice(i);
  return [...bottom, ...top];
}

function dealWithInc(deck: Deck, inc: number): Deck {
  const newDeck = new Array(deck.length);
  for (let i = 0; i < deck.length; i++) {
    const newIdx = (i * inc) % deck.length;
    newDeck[newIdx] = deck[i];
  }
  return newDeck;
}

function process(deck: Deck, program: Prog): Deck {
  for (let cmd of program) {
    switch (cmd.type) {
      case "new stack":
        deck = reverse(deck);
        break;
      case "cut":
        deck = cut(deck, cmd.n);
        break;
      case "deal inc":
        deck = dealWithInc(deck, cmd.inc);
        break;
    }
  }
  return deck;
}

const wrap = (i: number, size: number) =>
  i < 0 ? i + size : i >= size ? i - size : i;

function cutIdx(i: number, deckSize: number, n: number): number {
  const cutAt = n > 0 ? n : deckSize + n;
  return wrap(i - cutAt, deckSize);
}

function dealNewStackIdx(i: number, deckSize: number): number {
  return deckSize - 1 - i;
}

function dealWithIncIdx(i: number, deckSize: number, inc: number): number {
  return (i * inc) % deckSize;
}

type Cmd =
  | { type: "new stack"; typen: 0 }
  | { type: "cut"; typen: 1; n: number }
  | { type: "deal inc"; typen: 2; inc: number };

type Prog = Cmd[];

function parseProgram(input: string): Prog {
  const lines = input.trim().split("\n");
  const program: Prog = [];
  for (let line of lines) {
    if (line === "deal into new stack") {
      program.push({ type: "new stack", typen: 0 });
    } else if (line.startsWith("cut")) {
      const n = parseInt(line.split(" ")[1], 10);
      program.push({ type: "cut", typen: 1, n });
    } else if (line.startsWith("deal with increment")) {
      const inc = parseInt(line.split(" ")[3], 10);
      program.push({ type: "deal inc", typen: 2, inc });
    }
  }
  return program;
}

function processIdx(i: number, deckSize: number, program: Prog): number {
  for (let cmd of program) {
    switch (cmd.typen) {
      case 0:
        i = dealNewStackIdx(i, deckSize);
        break;
      case 1:
        i = cutIdx(i, deckSize, cmd.n);
        break;
      case 2:
        i = dealWithIncIdx(i, deckSize, cmd.inc);
        break;
    }
  }
  return i;
}

export default function() {
  const program = parseProgram(readFile("day22-input.txt"));
  const result = process(nDeck(10007), program);
  // console.log(result.indexOf(2019));
  // console.log(processIdx(2019, 10007, program));

  const initIdx = 2019;
  let i = initIdx;
  let count = 0;
  do {
    count += 1;
    i = processIdx(i, 119315717514047, program);
    if (count % 1000000 === 0) {
      console.log(count / 1000000, "M");
    }
  } while (i !== initIdx);
  console.log("took n cycles to repeat:", count);
}
