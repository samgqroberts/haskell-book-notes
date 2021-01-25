const x: readonly [number, string] = [1, "hi"]

function fn(input: unknown): string {
  if (typeof input === 'string') {
    const input2 = input.localeCompare
  }
  return input as string;
}

const z = fn();

x[1] = 1;

const y: [
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  string,
  number,
  number,
  number,
  number,
  number,
] = [];