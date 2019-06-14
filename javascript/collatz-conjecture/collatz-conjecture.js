export const steps = (collatz) => {
  if (collatz < 1) {
    throw new Error('Only positive numbers are allowed');
  }

  let steps = 0;
  while (collatz != 1) {
    steps++;
    collatz = collatz % 2 == 0 ? collatz / 2 : collatz * 3 + 1;
  }
  return steps;
};
