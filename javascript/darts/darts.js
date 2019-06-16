export const solve = (x, y) => {
  // Don't square-root because it's expensive, since we're only checking for
  // distances, we can just simply square the value of what we're checking
  // against i.e. x^2 + y^2 > z^2
  const distSquared = x * x + y * y;

  if (distSquared > 100) {
    return 0;
  } else if (distSquared > 25) {
    return 1;
  } else if (distSquared > 1) {
    return 5;
  } else {
    return 10;
  }
};
