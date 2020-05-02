/**
 * @prettier
 */

export const compute = (left, right) => {
  if (left.length !== right.length) {
    if (left.length === 0) throw new Error("left strand must not be empty");
    if (right.length === 0) throw new Error("right strand must not be empty");

    throw new Error("left and right strands must be of equal length");
  }

  return left.split("").reduce((count, leftCharacter, index) => {
    return leftCharacter != right[index] ? count + 1 : count;
  }, 0);
};
