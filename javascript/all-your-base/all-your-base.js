export const convert = (digits, baseIn, baseOut) => {
  const len = digits.length;

  if (baseIn < 2 || !Number.isInteger(baseIn)) {
    throw new Error("Wrong input base");
  }

  if (baseOut < 2 || !Number.isInteger(baseOut)) {
    throw new Error("Wrong output base");
  }

  if (
    len === 0 ||
    (len > 1 && digits[0] === 0) ||
    digits.some((n) => n < 0 || n >= baseIn)
  ) {
    throw new Error("Input has wrong format");
  }

  return fromInt(toInt(digits, baseIn), baseOut);
};

const toInt = (digits, base) =>
  digits.reduce((total, digit) => total * base + digit);

const fromInt = (num, base) => {
  let outputDigits = new Array();

  if (num === 0) {
    return [0];
  }

  while (num > 0) {
    outputDigits.push(num % base);
    num = ~~(num / base);
  }

  return outputDigits.reverse();
};
