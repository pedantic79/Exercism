export const convert = (digits, baseIn, baseOut) => {
  const len = digits.length;

  if (baseIn === undefined || baseIn < 2 || Math.floor(baseIn) != baseIn) {
    throw new Error('Wrong input base');
  }

  if (baseOut === undefined || baseOut < 2 || Math.floor(baseOut) != baseOut) {
    throw new Error('Wrong output base');
  }

  if (digits === undefined || len == 0 ||
    digits.some((n) => n < 0 || n >= baseIn ||
      (len > 1 && digits[0] == 0))) {
    throw new Error('Input has wrong format');
  }

  return fromBase10(toBase10(digits, baseIn), baseOut);
};

const toBase10 = (digits, base) => {
  const len = digits.length;

  let num = 0;

  for (let power = 0; power < len; power++) {
    num += digits[len - power - 1] * (base ** power);
  }

  return num;
}

const fromBase10 = (num, base) => {
  let outputDigits = new Array();

  if (num == 0) {
    return [0];
  }

  while (num > 0) {
    outputDigits.push(num % base);
    num = ~~(num / base);
  }

  return outputDigits.reverse();
}
