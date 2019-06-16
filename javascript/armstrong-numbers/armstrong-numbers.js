export const validate = (number) => {
  const digits = number.toString().split('')
  const len = digits.length;

  const armstrong = digits.reduce((sum, digit) => sum + digit ** len, 0);

  return armstrong === number;
};
