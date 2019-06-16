export const toRoman = (arabicNumber) => {
  let num = arabicNumber;
  let roman = '';

  for (const divisor of Object.keys(ROMAN_NUMERALS).sort((a, b) => b - a)) {
    while (parseInt(num / divisor) > 0) {
      roman += ROMAN_NUMERALS[divisor];
      num -= divisor;
    }
  }

  return roman;
};

const ROMAN_NUMERALS = {
  1000: 'M',
  900: 'CM',
  500: 'D',
  400: 'CD',
  100: 'C',
  90: 'XC',
  50: 'L',
  40: 'XL',
  10: 'X',
  9: 'IX',
  5: 'V',
  4: 'IV',
  1: 'I'
};
