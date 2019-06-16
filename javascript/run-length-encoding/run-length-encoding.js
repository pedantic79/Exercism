export const encode = (plain) => {
  const chars = plain.split('');
  let encoding = '';

  let last = '';
  let count = 0;
  for (const c of chars) {
    if (last === c) {
      count++;
    } else {
      encoding += (count > 1) ? count + last : last;

      last = c;
      count = 1;
    }
  }
  encoding += (count > 1) ? count + last : last;

  return encoding;
};

export const decode = (encoding) => {
  const chars = encoding.split('');
  let plain = '';

  let number = '';
  for (const c of chars) {
    if (parseInt(c)) {
      number += c;
    } else {
      plain += (number === '') ? c : c.repeat(number);
      number = '';
    }
  }

  return plain;
};
