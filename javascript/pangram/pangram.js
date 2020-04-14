const VALID_CHARS = /[a-z]/g;
const SIZE = 26;

export const isPangram = (input) =>
  new Set(input.toLowerCase().match(VALID_CHARS)).size === SIZE;
