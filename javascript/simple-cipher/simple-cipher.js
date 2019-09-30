const SHIFT_RIGHT_PROC = (letter, shift) => letter + shift
const SHIFT_LEFT_PROC = (letter, shift) => letter - shift

export class Cipher {
  constructor(key) {
    this.keyArray = key ? key.split('').map(c => char2val(c)) : randomKey()
  }

  encode(msg) {
    return encode_decode(msg, this.keyArray, SHIFT_RIGHT_PROC)
  }

  decode(msg) {
    return encode_decode(msg, this.keyArray, SHIFT_LEFT_PROC)
  }

  get key() {
    return this.keyArray.map(i => val2char(i)).join('')
  }
}

const A = 'a'.charCodeAt()
const char2val = (character) => character.charCodeAt() - A
const val2char = (number) => String.fromCharCode(number + A)
const eculidian_remainder = (dividend, divisor) => (dividend + divisor) % divisor

const RANDOM_KEY_LENGTH = 100;
const ALPHABET_LENGTH = 26;

const encode_decode = (msg, key, letter_shifter) => {
  return msg.split('').map(
    (character, idx) => {
      const shifted_letter = letter_shifter(char2val(character), key[idx % key.length])
      const within_range = eculidian_remainder(shifted_letter, ALPHABET_LENGTH)
      return val2char(within_range)
    }
  ).join('')
}

const randomKey = () => {
  return Array.from({ length: RANDOM_KEY_LENGTH }, () => Math.floor(Math.random() * ALPHABET_LENGTH))
}
