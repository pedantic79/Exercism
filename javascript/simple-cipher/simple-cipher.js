
export class Cipher {
  constructor(key) {
    this.keyArray = key ? key.split('').map(c => char2val(c)) : randomKey()
  }

  encode(msg) {
    return encode_decode(msg, this.keyArray, (a, b) => a + b)
  }

  decode(msg) {
    return encode_decode(msg, this.keyArray, (a, b) => a - b)
  }

  get key() {
    return this.keyArray.map(i => val2char(i)).join('')
  }
}

const RANDOM_KEY_LENGTH = 100;
const ALPHABET_LENGTH = 26;
const A = 'a'.charCodeAt()
const char2val = (c) => c.charCodeAt() - A
const val2char = (i) => String.fromCharCode(i + A)


const encode_decode = (msg, key, op) => {
  return msg.split('').map((c, idx) =>
    val2char((ALPHABET_LENGTH + op(char2val(c), key[idx % key.length])) % ALPHABET_LENGTH)).join('')
}

const randomKey = () => {
  return Array.from(new Array(RANDOM_KEY_LENGTH), () => Math.floor(Math.random() * ALPHABET_LENGTH))
}
