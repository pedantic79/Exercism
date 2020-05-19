/*
 * @prettier
 */
enum SimpleSipherMode {
  ENCODE,
  DECODE,
}

export default class SimpleCipher {
  public readonly keyArray: number[];
  readonly A_VALUE = "a".charCodeAt(0);
  readonly ALPHABET_LENGTH = this.character2Value("z") + 1;
  readonly RANDOM_KEY_LENGTH = 100;

  public get key(): string {
    return this.keyArray
      .map((character) => this.value2Character(character))
      .join("");
  }

  constructor(key: string = "") {
    this.keyArray = key
      ? key.split("").map((c) => this.character2Value(c))
      : this.randomKey();
  }

  encode(plainText: string): string {
    return this.codec(plainText, SimpleSipherMode.ENCODE);
  }

  decode(cipherText: string): string {
    return this.codec(cipherText, SimpleSipherMode.DECODE);
  }

  private character2Value(character: string): number {
    return character.charCodeAt(0) - this.A_VALUE;
  }

  private value2Character(value: number): string {
    const normalized = (value + this.ALPHABET_LENGTH) % this.ALPHABET_LENGTH;
    return String.fromCharCode(normalized + this.A_VALUE);
  }

  private randomKey(): number[] {
    return Array.from({ length: this.RANDOM_KEY_LENGTH }, () =>
      Math.floor(Math.random() * this.ALPHABET_LENGTH)
    );
  }

  private codec(msg: string, mode: SimpleSipherMode): string {
    return msg.replace(/./g, (character, idx) => {
      let offset = this.keyArray[idx % this.keyArray.length];

      if (mode == SimpleSipherMode.DECODE) {
        offset *= -1;
      }

      const shiftedLetter = this.character2Value(character) + offset;

      return this.value2Character(shiftedLetter);
    });
  }
}
