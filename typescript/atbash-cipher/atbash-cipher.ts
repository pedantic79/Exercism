/*
 * @prettier
 */
export default class AtbashCipher {
  private alphabet = "abcdefghijklmnopqrstuvwxyz";
  private numbers = "0123456789";

  encode(plainText: string): string {
    return this.atbash(plainText)
      .map((character, index) =>
        index > 0 && index % 5 == 0 ? ` ${character}` : character
      )
      .join("");
  }

  decode(cipherText: string): string {
    return this.atbash(cipherText).join("");
  }

  private atbashCodecCharacter(chararacter: string): string {
    return this.numbers.includes(chararacter)
      ? chararacter
      : this.alphabet[
          this.alphabet.length - (this.alphabet.indexOf(chararacter) + 1)
        ];
  }

  private atbash(input: string): string[] {
    return input
      .toLowerCase()
      .split("")
      .filter(
        (character) =>
          this.alphabet.includes(character) || this.numbers.includes(character)
      )
      .map((character) => this.atbashCodecCharacter(character));
  }
}
