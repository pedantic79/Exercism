/*
 * @prettier
 */
export default class AtbashCipher {
  private readonly ALPHABET = "abcdefghijklmnopqrstuvwxyz";
  private readonly NUMERIC = /[0-9]/;
  private readonly NOT_LOWERCASE_ALPHANUMERIC = /[^a-z0-9]/g;
  private readonly EVERY_CHARACTER = /./g;
  private readonly ENCODING_CADENCE = 5;

  encode(plainText: string): string {
    return this.intersperseSpaces(
      this.atbash(plainText),
      this.ENCODING_CADENCE
    ).join(" ");
  }

  decode(cipherText: string): string {
    return this.atbash(cipherText);
  }

  private intersperseSpaces(input: string, cadence: number): string[] {
    const everyNthCharacter = new RegExp(`.{1,${cadence}}`, "g");
    return input.match(everyNthCharacter) || [];
  }

  private atbashCodecCharacter(chararacter: string): string {
    if (chararacter.match(this.NUMERIC)) {
      return chararacter;
    }

    return this.ALPHABET[
      this.ALPHABET.length - (this.ALPHABET.indexOf(chararacter) + 1)
    ];
  }

  private atbash(input: string): string {
    return input
      .toLowerCase()
      .replace(this.NOT_LOWERCASE_ALPHANUMERIC, "")
      .replace(this.EVERY_CHARACTER, (character) =>
        this.atbashCodecCharacter(character)
      );
  }
}
