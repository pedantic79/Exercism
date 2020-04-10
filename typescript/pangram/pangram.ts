const LOWERCASE_ALPHABETIC_REGEX = /[a-z]/g;
const ALPHABET_SIZE = 26;

export default class Pangram {
  constructor(private phrase: Readonly<string>) {}

  isPangram(): boolean {
    const lowerCasedPhrase = this.phrase.toLowerCase();
    return (
      new Set(lowerCasedPhrase.match(LOWERCASE_ALPHABETIC_REGEX)).size ==
      ALPHABET_SIZE
    );
  }
}
