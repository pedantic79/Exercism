const ASCII_A: number = "A".charCodeAt(0);
const ASCII_Z: number = "Z".charCodeAt(0);

const ASCII_0: number = "0".charCodeAt(0);
const ASCII_9: number = "9".charCodeAt(0);

const ENGLISH_ALPHABET = { start: ASCII_A, end: ASCII_Z };
const ARABIC_NUMERALS = { start: ASCII_0, end: ASCII_9 };

class RobotFactory {
  name_cache: Set<string> = new Set();

  generateName(): string {
    let name = this.randomName();
    while (this.name_cache.has(name)) {
      name = this.randomName();
    }

    this.name_cache.add(name);
    return name;
  }

  randomCharacter(characterSet: { start: number; end: number }): string {
    const size = characterSet.end - characterSet.start + 1;
    return String.fromCharCode(
      characterSet.start + Math.floor(Math.random() * size)
    );
  }

  randomLetter(): string {
    return this.randomCharacter(ENGLISH_ALPHABET);
  }

  randomNumber(): string {
    return this.randomCharacter(ARABIC_NUMERALS);
  }

  randomName(): string {
    return (
      this.randomLetter() +
      this.randomLetter() +
      this.randomNumber() +
      this.randomNumber() +
      this.randomNumber()
    );
  }
}

const FACTORY = new RobotFactory();

export default class RobotName {
  private _name: string;

  constructor() {
    this._name = FACTORY.generateName();
  }

  get name(): string {
    return this._name;
  }

  resetName(): void {
    this._name = FACTORY.generateName();
  }
}
