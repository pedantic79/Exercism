const ASCII_A: number = "A".charCodeAt(0);
const ASCII_0: number = "0".charCodeAt(0);

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

  randomCharacter(start: number, size: number): string {
    return String.fromCharCode(start + Math.floor(Math.random() * size));
  }

  randomLetter(): string {
    return this.randomCharacter(ASCII_A, 26);
  }

  randomNumber(): string {
    return this.randomCharacter(ASCII_0, 10);
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
