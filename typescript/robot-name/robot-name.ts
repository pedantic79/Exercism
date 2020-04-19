export default class RobotName {
  private _name: string;
  private static NAME_CACHE: Set<string> = new Set();

  constructor() {
    this._name = RobotName.generateName();
  }

  get name(): string {
    return this._name;
  }

  resetName(): void {
    this._name = RobotName.generateName();
  }

  private static generateName(): string {
    let name = randomName();
    while (RobotName.NAME_CACHE.has(name)) {
      name = randomName();
    }

    RobotName.NAME_CACHE.add(name);
    return name;
  }
}

const ASCII_A: number = "A".charCodeAt(0);
const ASCII_0: number = "0".charCodeAt(0);

function randomCharacter(start: number, size: number): string {
  return String.fromCharCode(start + Math.floor(Math.random() * size));
}

function randomLetter(): string {
  return randomCharacter(ASCII_A, 26);
}

function randomNumber(): string {
  return randomCharacter(ASCII_0, 10);
}

function randomName(): string {
  return (
    randomLetter() +
    randomLetter() +
    randomNumber() +
    randomNumber() +
    randomNumber()
  );
}
