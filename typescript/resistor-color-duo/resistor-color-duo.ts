const COLORS = [
  "black",
  "brown",
  "red",
  "orange",
  "yellow",
  "green",
  "blue",
  "violet",
  "grey",
  "white"
];

export class ResistorColor {
  private colors: string[];

  constructor(colors: string[]) {
    if (colors.length < 2) {
      throw new Error("At least two colors need to be present");
    }
    this.colors = colors;
  }

  value = (): number =>
    this.colors
      .slice(0, 2)
      .map(color => COLORS.findIndex(c => c === color))
      .reduce((currentNumber, newNumber) => currentNumber * 10 + newNumber);
}
