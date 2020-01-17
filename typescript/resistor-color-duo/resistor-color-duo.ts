export class ResistorColor {
  private static readonly COLORS = [
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

  private static readonly MAX_COLOR_COUNT = 2;

  private colors: string[];

  constructor(colors: string[]) {
    if (colors.length < ResistorColor.MAX_COLOR_COUNT) {
      throw new Error("At least two colors need to be present");
    }
    this.colors = colors.slice(0, ResistorColor.MAX_COLOR_COUNT);
  }

  private getResistorValue(resistorColor: string): number {
    return ResistorColor.COLORS.findIndex(
      currentColor => currentColor === resistorColor
    );
  }

  private buildTotalResistorValue(
    totalResistorValue: number,
    currentResistorValue: number
  ): number {
    return totalResistorValue * 10 + currentResistorValue;
  }

  value = (): number =>
    this.colors.reduce((totalResistorValue, currentResistorColor) => {
      const currentResistorValue = this.getResistorValue(currentResistorColor);

      return this.buildTotalResistorValue(
        totalResistorValue,
        currentResistorValue
      );
    }, 0);
}
