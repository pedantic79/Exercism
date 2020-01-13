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

  private static readonly MAX_RESISTOR_COUNT: number = 2;

  private colors: string[];

  constructor(colors: string[]) {
    if (colors.length < ResistorColor.MAX_RESISTOR_COUNT) {
      throw new Error("At least two colors need to be present");
    }
    this.colors = colors.slice(0, ResistorColor.MAX_RESISTOR_COUNT);
  }

  private getResistorValue(resistorColor: string): number {
    return ResistorColor.COLORS.findIndex(
      currentColor => currentColor === resistorColor
    );
  }

  private buildTotalResitorValue(
    totalRestistorValue: number,
    currentResistorValue: number
  ): number {
    return totalRestistorValue * 10 + currentResistorValue;
  }

  value = (): number =>
    this.colors.reduce((totalRestistorValue, currentResistorColor) => {
      const currentResistorValue = this.getResistorValue(currentResistorColor);

      return this.buildTotalResitorValue(
        totalRestistorValue,
        currentResistorValue
      );
    }, 0);
}
