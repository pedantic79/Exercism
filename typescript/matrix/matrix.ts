export default class Matrix {
  public readonly rows: number[][];
  public readonly columns: number[][];

  constructor(inputString: string) {
    this.rows = inputString
      .split("\n")
      .map((row) => row.split(" ").map(Number));
    this.columns = transpose(this.rows);
  }
}

function transpose<T>(rows: Readonly<T[][]>): T[][] {
  return rows[0].map((_, columnIndex) => rows.map((row) => row[columnIndex]));
}
