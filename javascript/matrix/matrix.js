export class Matrix {
  constructor(input) {
    this.rows = input.split(/\r?\n/).map(row => row.split(' ').map(Number));
    this.columns = this.rows[0].map((_, cIdx) =>
      this.rows.map(row => row[cIdx])
    );
  }
}
