export class Triangle {
  constructor(count) {
    this.lastRow = [1]
    this.rows = [[1]]

    for (let r = 1; r < count; r++) {
      this.lastRow = nextRow(this.rows[this.rows.length - 1])
      this.rows.push(this.lastRow)
    }
  }
}

const nextRow = (last) => {
  let row = [1]
  for (let w = 0; w + 1 < last.length; w++) {
    row.push(last[w] + last[w + 1])
  }
  row.push(1)
  return row
}
