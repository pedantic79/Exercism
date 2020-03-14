class Squares {
  private readonly num: number

  constructor(num: number) { this.num = num }

  public get squareOfSum(): number {
    return (this.num ** 4 + 2 * this.num ** 3 + this.num ** 2) / 4
  }

  public get sumOfSquares(): number {
    return (this.num + 3 * this.num ** 2 + 2 * this.num ** 3) / 6
  }

  public get difference(): number {
    return this.squareOfSum - this.sumOfSquares
  }
}

export default Squares
