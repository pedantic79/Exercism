class Squares {
  constructor(private readonly number: number) { }

  public get squareOfSum(): number {
    return (this.number ** 4 + 2 * this.number ** 3 + this.number ** 2) / 4
  }

  public get sumOfSquares(): number {
    return (this.number + 3 * this.number ** 2 + 2 * this.number ** 3) / 6
  }

  public get difference(): number {
    return this.squareOfSum - this.sumOfSquares
  }
}

export default Squares
