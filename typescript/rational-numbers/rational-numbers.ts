/*
 * @prettier
 */

export default class Rational {
  constructor(private numerator: number, private denominator: number) {
    this.reduce();
  }

  reduce(): Rational {
    const gcd = this.greatestCommonDivisor(this.numerator, this.denominator);

    this.numerator /= gcd;
    this.denominator /= gcd;

    if (this.denominator < 0) {
      this.denominator *= -1;
      this.numerator *= -1;
    }

    return this;
  }

  add({ numerator, denominator }: Rational): Rational {
    return new Rational(
      numerator * this.denominator + this.numerator * denominator,
      denominator * this.denominator
    );
  }

  sub({ numerator, denominator }: Rational): Rational {
    return this.add(new Rational(numerator * -1, denominator));
  }

  mul({ numerator, denominator }: Rational): Rational {
    return new Rational(
      this.numerator * numerator,
      this.denominator * denominator
    );
  }

  div({ numerator, denominator }: Rational): Rational {
    return this.mul(new Rational(denominator, numerator));
  }

  abs(): Rational {
    return new Rational(Math.abs(this.numerator), Math.abs(this.denominator));
  }

  exprational(n: number): Rational {
    return new Rational(
      Math.pow(this.numerator, n),
      Math.pow(this.denominator, n)
    );
  }

  expreal(base: number): number {
    return Number(
      Math.pow(base, this.numerator / this.denominator).toPrecision(15)
    );
  }

  private greatestCommonDivisor(a: number, b: number): number {
    // Non-recursive version of Euclid's algorithm for finding greatest common divisor
    // gcd(a, 0) = a
    // gcd(a, b) = gcd(b, a mod b)
    while (b !== 0) {
      [a, b] = [b, a % b];
    }

    return a;
  }
}
