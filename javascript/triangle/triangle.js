export class Triangle {
  constructor(x, y, z) {
    this.sides = [x, y, z].sort((a, b) => a - b);
  }

  kind() {
    if (!this.valid()) {
      throw Error('invalid triangle');
    }

    const uniqueSides = new Set(this.sides);
    if (uniqueSides.size == 1) {
      return 'equilateral'
    } else if (uniqueSides.size == 2) {
      return 'isosceles'
    } else {
      return 'scalene'
    }
  }

  valid() {
    return this.sides.every((side) => side > 0) &&
        this.sides[0] + this.sides[1] > this.sides[2];
  }
}
