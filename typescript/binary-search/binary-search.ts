export default class BinarySearch {
  readonly array: number[] | undefined = undefined;

  constructor(inputArray: number[]) {
    if (isSorted(inputArray)) {
      this.array = inputArray;
    }
  }

  indexOf(element: number): number {
    if (this.array === undefined) {
      return -1;
    }

    let leftIndex = 0;
    let rightIndex = this.array.length;

    while (leftIndex <= rightIndex) {
      const midpoint = Math.trunc((leftIndex + rightIndex) / 2);

      if (element === this.array[midpoint]) {
        return midpoint;
      }

      if (element > this.array[midpoint]) {
        leftIndex = midpoint + 1;
      } else {
        rightIndex = midpoint - 1;
      }
    }

    return -1;
  }
}

function isSorted(possiblySortedArray: number[]): boolean {
  return (
    possiblySortedArray.findIndex(
      (value, index) => index > 0 && value < possiblySortedArray[index - 1]
    ) === -1
  );
}
