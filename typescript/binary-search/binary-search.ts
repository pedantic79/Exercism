/*
 * @prettier
 */

const FIND_INDEX_NOT_FOUND = -1;
const BINARY_SEARCH_NOT_FOUND = -1;

type Undefinable<T> = T | undefined;

export default class BinarySearch {
  readonly array: Undefinable<number[]> = undefined;

  constructor(inputArray: number[]) {
    if (isSorted(inputArray)) {
      this.array = inputArray;
    }
  }

  indexOf(element: number): number {
    if (this.array === undefined) {
      return BINARY_SEARCH_NOT_FOUND;
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

    return BINARY_SEARCH_NOT_FOUND;
  }
}

function isSorted(array: number[]): boolean {
  return (
    array.findIndex((value, index) => index > 0 && value < array[index - 1]) ===
    FIND_INDEX_NOT_FOUND
  );
}
