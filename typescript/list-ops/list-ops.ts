/*
 * @prettier
 */
type Functor<T, U> = (accumulator: U, value: T) => U;

export default class List<T> {
  constructor(private list: T[] = []) {}

  get values(): T[] {
    return this.list;
  }

  private clone(): List<T> {
    return new List(this.values);
  }

  append(input: List<T>): List<T> {
    const newList = this.clone();
    newList.list.push(...input.values);
    return newList;
  }

  foldl<U>(operation: Functor<T, U>, initial: U): U {
    let accumulator = initial;

    for (const item of this.list) {
      accumulator = operation(accumulator, item);
    }

    return accumulator;
  }

  concat(listOfLists: List<List<T>>): List<T> {
    return listOfLists.foldl(
      (accumulator: List<T>, value: List<T>): List<T> =>
        accumulator.append(value),
      this.clone()
    );
  }

  filter(predicate: (argument: T) => boolean): List<T> {
    return new List(
      this.foldl((accumulator: T[], value: T): T[] => {
        if (predicate(value)) {
          accumulator.push(value);
        }
        return accumulator;
      }, [])
    );
  }

  map<U>(operation: (argument: T) => U): List<U> {
    return new List(
      this.foldl((accumulator: U[], value: T): U[] => {
        accumulator.push(operation(value));
        return accumulator;
      }, [])
    );
  }

  length(): number {
    return this.foldl((total: number, _: T): number => total + 1, 0);
  }

  foldr<U>(operation: Functor<T, U>, initial: U): U {
    return this.reverse().foldl(operation, initial);
  }

  reverse(): List<T> {
    return new List(
      this.foldl((accumulator: T[], value: T): T[] => {
        accumulator.unshift(value);
        return accumulator;
      }, [])
    );
  }
}
