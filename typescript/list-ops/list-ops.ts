/*
 * @prettier
 */
/* eslint @typescript-eslint/no-use-before-define: "off" */
export default class List<T> {
  private head: NullableLinkedList<T>;

  constructor(list: T[] = []) {
    this.head = list.reduceRight<NullableLinkedList<T>>(LinkedList.cons, null);
  }

  // Turns a NullableLinkedList to a List
  private static wrap<T>(linkedList: NullableLinkedList<T>): List<T> {
    const list = new List<T>();
    list.head = linkedList;
    return list;
  }

  get values(): T[] {
    type IndexableArray = [T[], number];

    const [outputArray, _] = this.foldl(
      ([output, index]: IndexableArray, value: T): IndexableArray => {
        output[index] = value;
        return [output, index + 1];
      },
      [new Array(this.length()), 0]
    );

    return outputArray;
  }

  append(input: List<T>): List<T> {
    const nullableLinkedList = LinkedList.append(this.head, input.head);

    return List.wrap(nullableLinkedList);
  }

  concat(list: List<List<T>>): List<T> {
    const nullableLinkedList = LinkedList.append(
      this.head,
      list.foldr(
        (accumulator: NullableLinkedList<T>, value: List<T>) =>
          LinkedList.append(value.head, accumulator),
        null
      )
    );

    return List.wrap(nullableLinkedList);
  }

  filter(predicate: (argument: T) => boolean): List<T> {
    const nullableLinkedList = this.foldr(
      (accumulator: NullableLinkedList<T>, value: T) =>
        predicate(value) ? LinkedList.cons(accumulator, value) : accumulator,
      null
    );

    return List.wrap(nullableLinkedList);
  }

  map<U>(operation: (argument: T) => U): List<U> {
    const nullableLinkedList = this.foldr(
      (accumulator: NullableLinkedList<U>, value: T) =>
        LinkedList.cons(accumulator, operation(value)),
      null
    );

    return List.wrap(nullableLinkedList);
  }

  length(): number {
    return this.foldr((accumulator: number) => accumulator + 1, 0);
  }

  foldl<U>(operation: Functor<T, U>, initial: U): U {
    return LinkedList.foldl(operation, initial, this.head);
  }

  foldr<U>(operation: Functor<T, U>, initial: U): U {
    return LinkedList.foldr(operation, initial, this.head);
  }

  reverse(): List<T> {
    const nullableLinkedList = this.foldl<NullableLinkedList<T>>(
      LinkedList.cons,
      null
    );

    return List.wrap(nullableLinkedList);
  }
}

type Functor<T, U> = (accumulator: U, value: T) => U;
type NullableLinkedList<T> = LinkedList<T> | null;

// Simple Functional style linked list
class LinkedList<T> {
  private value: T;
  private next: NullableLinkedList<T>;

  constructor({ value, next }: { value: T; next: NullableLinkedList<T> }) {
    this.value = value;
    this.next = next;
  }

  // https://en.wikipedia.org/wiki/Cons
  static cons<T>(list: NullableLinkedList<T>, value: T): NullableLinkedList<T> {
    return new LinkedList({ value: value, next: list });
  }

  static foldr<T, U>(
    operation: Functor<T, U>,
    initial: U,
    list: NullableLinkedList<T>
  ): U {
    if (!list) {
      return initial;
    }

    return operation(
      LinkedList.foldr(operation, initial, list.next),
      list.value
    );
  }

  static foldl<T, U>(
    operation: Functor<T, U>,
    initial: U,
    list: NullableLinkedList<T>
  ): U {
    if (!list) {
      return initial;
    }

    return LinkedList.foldl(
      operation,
      operation(initial, list.value),
      list.next
    );
  }

  static append<T>(
    base: NullableLinkedList<T>,
    list: NullableLinkedList<T>
  ): NullableLinkedList<T> {
    return LinkedList.foldr(LinkedList.cons, list, base);
  }
}
