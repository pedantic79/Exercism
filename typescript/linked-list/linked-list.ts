/*
 * @prettier
 */

type NodePtr<T> = Node<T> | null;
type IterateCallBack<T> = (
  value: T,
  prev: NodePtr<T>,
  next: NodePtr<T>
) => void;

class IteratorEarlyReturn extends Error {
  constructor() {
    super("Iterator Early Return");
    this.name = "IteratorEarlyReturn";
  }
}

class Node<T> {
  public value: T;
  public prev: NodePtr<T>;
  public next: NodePtr<T>;

  constructor({
    value,
    prev,
    next,
  }: {
    value: T;
    prev?: NodePtr<T>;
    next?: NodePtr<T>;
  }) {
    this.value = value;
    this.prev = prev || null;
    this.next = next || null;
  }
}

export default class LinkedList<T> {
  private head: NodePtr<T>;
  private last: NodePtr<T>;

  constructor() {
    this.head = null;
    this.last = null;
  }

  push(value: T): void {
    const node = new Node({ value, prev: this.last });

    if (this.last) {
      this.last.next = node;
    } else {
      this.head = node;
    }

    this.last = node;
  }

  shift(): T {
    if (!this.head) {
      throw new Error("empty list");
    }

    const { value } = this.head;
    if (this.head.next) {
      this.head = this.head.next;
      this.head.prev = null;
    } else {
      this.last = this.head = null;
    }
    return value;
  }

  unshift(value: T): void {
    const node = new Node({ value, next: this.head });
    if (this.head) {
      this.head.prev = node;
    } else {
      this.last = node;
    }

    this.head = node;
  }

  pop(): T {
    if (!this.last) {
      throw new Error("empty list");
    }

    const { value } = this.last;
    if (this.last.prev) {
      this.last = this.last.prev;
      this.last.next = null;
    } else {
      this.last = this.head = null;
    }

    return value;
  }

  count(): number {
    let count = 0;

    this.iterate(() => {
      count += 1;
    });

    return count;
  }

  delete(value: T): void {
    this.iterate(
      (cursorValue: T, cursorPrev: NodePtr<T>, cursorNext: NodePtr<T>) => {
        if (cursorValue === value) {
          const left = cursorPrev;
          const rite = cursorNext;

          if (left) {
            left.next = rite;
          } else {
            this.head = this.head && this.head.next;
          }

          if (rite) {
            rite.prev = left;
          } else {
            this.last = this.last && this.last.prev;
          }

          throw new IteratorEarlyReturn();
        }
      }
    );
  }

  private iterate(callback: IterateCallBack<T>): void {
    for (let cursor = this.head; cursor; cursor = cursor.next) {
      try {
        callback(cursor.value, cursor.prev, cursor.next);
      } catch (EarlyExit) {
        break;
      }
    }
  }
}
