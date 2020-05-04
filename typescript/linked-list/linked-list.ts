/*
 * @prettier
 */

type NodePtr<T> = Node<T> | null;
type IterateCallBack<T> = (node: Node<T>) => void;

class IterateEarlyReturn extends Error {
  constructor() {
    super("Iterate Early Return");
    this.name = "IterateEarlyReturn";
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
    return this.deleteNode(this.head);
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
    return this.deleteNode(this.last);
  }

  count(): number {
    let count = 0;

    this.iterate(() => {
      count += 1;
    });

    return count;
  }

  delete(value: T): void {
    this.iterate((node: Node<T>) => {
      if (value === node.value) {
        this.deleteNode(node);
        throw new IterateEarlyReturn();
      }
    });
  }

  private deleteNode(node: NodePtr<T>): T {
    if (!node) {
      throw new Error("empty list");
    }

    const { value, prev, next } = node;

    const left = prev;
    const right = next;

    if (left) {
      left.next = right;
    } else {
      this.head = this.head && this.head.next;
    }

    if (right) {
      right.prev = left;
    } else {
      this.last = this.last && this.last.prev;
    }

    return value;
  }

  private iterate(callback: IterateCallBack<T>): void {
    for (let cursor = this.head; cursor; cursor = cursor.next) {
      try {
        callback(cursor);
      } catch (e) {
        if (e instanceof IterateEarlyReturn) {
          break;
        } else {
          throw e;
        }
      }
    }
  }
}
