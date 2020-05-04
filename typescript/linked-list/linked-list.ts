/*
 * @prettier
 */

type NodePtr<T> = Node<T> | null;
type NodeIteratorElement<T> = { value?: NodePtr<T>; done?: boolean };
type NodeIterator<T> = { next(): NodeIteratorElement<T> };

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
    for (const _ of this) {
      count += 1;
    }

    return count;
  }

  delete(value: T): void {
    const element = this.find(value);
    if (element) {
      this.deleteNode(element);
    }
  }

  private find(value: T): NodePtr<T> {
    for (const node of this) {
      if (node && value === node.value) {
        return node;
      }
    }

    return null;
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

  [Symbol.iterator](): NodeIterator<T> {
    let cursor = this.head;
    const iterator = {
      next(): NodeIteratorElement<T> {
        if (cursor) {
          const current: NodePtr<T> = cursor;
          cursor = cursor.next;
          return { value: current };
        } else {
          return { done: true };
        }
      },
    };

    return iterator;
  }
}
