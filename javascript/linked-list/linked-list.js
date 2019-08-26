class Node {
  constructor({ value, prev, next }) {
    this.value = value
    this.prev = prev
    this.next = next
  }
}


export class LinkedList {
  constructor() {
    this.head = null
    this.last = null
  }

  push(value) {
    const node = new Node({ value, prev: this.last })

    if (this.last) {
      this.last.next = node
    } else {
      this.head = node
    }

    this.last = node
  }

  pop() {
    if (!this.last) {
      throw new Error("empty list");
    }

    const { value } = this.last
    if (this.last.prev) {
      this.last = this.last.prev
      this.last.next = null
    } else {
      this.last = this.head = null
    }

    return value
  }

  shift() {
    if (!this.head) {
      throw new Error("empty list");
    }

    const { value } = this.head
    if (this.head.next) {
      this.head = this.head.next
      this.head.prev = null
    } else {
      this.last = this.head = null
    }
    return value
  }

  unshift(value) {
    const node = new Node({ value, next: this.head })
    if (this.head) {
      this.head.prev = node
    } else {
      this.last = node
    }

    this.head = node
  }

  delete(value) {
    for (let cursor = this.head; cursor; cursor = cursor.next) {
      if (cursor.value === value) {
        const left = cursor.prev
        const rite = cursor.next

        if (left) {
          left.next = rite
        } else {
          this.head = this.head.next
        }

        if (rite) {
          rite.prev = left
        } else {
          this.last = this.last.prev
        }

        break
      }
    }
  }

  count() {
    let count = 0
    for (let cursor = this.head; cursor; cursor = cursor.next) {
      count += 1
    }
    return count
  }
}
