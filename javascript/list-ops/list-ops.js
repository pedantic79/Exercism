export class List {
  constructor(list = []) {
    this.head = list.reduceRight(LinkedList.cons, null)
  }

  // Turns a LinkedList to a List
  static wrap(linkedList) {
    let l = new List()
    l.head = linkedList
    return l
  }

  get values() {
    return this.foldl((acc, v) => {
      acc.push(v)
      return acc
    }, [])
  }

  append(list) {
    return List.wrap(LinkedList.append(this.head, list.head))
  }

  concat(list) {
    return List.wrap(list.foldl((acc, v) => LinkedList.append(acc, v.head), this.head))
  }

  filter(pred) {
    return List.wrap(this.foldr((acc, v) => pred(v) ? LinkedList.cons(acc, v) : acc, null))
  }

  map(fn) {
    return List.wrap(this.foldr((acc, v) => LinkedList.cons(acc, fn(v)), null))
  }

  length() {
    return this.foldl((acc, v) => acc + 1, 0)
  }

  foldl(fn, init) {
    return LinkedList.foldl(fn, init, this.head)
  }

  foldr(fn, init) {
    return LinkedList.foldr(fn, init, this.head)
  }

  reverse() {
    return List.wrap(this.foldl(LinkedList.cons, null))
  }
}

// Simple Functional Singly-Linked List
class LinkedList {
  constructor({ value, next }) {
    this.value = value
    this.next = next
  }

  static cons(list, value) {
    return new LinkedList({ value: value, next: list })
  }

  static foldr(fn, init, xs) {
    return xs ? fn(LinkedList.foldr(fn, init, xs.next), xs.value) : init
  }

  static foldl(fn, init, xs) {
    return xs ? LinkedList.foldl(fn, fn(init, xs.value), xs.next) : init
  }

  static append(base, xs) {
    return base ? LinkedList.foldr(LinkedList.cons, xs, base) : xs
  }
}
