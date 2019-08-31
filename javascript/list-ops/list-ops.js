export class List {
  constructor(list = []) {
    this.head = list.reduceRight(ListImpl.cons, null)
  }

  // Turns a ListImpl to a List
  static wrap(listImpl) {
    let l = new List()
    l.head = listImpl
    return l
  }

  get values() {
    return this.foldl((acc, v) => acc.concat(v), [])
  }

  append(list) {
    return List.wrap(ListImpl.append(this.head, list.head))
  }

  concat(list) {
    return List.wrap(list.foldl((acc, v) => ListImpl.append(acc, v.head), this.head))
  }

  filter(pred) {
    return List.wrap(this.foldr((acc, v) => pred(v) ? ListImpl.cons(acc, v) : acc, null))
  }

  map(fn) {
    return List.wrap(this.foldr((acc, v) => ListImpl.cons(acc, fn(v)), null))
  }

  length() {
    return this.foldl((acc) => acc + 1, 0)
  }

  foldl(fn, init) {
    return ListImpl.foldl(fn, init, this.head)
  }

  foldr(fn, init) {
    return ListImpl.foldr(fn, init, this.head)
  }

  reverse() {
    return List.wrap(this.foldl(ListImpl.cons, null))
  }
}

// Simple Functional Singly-Linked List
class ListImpl {
  constructor({ value, next }) {
    this.value = value
    this.next = next
  }

  static cons(list, value) {
    return new ListImpl({ value: value, next: list })
  }

  static foldr(fn, init, xs) {
    return xs ? fn(ListImpl.foldr(fn, init, xs.next), xs.value) : init
  }

  static foldl(fn, init, xs) {
    return xs ? ListImpl.foldl(fn, fn(init, xs.value), xs.next) : init
  }

  static append(base, xs) {
    return ListImpl.foldr(ListImpl.cons, xs, base)
  }
}
