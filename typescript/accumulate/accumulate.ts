// This is the same as map, and the simple way to do this would be to use
// list.map(fn), but the problem explicitly states to avoid that.
function accumulate<T, U>(list: T[], fn: (e: T) => U): U[] {
    let output: U[] = []
    list.forEach((element: T) => { output.push(fn(element)) })
    return output
}

export default accumulate
