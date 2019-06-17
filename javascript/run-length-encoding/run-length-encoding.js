// Based on a solution found here:
// https://exercism.io/tracks/typescript/exercises/run-length-encoding/solutions/801f5c767a424829b679be170f4223f4

export const encode = (plain) =>
    plain.replace(/(.)\1{1,}/g, (match, c) => match.length + c)

export const decode = (encoding) => encoding.replace(
    /(\d*)(.)/g, (_, length, c) => c.repeat(parseInt(length) || 1))
