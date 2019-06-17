class RunLengthEncoding {
    static encode(plain: string): string {
        return plain.replace(/(.)\1{1,}/g, (match, c) => match.length + c)
    }

    static decode(encoded: string): string {
        return encoded.replace(/(\d*)(.)/g, (_, len, c) => c.repeat(parseInt(len) || 1))
    }
}

export default RunLengthEncoding
