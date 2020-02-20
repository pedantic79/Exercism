class ReverseString {
    static reverse(input: Readonly<string>): string {
        return input.split('').reverse().join('')
    }
}

export default ReverseString
