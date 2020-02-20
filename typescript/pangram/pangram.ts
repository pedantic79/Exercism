const ALPHA_REGEX = /[a-z]/g
const ALPHA_SIZE = 26

export default class Pangram {
    constructor(private readonly input: Readonly<string>) { }

    isPangram(): boolean {
        const lowerCasedInput = this.input.toLowerCase()
        return new Set(lowerCasedInput.match(ALPHA_REGEX)).size == ALPHA_SIZE
    }
}
