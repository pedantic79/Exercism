export class ArgumentError {
    constructor(message) {
        this.name = 'ArgumentError'
        this.message = message
    }
}

export class WordProblem {
    constructor(input) {
        this.input = input
    }

    answer() {
        if (!this.input.endsWith('?')) {
            throw new ArgumentError(`parse error: ${this.input} does not end with ?`)
        }

        const tokens = this.input.replace(/\?$/, '').split(/ +/)

        check_token(tokens.shift(), 'What')
        check_token(tokens.shift(), 'is')
        let total = parse_num(tokens.shift())

        while (tokens.length > 0) {
            const operation = parse_op(tokens)
            const number = parse_num(tokens.shift())
            total = operation(total, number)
        }
        return total
    }
}

const check_token = (expected, actual) => {
    if (expected !== actual) {
        throw new ArgumentError(`parse error: '${expected}' is expected, found ${actual}`)
    }
}

const parse_num = (token) => {
    const number = token.match(/^(-?\d+)$/)
    if (number === undefined) {
        throw new ArgumentError(`parse error: invalid number ${token}`)
    }
    return Number(number[1])
}

const OPERATIONS = {
    'plus': (total, value) => total + value,
    'minus': (total, value) => total - value,
    'multiplied': (total, value) => total * value,
    'divided': (total, value) => total / value
}

const parse_op = (tokens) => {
    const token = tokens.shift()
    if (token === 'multiplied' || token === 'divided') {
        check_token('by', tokens.shift())
    }

    const operation = OPERATIONS[token]
    if (operation === undefined) {
        throw new ArgumentError(`parse error: unknown operation ${token}`)
    }

    return operation
}
