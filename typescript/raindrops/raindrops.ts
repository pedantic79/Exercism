export default class Raindrop {
    convert(num: number): string {
        let output = ""
        if (num % 3 === 0) {
            output += 'Pling'
        }

        if (num % 5 === 0) {
            output += 'Plang'
        }

        if (num % 7 === 0) {
            output += 'Plong'
        }

        if (output === '') {
            return num.toString()
        }

        return output
    }
}
