class ArmstrongNumbers {
    static isArmstrongNumber(num: number): boolean {
        const digits = num.toString().split('')
        const len = digits.length
        const armstrong = digits.reduce((acc, digit) => acc + parseInt(digit) ** len, 0)

        return armstrong == num
    }
}

export default ArmstrongNumbers
