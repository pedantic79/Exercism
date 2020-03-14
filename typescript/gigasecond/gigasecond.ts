class Gigasecond {
    private readonly start: Date
    constructor(start: Date) { this.start = start }

    date(): Date {
        return new Date(this.start.getTime() + 10 ** 12)
    }
}

export default Gigasecond
