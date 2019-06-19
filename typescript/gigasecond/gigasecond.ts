class Gigasecond {
    constructor(private readonly start: Date) { }

    date(): Date {
        return new Date(this.start.getTime() + 10 ** 12);
    }
}

export default Gigasecond
