const ALPHA = [...'ABCDEFGHIJKLMNOPQRSTUVWXYZ']
const ROBOTS = []

ALPHA.forEach((a) => {
    ALPHA.forEach((b) => {
        for (let i = 0; i < 1000; i++) {
            ROBOTS.push([a, b, i.toString().padStart(3, '0')].join(''))
        }
    })
})

let position = ROBOTS.length

export class Robot {
    constructor() { this.reset() }
    reset() { this._name = generateName() }
    get name() { return this._name }
}

Robot.releaseNames = () => {
    position = ROBOTS.length
};

const generateName = () => {
    if (position > 0) {
        const r = Math.floor(Math.random() * position)

        const temp = ROBOTS[r]
        ROBOTS[r] = ROBOTS[position - 1]
        ROBOTS[position - 1] = temp

        position--
        return temp
    } else {
        throw new Error("no robot names remain")
    }
}
