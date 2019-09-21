const ALPHA = [...'ABCDEFGHIJKLMNOPQRSTUVWXYZ']
const ROBOTS = []

// Generate the array of all robot names
ALPHA.forEach((a) => {
    ALPHA.forEach((b) => {
        for (let i = 0; i < 1000; i++) {
            ROBOTS.push([a, b, i.toString().padStart(3, '0')].join(''))
        }
    })
})

// This keeps track of what robot names have been given out
let position = ROBOTS.length

export class Robot {
    constructor() { this.reset() }
    reset() { this._name = generateName() }
    get name() { return this._name }
}

Robot.releaseNames = () => {
    position = ROBOTS.length
};

// This randomly selects a name from the list of available names
// (between 0 and position). The name is then swapped with the unused name at
// the end and position is decremented. This is basically a Fisher-Yates
// shuffle. Rather than precomputing the shuffle, we are randomly selecting
// the name as they are asked for.
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
