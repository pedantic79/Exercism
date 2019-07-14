#[derive(PartialEq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

pub struct Robot {
    x: i32,
    y: i32,
    d: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Self { x, y, d }
    }

    pub fn turn_right(self) -> Self {
        let d = match self.d {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        };

        Self { d, ..self }
    }

    pub fn turn_left(self) -> Self {
        let d = match self.d {
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
            Direction::East => Direction::North,
        };

        Self { d, ..self }
    }

    pub fn advance(self) -> Self {
        let (x, y) = match self.d {
            Direction::North => (self.x, self.y + 1),
            Direction::West => (self.x - 1, self.y),
            Direction::South => (self.x, self.y - 1),
            Direction::East => (self.x + 1, self.y),
        };
        Self { x, y, ..self }
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |robot, i| match i {
            'A' => robot.advance(),
            'L' => robot.turn_left(),
            'R' => robot.turn_right(),
            _ => robot,
        })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
