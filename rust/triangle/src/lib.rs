use std::ops::Add;

#[derive(PartialEq)]
pub enum Triangle {
    Equalateral,
    Scalene,
    Isosceles,
}

impl Triangle {
    pub fn build<T>(sides: [T; 3]) -> Option<Self>
    where
        T: PartialOrd + Copy + Default + Add<Output = T>,
    {
        if Self::is_valid(&sides) {
            Some(Self::classify(&sides))
        } else {
            None
        }
    }

    pub fn is_equilateral(&self) -> bool {
        *self == Triangle::Equalateral
    }

    pub fn is_scalene(&self) -> bool {
        *self == Triangle::Scalene
    }

    pub fn is_isosceles(&self) -> bool {
        *self == Triangle::Isosceles
    }

    fn is_valid<T>(sides: &[T; 3]) -> bool
    where
        T: Add<Output = T> + Copy + Default + PartialOrd,
    {
        sides.iter().all(|s| *s > T::default())
            && sides[0] + sides[1] >= sides[2]
            && sides[0] + sides[2] >= sides[1]
            && sides[1] + sides[2] >= sides[0]
    }

    fn classify<T>(sides: &[T; 3]) -> Self
    where
        T: PartialEq,
    {
        if sides[0] == sides[1] && sides[1] == sides[2] {
            Triangle::Equalateral
        } else if sides[0] == sides[1] || sides[1] == sides[2] || sides[2] == sides[0] {
            Triangle::Isosceles
        } else {
            Triangle::Scalene
        }
    }
}
