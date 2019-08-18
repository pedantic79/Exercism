use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub struct CustomSet<T>
where
    T: Hash + Eq,
{
    map: HashMap<T, ()>,
}

impl<T> FromIterator<T> for CustomSet<T>
where
    T: Hash + Eq,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            map: iter.into_iter().map(|k| (k, ())).collect(),
        }
    }
}

impl<T> CustomSet<T>
where
    T: Hash + Eq + Clone,
{
    pub fn new(input: &[T]) -> Self {
        input.iter().cloned().collect()
    }

    pub fn contains(&self, element: &T) -> bool {
        self.map.contains_key(element)
    }

    pub fn add(&mut self, element: T) {
        self.map.insert(element, ());
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.map.keys().all(|elem| other.contains(elem))
    }

    pub fn is_empty(&self) -> bool {
        self.map.len() == 0
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        !self.map.keys().any(|elem| other.contains(elem))
    }

    pub fn intersection(&self, other: &Self) -> Self {
        self.map
            .keys()
            .filter(|elem| other.contains(elem))
            .cloned()
            .collect()
    }

    pub fn difference(&self, other: &Self) -> Self {
        self.map
            .keys()
            .filter(|elem| !other.contains(elem))
            .cloned()
            .collect()
    }

    pub fn union(&self, other: &Self) -> Self {
        self.map.keys().chain(other.map.keys()).cloned().collect()
    }
}
