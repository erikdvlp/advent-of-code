#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Range {
    pub lower: u64,
    pub upper: u64,
}

impl Range {
    pub fn contains(&self, ingredient: &Ingredient) -> bool {
        ingredient.0 >= self.lower && ingredient.0 <= self.upper
    }

    pub fn overlaps(a: &Self, b: &Self) -> bool {
        a.lower <= b.upper + 1 && b.lower <= a.upper + 1
    }

    pub fn len(&self) -> usize {
        self.upper as usize - self.lower as usize + 1
    }
}

#[derive(Debug)]
pub struct Ingredient(pub u64);
