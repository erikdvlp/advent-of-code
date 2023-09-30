type Section = u8;
pub type AssignmentPair = ((Section, Section), (Section, Section));

pub enum Overlap {
    None,
    Partial,
    Full,
}

/// Parses a given input file line and converts it into an assignment pair.
pub fn line_to_assignment_pair(line: &str) -> AssignmentPair {
    let mut parts: Vec<Section> = Vec::new();
    let assignments = line.split(',');
    for assignment in assignments {
        let sections = assignment.split('-');
        for section in sections {
            parts.push(section.parse::<Section>().unwrap());
        }
    }
    (
        (*parts.first().unwrap(), *parts.get(1).unwrap()),
        (*parts.get(2).unwrap(), *parts.get(3).unwrap()),
    )
}

/// Gets an overlap for a given assignment pair.
pub fn get_overlap(assignment_pair: &AssignmentPair) -> Overlap {
    let ((w, x), (y, z)) = assignment_pair;
    if (w <= y && x >= z) || (w >= y && x <= z) {
        Overlap::Full
    } else if (w <= y && y <= x) || (w <= z && z <= x) {
        Overlap::Partial
    } else {
        Overlap::None
    }
}
