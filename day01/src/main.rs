
fn fuel_for_mass(mass: i32) -> i32 {
    std::iter::successors(
        Some(mass),
        |load| {
            let requirement = load / 3 - 2;
            if requirement > 0 { Some(requirement) }
            else               { None }
        }
    ).skip(1).sum::<i32>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuel_for_mass() {
        assert_eq!(fuel_for_mass(    14),     2);
        assert_eq!(fuel_for_mass(  1969),   966);
        assert_eq!(fuel_for_mass(100756), 50346);
    }
}

fn main() {
    let input = include_str!("../input");

    let fuel: i32 = input.lines()
        .map(|line| {
            let mass: i32 = line.parse().expect("bad input");
            fuel_for_mass(mass)
        })
        .sum();

    println!("Total fuel reuqired for modules: {}", fuel);

}

