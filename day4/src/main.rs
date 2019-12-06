

fn nondecreasing(from: u32, length: u32) -> impl Iterator<Item = u32> {
    assert!(length > 0);
    (from ..= 9)
        .flat_map(move |dig| -> Box<dyn Iterator<Item = u32>> {
            let head = dig * 10u32.pow(length-1);
            if length > 1 {
                Box::new(nondecreasing(dig, length - 1).map(move |tail| head + tail))
            }
            else {
                Box::new(std::iter::once(head))
            }
        })
}

fn initial_guesses() -> impl Iterator<Item = String> {
    nondecreasing(0, 6)
        .skip_while(|guess| *guess <  137683)
        .take_while(|guess| *guess <= 596253)
        .map(|guess| guess.to_string())
}

fn main() {
    let part1_count = initial_guesses()
        .filter(|guess| {
            guess.chars().zip(guess.chars().skip(1))
                .any(|(a, b)| a == b)
        })
        .count();

    println!("Part 1: {} possible codes", part1_count);

    let part2_count = initial_guesses()
        .filter(|guess| {
            guess.chars()
                .fold(None, |state, cur| match state {
                    None => Some((cur, 1, false)),
                    Some((prev, run, has_pair)) =>
                        if prev == cur { Some((cur, run+1, has_pair            )) }
                        else           { Some((cur, 1,     has_pair || run == 2)) }
                })
                .map(|(_, run, has_pair)| has_pair || run == 2)
                .unwrap()
        })
        .count();

    println!("Part 2: {} possible codes", part2_count);
}

