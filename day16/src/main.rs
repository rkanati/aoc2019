
fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn lcm(a: usize, b: usize) -> usize {
    (a * b) / gcd(a, b)
}

fn fft(digits: Vec<i32>) -> Vec<i32> {
    dbg!(digits.len());
    let mut state = digits;
    for pass in 1..=100 {
        for i in 1..=state.len() {
            const BASE: [i32; 4] = [0, 1, 0, -1];
            let index_period = 4 * i;
            let factors_needed = lcm(index_period, state.len());
            let factors = (1..factors_needed)
                .map(|j| BASE[(j / i) % 4]);
            let scale = state.len() / factors_needed;

            let mut new_digit = 0;
            for (digit, factor) in state.iter().zip(factors.clone()) {
                new_digit += digit * factor * scale as i32;
            }

            let remainder = state.len() - (scale * factors_needed);
            for (digit, factor) in state.iter().zip(factors).take(remainder) {
                new_digit += digit * factor;
            }

            state[i-1] = new_digit.abs() % 10;

            print!("{} ", i);
        }

        //if pass % 10 == 0 {
            print!("...{}% ", pass);
        //}
    }

    println!();

    state
}

fn parse_digits(string: &str) -> Vec<i32> {
    string.chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect()
}

#[cfg(test)]
mod tests
{
    use super::*;

    #[test]
    fn test_fft() {
        assert_eq!(
            fft(parse_digits("80871224585914546619083218645595"))[0..8],
            parse_digits("24176176")[..]
        );

        assert_eq!(
            fft(parse_digits("19617804207202209144916044189917"))[0..8],
            parse_digits("73745418")[..]
        );

        assert_eq!(
            fft(parse_digits("69317163492948606335995924319873"))[0..8],
            parse_digits("52432133")[..]
        );
    }
}

fn main() {
    let input = include_str!("../input");
    let digits = parse_digits(input.trim());
    let result = fft(digits.clone());
    println!("Part 1: {:?}", &result[0..8]);

    let offset: usize = digits[0..7].iter()
        .fold(0, |s, d| s * 10 + *d as usize);

    let digits: Vec<i32> = digits.iter()
        .copied()
        .cycle()
        .take(digits.len() * 10_000)
        .skip(offset)
        .collect();

    let result = fft(digits);
    println!("Part 2: {:?}", &result[offset .. offset+8]);
}

