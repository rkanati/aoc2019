
use {
    std::collections::HashMap,
};

type Multiple<'a> = (i64, &'a str);
type Equations<'a> = HashMap<&'a str, (i64, Vec<Multiple<'a>>)>;

fn parse_multiple<'a> (raw: &'a str) -> Multiple<'a> {
    let mut parts = raw.trim().split_whitespace();
    let count = parts.next().unwrap().parse().unwrap();
    let name  = parts.next().unwrap();
    (count, name)
}

fn refine<'a> (
    stock: &mut HashMap<&'a str, i64>,
    eqns: &Equations<'a>)
{
    loop {
        let mut delta: HashMap<&'a str, i64> = HashMap::new();

        for (name, deficit) in stock.iter().filter(|(_, count)| **count < 0) {
            let (prod_factor, reqs) = if let Some(eqn) = eqns.get(name) {
                eqn
            }
            else {
                continue;
            };

            let runs = (deficit.abs() + prod_factor - 1) / prod_factor;

            for (req_factor, req) in reqs.iter() {
                *delta.entry(req).or_insert(0) -= runs * req_factor;
            }

            *delta.entry(name).or_insert(0) += runs * prod_factor;
        }

        if delta.is_empty() { break; }

        for (name, count) in delta.iter() {
            *stock.entry(name).or_insert(0) += count;
        }
    }
}

fn get_ore_required<'a> (eqns: &Equations<'a>, name: &'a str, need: i64) -> i64 {
    let mut stock = HashMap::new();
    stock.insert(name, -need);
    refine(&mut stock, eqns);
    -stock.get("ORE").unwrap()
}

fn main() {
    let input = include_str!("../input");
    let eqns: HashMap<_, _> = input.lines()
        .map(|line| {
            let mut parts = line.split("=>");
            let reqs_part = parts.next().unwrap();
            let prod_part = parts.next().unwrap();
            let reqs: Vec<(i64, &'static str)> = reqs_part.trim()
                .split(',')
                .map(parse_multiple)
                .collect();
            let (count, product) = parse_multiple(prod_part);
            (product, (count, reqs))
        })
        .collect();

    let ore_required = get_ore_required(&eqns, "FUEL", 1);
    println!("Part 1: ore required for 1 fuel: {}", ore_required);

    // Iteratively close in on the highest fuel for part 2
    // This is probably not as efficient as a proper linear algebra optimization method,
    // but it's a lot less work to use the part 1 solver than implement something new.
    const TOTAL_ORE: i64 = 1_000_000_000_000;

    let mut fuel_guess = TOTAL_ORE / ore_required;
    let mut fuel_delta = 0;

    let ore_guess = loop {
        fuel_guess += fuel_delta;
        let ore_guess = get_ore_required(&eqns, "FUEL", fuel_guess);

        fuel_delta = if ore_guess < TOTAL_ORE {
            if fuel_delta >= 0       { fuel_guess / 2 }
            else if fuel_delta == -1 { break ore_guess; }
            else                     { (-fuel_delta / 2).max(1) }
        }
        else if ore_guess > TOTAL_ORE {
            if fuel_delta <= 0 { (-fuel_guess / 2).min(-1) }
            else               { (-fuel_delta / 2).min(-1) }
        }
        else {
            break ore_guess;
        }
    };

    println!("Part 2: {} fuel for {} ore", fuel_guess, ore_guess);
}

