
use {
    std::collections::{HashSet, HashMap},
};

// euclidean algorithm, for reducing ratios
fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

// reduce a integer vector v into
// - d: a direction (in lowest terms)
// - k: a factor for this direction
// such that v = kd
struct Relative {
    pub direction: (i32, i32),
    pub factor:    i32,
}

fn relative(x: i32, y: i32) -> Option<Relative> {
    let sx = x.signum();
    let sy = y.signum();

    let mx = x.abs();
    let my = y.abs();

    if sx == 0 && sy == 0 {
        return None;
    }
    else if sx == 0 {
        return Some(Relative { direction: (sx, sy), factor: my });
    }
    else if sy == 0 {
        return Some(Relative { direction: (sx, sy), factor: mx });
    }

    let factor = gcd(mx, my);
    let direction = (sx * (mx / factor), sy * (my / factor));

    Some(Relative { direction, factor })
}

// clockwise angle from "up", for x-right, y-down coords
fn bearing(x: i32, y: i32) -> f64 {
    let x = x as f64;
    let y = y as f64;
    let raw = x.atan2(-y);
    if raw < 0. { raw + 2. * std::f64::consts::PI }
    else        { raw }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bearing() {
        assert!((bearing( 1,  1) -      std::f64::consts::FRAC_PI_4).abs() < 0.00001);
        assert!((bearing( 1, -1) - 3. * std::f64::consts::FRAC_PI_4).abs() < 0.00001);
        assert!((bearing(-1, -1) - 5. * std::f64::consts::FRAC_PI_4).abs() < 0.00001);
        assert!((bearing(-1,  1) - 7. * std::f64::consts::FRAC_PI_4).abs() < 0.00001);

        assert!((bearing( 0,  1)                                   ).abs() < 0.00001);
        assert!((bearing( 1,  0) -      std::f64::consts::FRAC_PI_2).abs() < 0.00001);
        assert!((bearing( 0, -1) - 2. * std::f64::consts::FRAC_PI_2).abs() < 0.00001);
        assert!((bearing(-1,  0) - 3. * std::f64::consts::FRAC_PI_2).abs() < 0.00001);
    }
}

fn main() {
    // parse input into vec of points
    let input = include_str!("../input");
    let roids: Vec<_> = input.lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, ch)| *ch == '#')
                .map(move |(x, _)| (x as i32, y as i32))
        })
        .collect();

    // find the best point, by line-of-sight count, for the station
    let ((station_x, station_y), best_count) = roids.iter()
        .map(|(cx, cy)| {
            let directions: HashSet<_> = roids.iter()
                .filter_map(|(rx, ry)| relative(rx - cx, ry - cy).map(|r| r.direction))
                .collect();
            ((cx, cy), directions.len())
        })
        .max_by_key(|(_, num_directions)| *num_directions)
        .unwrap();

    println!("Part 1: {} from ({} {})", best_count, station_x, station_y);

    // figure out the order in which roids will be vaporized
    let roids_in_order = {
        // map each roid according to its direction from the station
        let mut map: HashMap<_, _> = HashMap::new();

        roids.iter()
            .filter_map(|(rx, ry)|
                relative(rx - station_x, ry - station_y)
                    .map(|rel| (rel, (rx, ry)))
            )
            .for_each(|(rel, roid)| {
                map.entry(rel.direction)
                    .or_insert(Vec::new())
                    .push((rel.factor, roid));
            });

        // for each direction, sort the roids along it by increasing distance
        for roids in map.values_mut() {
            roids.sort_by_key(|(factor, _)| *factor);
        }

        // find bearing for each direction
        // key each roid by total angle turned through by laser,
        // i.e. its bearing + a full turn for each roid occluding it
        let mut vec: Vec<_> = map.iter()
            .flat_map(|((dx, dy), roids)| {
                let theta = bearing(*dx, *dy);
                roids.iter()
                    .enumerate()
                    .map(move |(rank, (_, roid))|
                        (rank as f64 * 2. * std::f64::consts::PI + theta, *roid)
                    )
            })
            .collect();

        // sort by this key angle
        vec.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());
        vec
    };

    // get the 200th roid vaporized
    let (_, roid_200th) = roids_in_order.iter().nth(199).unwrap();
    let (x, y) = *roid_200th;

    println!("Part 2: ({} {}) = {}", x, y, x * 100 + y);
}

