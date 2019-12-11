
#![feature(bool_to_option)]

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct P2 { x: i32, y: i32 }

impl P2 {
    fn new(x: i32, y: i32) -> P2 {
        P2 { x, y }
    }

    fn manhattan_norm(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum V2 {
    Dx(i32),
    Dy(i32)
}

impl V2 {
    fn length(self) -> i32 {
        match self {
            V2::Dx(d) => d.abs(),
            V2::Dy(d) => d.abs()
        }
    }
}

impl std::ops::Add<V2> for P2 {
    type Output = P2;
    fn add(self, rhs: V2) -> P2 {
        match rhs {
            V2::Dx(dx) => P2::new(self.x + dx, self.y),
            V2::Dy(dy) => P2::new(self.x, self.y + dy),
        }
    }
}

impl std::ops::AddAssign<V2> for P2 {
    fn add_assign(&mut self, rhs: V2) {
        *self = *self + rhs;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Seg {
    pub p: P2,
    pub d: V2
}

impl Seg {
    fn new(p: P2, d: V2) -> Seg {
        Seg { p, d }
    }
}

mod intersections {
    use super::*;

    fn impl_parallel(pa: i32, pb: i32, sa: i32, sb: i32) -> Option<(i32, i32, i32)> {
        use std::cmp::{min, max};
        let mina = min(pa, pa + sa);
        let maxa = max(pa, pa + sa);
        let minb = min(pb, pb + sb);
        let maxb = max(pb, pb + sb);
        let mini = max(mina, minb);
        let maxi = min(maxa, maxb);
        if maxi < mini { return None; }
        let da = (pa - mini).abs();
        let db = (pb - mini).abs();
        Some((mini, da, db))
    }

    fn impl_ortho(pa: i32, pb: i32, sa: i32, sb: i32, aopp: i32, bopp: i32) -> bool {
        use std::cmp::{min, max};
        let mina = min(pa, pa + sa);
        let maxa = max(pa, pa + sa);
        let minb = min(pb, pb + sb);
        let maxb = max(pb, pb + sb);
        (minb ..= maxb).contains(&aopp) && (mina ..= maxa).contains(&bopp)
    }

    pub fn intersection(a: &Seg, b: &Seg) -> Option<(P2, i32, i32)> {
        use V2::*;

        match (a.d, b.d) {
            (Dx(wa), Dx(wb)) => {
                if a.p.y != b.p.y { return None; }
                impl_parallel(a.p.x, b.p.x, wa, wb)
                    .map(|(ix, da, db)| (P2::new(ix, a.p.y), da, db))
            }
            (Dy(ha), Dy(hb)) => {
                if a.p.x != b.p.x { return None; }
                impl_parallel(a.p.y, b.p.y, ha, hb)
                    .map(|(iy, da, db)| (P2::new(a.p.x, iy), da, db))
            }
            (Dx(wa), Dy(hb)) => {
                impl_ortho(a.p.x, b.p.y, wa, hb, a.p.y, b.p.x)
                    .then(|| (P2::new(b.p.x, a.p.y), (b.p.x-a.p.x).abs(), (a.p.y-b.p.y).abs()))
            }
            (Dy(ha), Dx(wb)) => {
                impl_ortho(a.p.y, b.p.x, ha, wb, a.p.x, b.p.y)
                    .then(|| (P2::new(a.p.x, b.p.y), (b.p.x-a.p.x).abs(), (a.p.y-b.p.y).abs()))
            }
        }
    }
}

use intersections::intersection;

fn parse_path_segment(seg: &str) -> Option<V2> {
    let dist: i32 = seg[1..].parse().ok()?;
    let vec = match seg.chars().next()? {
        'R' => V2::Dx( dist),
        'L' => V2::Dx(-dist),
        'U' => V2::Dy( dist),
        'D' => V2::Dy(-dist),
        _ => { return None; }
    };
    Some(vec)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path_segment() {
        assert_eq!(parse_path_segment("R1" ).unwrap(), V2::Dx(  1));
        assert_eq!(parse_path_segment("L2" ).unwrap(), V2::Dx( -2));
        assert_eq!(parse_path_segment("U3" ).unwrap(), V2::Dy(  3));
        assert_eq!(parse_path_segment("D40").unwrap(), V2::Dy(-40));
    }
}



fn main() {
    let (wire_a, wire_b) = {
        let input = include_str!("../input");
        let mut wires = input
            .lines()
            .map(|line| {
                line.split(',')
                    .map(|seg| parse_path_segment(seg).expect("parsing path segment"))
                    .scan(
                        (0i32, P2::new(0,0)),
                        |(covered, pos), step| {
                            let result = (*covered, Seg::new(*pos, step));
                            *pos     += step;
                            *covered += step.length();
                            Some(result)
                        }
                    )
                    .collect::<Vec<_>>()
            });

        let a = wires.next().expect("reading first wire");
        let b = wires.next().expect("reading second wire");
        (a, b)
    };

    let mut intersections: Vec<_> = wire_a.iter()
        .flat_map(|(covered_a, seg_a)| {
            wire_b.iter()
                .filter_map(move |(covered_b, seg_b)| {
                    intersection(seg_a, seg_b)
                        .map(move |(point, da, db)| {
                            let mn = point.manhattan_norm();
                            let loop_mn = *covered_a + *covered_b + da + db;
                            (mn, loop_mn, point)
                        })
                })
        })
        .collect();

    intersections.sort_by_key(|(mn, _, _)| *mn);
    let (part1_norm, _, _) = intersections.iter().nth(1).unwrap();
    println!("Part 1: {:?}", part1_norm);

    intersections.sort_by_key(|(_, l, _)| *l);
    let (_, part2_sum, _) = intersections.iter().nth(1).unwrap();
    println!("Part 2: {:?}", part2_sum);
}

