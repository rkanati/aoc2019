
#[macro_use]
extern crate scan_fmt;

use {
    std::{
        ops::{Add, Sub, AddAssign, SubAssign},
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct V3 {
    x: i32,
    y: i32,
    z: i32
}

impl V3 {
    fn new(x: i32, y: i32, z: i32) -> V3 {
        V3 { x, y, z }
    }

    fn map(self, f: impl Fn(i32) -> i32) -> V3 {
        V3 { x: f(self.x), y: f(self.y), z: f(self.z) }
    }

    fn map2(self, rhs: V3, f: impl Fn(i32, i32) -> i32) -> V3 {
        V3 { x: f(self.x, rhs.x), y: f(self.y, rhs.y), z: f(self.z, rhs.z) }
    }

    fn fold(self, mut s: i32, f: impl Fn(i32, i32) -> i32) -> i32 {
        s = f(s, self.x);
        s = f(s, self.y);
        f(s, self.z)
    }

    fn sum(self) -> i32 {
        self.fold(0, |s, x| s + x)
    }
}

#[derive(Clone, Copy, Debug)]
struct ParseV3Error { }

impl std::str::FromStr for V3 {
    type Err = ParseV3Error;
    fn from_str(s: &str) -> Result<V3, Self::Err> {
        let (x, y, z) = scan_fmt!(s, "<x={d}, y={d}, z={d}>", i32, i32, i32)
            .map_err(|_| ParseV3Error { })?;
        Ok(V3::new(x, y, z))
    }
}

#[cfg(tests)]
mod tests {
    use super::*;

    #[test]
    fn test_v3_fromstr() {
        assert_eq!("<x=1, y=-2, z=34>".parse::<V3>(), Ok(V3::new(1, -2, 34)));
    }
}

impl Add for V3 {
    type Output = V3;
    fn add(self, rhs: V3) -> V3 {
        self.map2(rhs, |a, b| a + b)
    }
}

impl AddAssign for V3 {
    fn add_assign(&mut self, rhs: V3) {
        *self = *self + rhs;
    }
}

impl Sub for V3 {
    type Output = V3;
    fn sub(self, rhs: V3) -> V3 {
        self.map2(rhs, |a, b| a - b)
    }
}

impl SubAssign for V3 {
    fn sub_assign(&mut self, rhs: V3) {
        *self = *self - rhs;
    }
}

trait MoonCoord: AddAssign + SubAssign + Sized + Copy + Eq {
    fn gravity(self: Self, rhs: Self) -> Self;
}

impl MoonCoord for V3 {
    fn gravity(self: V3, rhs: V3) -> V3 {
        (rhs - self).map(i32::signum)
    }
}

impl MoonCoord for i32 {
    fn gravity(self: i32, rhs: i32) -> i32 {
        (rhs - self).signum()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Moon<X: MoonCoord> {
    pub pos: X,
    pub vel: X
}

fn step<X> (moons: &mut [Moon<X>])
    where X: MoonCoord
{
    for index_a in 0..moons.len() {
        for index_b in 0..moons.len() {
            if index_b <= index_a { continue; }
            let mut moon_a = moons[index_a];
            let mut moon_b = moons[index_b];
            let g = moon_a.pos.gravity(moon_b.pos);
            moon_a.vel += g;
            moon_b.vel -= g;
            moons[index_a] = moon_a;
            moons[index_b] = moon_b;
        }
    }

    for moon in moons.iter_mut() {
        moon.pos += moon.vel;
    }
}

fn steps_to_repeat(mut moons: [Moon<i32>; 4]) -> usize {
    let first = moons;
    for i in 1.. {
        step(&mut moons);
        if moons == first { return i; }
    }
    unreachable!()
}

fn steps_to_repeat_axis(initial: &Vec<Moon<V3>>, f: impl Fn(V3) -> i32) -> usize {
    assert!(initial.len() == 4);
    let moons: Vec<Moon<i32>> = initial.iter()
        .map(|moon| Moon { pos: f(moon.pos), vel: f(moon.vel) })
        .collect();
    let moons = [moons[0], moons[1], moons[2], moons[3]];
    steps_to_repeat(moons)
}

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

fn main() {
    let input = include_str!("../input");
    let initial_moons: Vec<Moon<V3>> = input.lines()
        .map(|line| {
            let pos = line.parse().expect("parsing input");
            let vel = V3::new(0, 0, 0);
            Moon { pos, vel }
        })
        .collect();

    // part 1
    {   let mut moons = initial_moons.clone();

        for _ in 0..1000 {
            step(&mut moons);
        }

        let energy: i32 = moons.iter()
            .map(|moon| {
                let pot = moon.pos.map(i32::abs).sum();
                let kin = moon.vel.map(i32::abs).sum();
                pot * kin
            })
            .sum();

        println!("Part 1: total energy after 1000 steps: {}", energy);
    }

    let rep_steps_x = steps_to_repeat_axis(&initial_moons, |v| v.x);
    let rep_steps_y = steps_to_repeat_axis(&initial_moons, |v| v.y);
    let rep_steps_z = steps_to_repeat_axis(&initial_moons, |v| v.z);
    let rep_steps = lcm(rep_steps_x, lcm(rep_steps_y, rep_steps_z));
    println!("Part 2: steps to repeat: {}", rep_steps);
}

