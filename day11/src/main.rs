
use {
    std::{
        collections::HashMap,
        convert::TryFrom,
    },
    intcode_rk::{VM, VMResult, parse_programs},
};


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Colour {
    Black,
    White
}

impl TryFrom<i64> for Colour {
    type Error = ();
    fn try_from(i: i64) -> Result<Colour, ()> {
        let colour = match i {
            0 => Colour::Black,
            1 => Colour::White,
            _ => return Err(())
        };
        Ok(colour)
    }
}

impl From<Colour> for i64 {
    fn from(c: Colour) -> i64 {
        match c {
            Colour::Black => 0,
            Colour::White => 1
        }
    }
}

struct PaintJob {
    pub panels: HashMap<(i32, i32), Colour>,
    pub x_bounds: (i32, i32),
    pub y_bounds: (i32, i32)
}

impl PaintJob {
    fn width(&self) -> usize {
        (self.x_bounds.1 - self.x_bounds.0) as usize + 1
    }

    fn height(&self) -> usize {
        (self.y_bounds.1 - self.y_bounds.0) as usize + 1
    }

    fn dims(&self) -> (usize, usize) {
        (self.width(), self.height())
    }

    fn white_panels<'a> (&'a self) -> impl Iterator<Item = (i32, i32)> + 'a {
        self.panels.iter()
            .filter(|(_, c)| **c == Colour::White)
            .map(move |((x, y), _)| (*x - self.x_bounds.0, *y - self.y_bounds.0))
    }
}

fn run_robot(mem: Vec<i64>, start_colour: Colour) -> PaintJob {
    let mut panels: HashMap<(i32, i32), Colour> = HashMap::new();
    panels.insert((0, 0), start_colour);

    // right handed, +ve y up
    let mut pos_x = 0;
    let mut pos_y = 0;
    let mut dir_x = 0;
    let mut dir_y = 1;

    let mut min_x = std::i32::MAX;
    let mut max_x = std::i32::MIN;
    let mut min_y = std::i32::MAX;
    let mut max_y = std::i32::MIN;

    let mut vm = VM::new(mem);
    loop {
        match vm.run() {
            VMResult::Output(paint) => {
                let colour = Colour::try_from(paint).expect("bad paint output");
                panels.insert((pos_x, pos_y), colour);

                let turn = if let VMResult::Output(turn) = vm.run() {
                    turn
                }
                else {
                    panic!("paint output without turn");
                };

                let (new_dir_x, new_dir_y) = match turn {
                    0 => (-dir_y, dir_x), // left 90
                    1 => (dir_y, -dir_x), // right 90
                    _ => panic!("bad direction")
                };
                dir_x = new_dir_x;
                dir_y = new_dir_y;
                pos_x += dir_x;
                pos_y += dir_y;

                min_x = min_x.min(pos_x);
                max_x = max_x.max(pos_x);
                min_y = min_y.min(pos_y);
                max_y = max_y.max(pos_y);
            }

            VMResult::WaitInput => {
                let colour = panels.entry((pos_x, pos_y)).or_insert(Colour::Black);
                vm.feed_input(i64::from(*colour))
            }

            VMResult::Stopped => {
                break;
            }
        }
    }

    PaintJob {
        panels,
        x_bounds: (min_x, max_x),
        y_bounds: (min_y, max_y)
    }
}

fn main() {
    let input = include_str!("../input");
    let mem = parse_programs(input).nth(0).expect("loading program");

    let part1_paintjob = run_robot(mem.clone(), Colour::Black);
    println!("Part 1: distinct panels painted: {}", part1_paintjob.panels.len());

    let reg_plate = run_robot(mem, Colour::White);
    println!("Part 2:");

    let (width, height) = reg_plate.dims();

    let mut image: Vec<_> = std::iter::repeat(' ')
        .take(width * height)
        .collect();

    for (x, y) in reg_plate.white_panels() {
        let x = x as usize;
        let y = height - y as usize - 1;
        image[y * width + x] = '#';
    }

    for row in image.chunks_exact(width as usize) {
        let string: String = row.iter().collect();
        println!("{}", string);
    }
}

