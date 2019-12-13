
use {
    intcode_rk::{VM, VMResult, parse_programs},
    std::{
        convert::TryFrom,
        io::Write,
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TileType {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball
}

#[derive(Clone, Copy, Debug)]
struct BadTileID(i64);

impl std::convert::TryFrom<i64> for TileType {
    type Error = BadTileID;
    fn try_from(id: i64) -> Result<TileType, BadTileID> {
        use TileType::*;
        let ty = match id {
            0 => Empty,
            1 => Wall,
            2 => Block,
            3 => Paddle,
            4 => Ball,
            _ => return Err(BadTileID(id))
        };
        Ok(ty)
    }
}

impl From<TileType> for char {
    fn from(ty: TileType) -> char {
        use TileType::*;
        match ty {
            Empty  => ' ',
            Wall   => '█',
            Block  => '◻',
            Paddle => '━',
            Ball   => '●',
        }
    }
}

const MAX_WIDTH:  usize = 40;
const MAX_HEIGHT: usize = 26;

fn print_board(buffer: &mut String, tiles: &[TileType]) {
    for (y, row) in tiles.chunks_exact(MAX_WIDTH).enumerate() {
        let chars: String = row.iter().copied()
            .enumerate()
            .map(|(x, ty)| if ty == TileType::Block {
                    let hash = x*3 + y*5;
                    const CHARS: &'static [char] =
                        &['▣','■','□','◰','◱','◲','◳','◧','◨','◩','◪','◫','◙'];
                    let index = hash % CHARS.len();
                    CHARS[index]
                }
                else {
                    char::from(ty)
                }
            )
            .collect();
        buffer.push_str(&chars);
        buffer.push('\n');
    }

    buffer.push_str("\n\n");
}

fn play_game(mut mem: Vec<i64>, coins: Option<i64>) -> (Vec<TileType>, i64) {
    if let Some(quarters) = coins {
        mem[0] = quarters;
    }

    let mut screen_buf = String::new();

    let mut board: Vec<TileType> = std::iter::repeat(TileType::Empty)
        .take(MAX_WIDTH * MAX_HEIGHT)
        .collect();

    let mut score = 0;

    let mut paddle_x = 0;
    let mut ball_x   = 0;

    let mut vm = VM::new(mem);
    loop {
        match vm.run() {
            VMResult::Output(x) => {
                let y = vm.run().expect_output("missing y");
                let t = vm.run().expect_output("missing tile id / score");
                if x == -1 && y == 0 {
                    score = t;
                    continue;
                }

                assert!((x as usize) < MAX_WIDTH && (y as usize) < MAX_HEIGHT);
                let ty = TileType::try_from(t).expect("bad tile id");
                let index = y as usize * MAX_WIDTH + x as usize;
                board[index] = ty;
                match ty {
                    TileType::Paddle => { paddle_x = x; }
                    TileType::Ball   => { ball_x   = x; }
                    _                => { }
                }
            }

            VMResult::WaitInput => {
                let direction = (ball_x - paddle_x).signum();
                vm.feed_input(direction);

                screen_buf.clear();
                print_board(&mut screen_buf, &board);
                let mut stdout = std::io::stdout();
                write!(
                    &mut stdout,
                    "SCORE: {:06}                INSERT COIN\n",
                    score
                ).unwrap();
                stdout.flush().unwrap();
                stdout.write(screen_buf.as_bytes()).unwrap();

                std::thread::sleep(std::time::Duration::from_millis(16));
            }

            VMResult::Stopped => {
                break;
            }
        }
    }

    (board, score)
}

fn main() {
    let input = include_str!("../input");
    let mem = parse_programs(input).nth(0).expect("loading program");

    let (board, _) = play_game(mem.clone(), None);
    let block_count = board.iter().copied()
        .filter(|ty| *ty == TileType::Block)
        .count();

    let (_, score) = play_game(mem, Some(2));

    println!("Part 1: block count: {}", block_count);
    println!("Part 2: final score: {}", score);
}

