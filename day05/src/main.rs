
use {
    std::{
        io::{Write, BufRead},
    },
};

#[derive(Clone, Copy)]
enum Operand {
    Imm(i32),
    Mem(u32)
}

impl Operand {
    fn decode<'a>(word: i32, iword: i32) -> Option<(Operand, i32)> {
        use Operand::*;
        let mode = iword % 10;
        let operand = match mode {
            0 => Mem(word as u32),
            1 => Imm(word),
            _ => { return None; }
        };

        Some((operand, iword / 10))
    }
}

#[derive(Clone, Copy)]
enum OpXXM {
    Add,
    Mul,
    Les,
    Equ,
}

#[derive(Clone, Copy)]
enum OpXX {
    JiT,
    JiF,
}

#[derive(Clone, Copy)]
enum OpM {
    Inp,
}

#[derive(Clone, Copy)]
enum OpX {
    Out,
}

#[derive(Clone, Copy)]
enum OpN {
    Halt,
}

#[derive(Clone, Copy)]
enum Instruction {
    XXM(OpXXM, Operand, Operand, u32),
    XX(OpXX, Operand, Operand),
    M(OpM, u32),
    X(OpX, Operand),
    N(OpN)
}

impl Instruction {
    fn decode(stream: &[i32]) -> Option<(Instruction, u32)> {
        use Instruction::*;

        let iword = stream[0];
        let (opcode, iword) = (iword % 100, iword / 100);

        let inst = match opcode {
            1 | 2 | 7 | 8 => {
                let (s1, iword) = Operand::decode(stream[1], iword)?;
                let (s2, iword) = Operand::decode(stream[2], iword)?;
                let (d,  _    ) = Operand::decode(stream[3], iword)?;
                let d = if let Operand::Mem(d) = d { d }
                        else { return None; };//panic!("instruction with immediate destination"); };
                use OpXXM::*;
                let op = match opcode {
                    1 => Add,
                    2 => Mul,
                    7 => Les,
                    8 => Equ,
                    _ => unreachable!()
                };
                (XXM(op, s1, s2, d), 4)
            }

            5 | 6 => {
                let (oa1, iword) = Operand::decode(stream[1], iword)?;
                let (oa2, _    ) = Operand::decode(stream[2], iword)?;
                use OpXX::*;
                let op = match opcode {
                    5 => JiT,
                    6 => JiF,
                    _ => unreachable!()
                };
                (XX(op, oa1, oa2), 3)
            }

            3 => {
                let (oa, _) = Operand::decode(stream[1], iword)?;
                let ptr = if let Operand::Mem(ptr) = oa { ptr }
                          else { return None; };//      { panic!("M instruction with immed"); };
                use OpM::*;
                let op = match opcode {
                    3 => Inp,
                    _ => unreachable!()
                };
                (M(op, ptr), 2)
            }

            4 => {
                let (oa, _) = Operand::decode(stream[1], iword)?;
                use OpX::*;
                let op = match opcode {
                    4 => Out,
                    _ => unreachable!()
                };
                (X(op, oa), 2)
            }

            99 => (N(OpN::Halt), 1),

            _ => { return None; }//panic!("Invalid opcode {}", opcode)
        };

        Some(inst)
    }
}

#[derive(Clone, Copy)]
enum VMState {
    Running { ip: usize },
    Stopped
}

struct VM {
    state: VMState,
    mem: Vec<i32>
}

impl VM {
    fn new(mem: Vec<i32>) -> VM {
        VM { state: VMState::Running { ip: 0 }, mem }
    }

    fn step(&mut self) {
        let ip = match self.state {
            VMState::Running { ip } => ip,
            VMState::Stopped        => { return; }
        };

        let (inst, len) = Instruction::decode(&self.mem[ip..]).expect("instruction decode");
        let mut next_ip = ip + len as usize;

        use {Instruction::*, Operand::*};
        match inst {
            XXM(op, s1, s2, d) => {
                let s1 = match s1 { Imm(x) => x, Mem(p) => self.mem[p as usize] };
                let s2 = match s2 { Imm(x) => x, Mem(p) => self.mem[p as usize] };
                use OpXXM::*;
                let result = match op {
                    Add => s1 + s2,
                    Mul => s1 * s2,
                    Les => if s1 <  s2 { 1 } else { 0 },
                    Equ => if s1 == s2 { 1 } else { 0 },
                };
                self.mem[d as usize] = result;
            },

            XX(op, x1, x2) => {
                let x1 = match x1 { Imm(x) => x, Mem(p) => self.mem[p as usize] };
                let x2 = match x2 { Imm(x) => x, Mem(p) => self.mem[p as usize] };
                use OpXX::*;
                match op {
                    JiT => if x1 != 0 { next_ip = x2 as usize; }
                    JiF => if x1 == 0 { next_ip = x2 as usize; }
                }
            }

            X(op, x) => {
                let x = match x { Imm(x) => x, Mem(p) => self.mem[p as usize] };
                use OpX::*;
                match op {
                    Out => println!("{}", x)
                }
            }

            M(op, ptr) => {
                use OpM::*;
                match op {
                    Inp => {
                        let value: i32 = {
                            print!("> ");
                            std::io::stdout().flush().unwrap();
                            let mut buf = String::new();
                            std::io::stdin().read_line(&mut buf).expect("reading stdin");
                            buf.trim().parse().expect("parsing input")
                        };
                        self.mem[ptr as usize] = value;
                    }
                }
            }

            N(op) => {
                use OpN::*;
                match op {
                    Halt => {
                        self.state = VMState::Stopped;
                        return;
                    }
                }
            }
        };

        self.state = VMState::Running { ip: next_ip };
    }

    fn run(mut self) -> Vec<i32> {
        while let VMState::Running{..} = self.state {
            self.step();
        }

        self.mem
    }
}

fn run_vm(mem: Vec<i32>) -> Vec<i32> {
    let vm = VM::new(mem);
    vm.run()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vm() {
        assert_eq!(&run_vm(vec![1,0,0,0,99]), &[2,0,0,0,99]);
        assert_eq!(&run_vm(vec![2,3,0,3,99]), &[2,3,0,6,99]);
        assert_eq!(&run_vm(vec![2,4,4,5,99,0]), &[2,4,4,5,99,9801]);
        assert_eq!(&run_vm(vec![1,1,1,4,99,5,6,0,99]), &[30,1,1,4,2,5,6,0,99]);
    }
}

fn main() {
    let file = std::fs::File::open("input").expect("opening input");
    let input = std::io::BufReader::new(file);
    let line: String = input
        .lines()
        .take(1)
        .next().unwrap()
        .expect("i/o error");

    let mem: Vec<i32> = line
        .split(',')
        .map(|string| {
            string.parse().expect(&format!("parsing input {}", string))
        })
        .collect();

    run_vm(mem);
}

