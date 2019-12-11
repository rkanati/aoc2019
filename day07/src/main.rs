
#![feature(vec_remove_item)]

use {
    std::{
        io::{Write},
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
    WaitInput { dest: usize, resume_ip: usize },
    Stopped
}

#[derive(Clone, Copy)]
enum VMResult {
    Stopped,
    WaitInput,
    Output(i32)
}

#[derive(Clone)]
struct VM {
    state: VMState,
    mem: Vec<i32>
}

impl VM {
    fn new(mem: Vec<i32>) -> VM {
        VM { state: VMState::Running { ip: 0 }, mem }
    }

    fn feed_input(&mut self, input: i32) {
        if let VMState::WaitInput { dest, resume_ip } = self.state {
            self.mem[dest] = input;
            self.state = VMState::Running { ip: resume_ip };
        }
        else {
            panic!("not ready for input");
        }
    }

    fn step(&mut self) -> Option<i32> {
        let ip = match self.state {
            VMState::Running   { ip } => ip,
            VMState::WaitInput { .. } => panic!("input not supplied"),
            VMState::Stopped          => { return None; }
        };

        let (inst, len) = Instruction::decode(&self.mem[ip..]).expect("instruction decode");
        let mut next_ip = ip + len as usize;
        let mut output = None;

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
                    Out => { output = Some(x); }
                }
            }

            M(op, ptr) => {
                use OpM::*;
                match op {
                    Inp => {
                        self.state = VMState::WaitInput { dest: ptr as usize, resume_ip: next_ip };
                        return None;
                    }
                }
            }

            N(op) => {
                use OpN::*;
                match op {
                    Halt => {
                        self.state = VMState::Stopped;
                        return None;
                    }
                }
            }
        };

        self.state = VMState::Running { ip: next_ip };
        output
    }

    fn run(&mut self) -> VMResult {
        loop {
            if let Some(output) = self.step() {
                return VMResult::Output(output);
            }

            match self.state {
                VMState::Running   {..} => { continue; }
                VMState::WaitInput {..} => { return VMResult::WaitInput; }
                VMState::Stopped        => { return VMResult::Stopped; }
            }
        }
    }

    fn waiting(&self) -> bool {
        match self.state {
            VMState::WaitInput {..} => true,
            _                       => false
        }
    }
}

fn run_vm(mem: Vec<i32>, input: impl IntoIterator<Item = i32>) -> Vec<i32> {
    let mut input = input.into_iter();
    let mut vm = VM::new(mem);
    let mut output = Vec::new();

    loop {
        match vm.run() {
            VMResult::Output(out) => { output.push(out); }
            VMResult::WaitInput   => { vm.feed_input(input.next().unwrap()); }
            VMResult::Stopped     => { break; }
        }
    }

    output
}

fn permutations(mut set: Vec<i32>) -> Vec<Vec<i32>> {
    if set.len() == 1 {
        return set.iter().map(|item| vec![*item]).collect();
    }

    set.iter().flat_map(|head| {
        let mut rest = set.clone();
        rest.remove_item(head);
        let mut perms = permutations(rest);
        perms.iter_mut().for_each(|tail| tail.insert(0, *head));
        perms.into_iter()
    })
    .collect()
}

fn main() {
    let input = include_str!("../input");
    let line = input
        .lines()
        .take(1)
        .next().unwrap();

    let mem: Vec<i32> = line
        .split(',')
        .map(|string|
            string.parse().expect(&format!("parsing input {}", string))
        )
        .collect();

    // part 1
    {   let (best_settings, best_amplitude) = permutations((0 ..= 4).collect())
            .iter()
            .map(|settings| {
                let output = settings.iter()
                    .fold(0, |input, setting|
                        *run_vm(mem.clone(), vec![*setting, input]).first().expect("no output")
                    );
                (settings.clone(), output)
            })
            .max_by_key(|(_, amplitude)| *amplitude)
            .expect("no best");

        println!("Part 1: Best settings: {:?}, amplitude {}", best_settings, best_amplitude);
    }

    // part 2
    {   let best_amplitude = permutations((5 ..= 9).collect())
            .iter()
            .map(|settings| {
                let mut stages: Vec<VM> = std::iter::repeat(VM::new(mem.clone()))
                    .take(5)
                    .collect();

                for (stage, phase) in stages.iter_mut().zip(settings.iter()) {
                    stage.run();
                    stage.feed_input(*phase);
                }

                let mut prev_out = 0;
                'feedback_loop: loop {
                    'stage_loop: for stage in stages.iter_mut() {
                        loop {
                            match stage.run() {
                                VMResult::Output(out) => { prev_out = out; continue 'stage_loop; }
                                VMResult::WaitInput   => { stage.feed_input(prev_out); }
                                VMResult::Stopped     => { break 'feedback_loop; }
                            }
                        }
                    }
                }

                prev_out
            })
            .max()
            .expect("no best");

        println!("Part 2: Best amplitude {}", best_amplitude);
    }
}

