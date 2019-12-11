
#[derive(Clone, Copy)]
enum MemRef {
    Abs(u64),
    Rel(i64)
}

#[derive(Clone, Copy)]
enum Operand {
    Mem(MemRef),
    Imm(i64)
}

impl Operand {
    fn decode<'a>(word: i64, iword: i64) -> Option<(Operand, i64)> {
        use Operand::*;
        let mode = iword % 10;
        let operand = match mode {
            0 => Mem(MemRef::Abs(word as u64)),
            1 => Imm(word),
            2 => Mem(MemRef::Rel(word)),
            _ => { eprintln!("bad parameter mode {}", mode); return None; }
        };

        Some((operand, iword / 10))
    }
}

#[derive(Clone, Copy)]
enum OpXXM {
    Add, // ADD
    Mul, // MULtiply
    Les, // LESs than
    Equ, // EQUal to
}

#[derive(Clone, Copy)]
enum OpXX {
    JiT, // Jump If True
    JiF, // Jump If False
}

#[derive(Clone, Copy)]
enum OpM {
    Inp, // INPut
}

#[derive(Clone, Copy)]
enum OpX {
    Out, // OUTput
    ARB, // Adjust Relative Base
}

#[derive(Clone, Copy)]
enum OpN {
    Halt, // HALT machine
}

#[derive(Clone, Copy)]
enum Instruction {
    XXM(OpXXM, Operand, Operand, MemRef),
    XX(OpXX, Operand, Operand),
    M(OpM, MemRef),
    X(OpX, Operand),
    N(OpN)
}

impl Instruction {
    fn decode(stream: &[i64]) -> Option<(Instruction, u64)> {
        use Instruction::*;

        let iword = stream[0];
        let (opcode, iword) = (iword % 100, iword / 100);

        let inst = match opcode {
            1 | 2 | 7 | 8 => {
                let (s1, iword) = Operand::decode(stream[1], iword)?;
                let (s2, iword) = Operand::decode(stream[2], iword)?;
                let (d,  _    ) = Operand::decode(stream[3], iword)?;
                let d = if let Operand::Mem(d) = d { d }
                        else { eprintln!("instruction with immediate destination"); return None; };
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
                          else { eprintln!("M instruction with immediate operand"); return None; };
                use OpM::*;
                let op = match opcode {
                    3 => Inp,
                    _ => unreachable!()
                };
                (M(op, ptr), 2)
            }

            4 | 9 => {
                let (oa, _) = Operand::decode(stream[1], iword)?;
                use OpX::*;
                let op = match opcode {
                    4 => Out,
                    9 => ARB,
                    _ => unreachable!()
                };
                (X(op, oa), 2)
            }

            99 => (N(OpN::Halt), 1),

            _ => { eprintln!("invalid opcode {}", opcode); return None; }
        };

        Some(inst)
    }
}

#[derive(Clone, Copy)]
enum VMState {
    Running { ip: u64 },
    WaitInput { dest: MemRef, resume_ip: u64 },
    Stopped
}

#[derive(Clone, Copy)]
pub enum VMResult {
    Stopped,
    WaitInput,
    Output(i64)
}

#[derive(Clone)]
pub struct VM {
    state: VMState,
    rel_base: i64,
    memory: Vec<i64>
}

impl VM {
    pub fn new(memory: Vec<i64>) -> VM {
        VM { state: VMState::Running { ip: 0 }, rel_base: 0, memory }
    }

    pub fn feed_input(&mut self, input: i64) {
        if let VMState::WaitInput { dest, resume_ip } = self.state {
            *self.mem_mut(dest) = input;
            self.state = VMState::Running { ip: resume_ip };
        }
        else {
            panic!("not ready for input");
        }
    }

    pub fn run(&mut self) -> VMResult {
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

    pub fn waiting(&self) -> bool {
        match self.state {
            VMState::WaitInput {..} => true,
            _                       => false
        }
    }

    pub fn dump_memory(&self) -> Vec<i64> {
        self.memory.clone()
    }

    fn mem(&mut self, addr: u64) -> i64 {
        let addr = addr as usize;
        if addr >= self.memory.len() {
            self.memory.resize(addr + 1, 0);
        }
        self.memory[addr]
    }

    fn mem_mut(&mut self, ptr: MemRef) -> &mut i64 {
        use MemRef::*;
        let addr = match ptr {
            Abs(p) => p as usize,
            Rel(o) => (self.rel_base + o) as usize,
        };

        if addr >= self.memory.len() {
            self.memory.resize(addr + 1, 0);
        }

        &mut self.memory[addr]
    }

    fn read_operand(&mut self, oa: Operand) -> i64 {
        use {Operand::*, MemRef::*};
        match oa {
            Imm(x) => x,
            Mem(r) => match r {
                Abs(p) => self.mem(p),
                Rel(o) => self.mem((self.rel_base + o) as u64)
            }
        }
    }

    fn step(&mut self) -> Option<i64> {
        let ip = match self.state {
            VMState::Running   { ip } => ip,
            VMState::WaitInput { .. } => panic!("input not supplied"),
            VMState::Stopped          => { return None; }
        };

        let (inst, len) = Instruction::decode(&self.memory[ip as usize..])
            .expect("instruction decode");
        let mut next_ip = ip + len;
        let mut output = None;

        use {Instruction::*};
        match inst {
            XXM(op, s1, s2, d) => {
                let s1 = self.read_operand(s1);
                let s2 = self.read_operand(s2);
                use OpXXM::*;
                let result = match op {
                    Add => s1 + s2,
                    Mul => s1 * s2,
                    Les => if s1 <  s2 { 1 } else { 0 },
                    Equ => if s1 == s2 { 1 } else { 0 },
                };
                *self.mem_mut(d) = result;
            },

            XX(op, x1, x2) => {
                let x1 = self.read_operand(x1);
                let x2 = self.read_operand(x2);
                use OpXX::*;
                match op {
                    JiT => if x1 != 0 { next_ip = x2 as u64; }
                    JiF => if x1 == 0 { next_ip = x2 as u64; }
                }
            }

            X(op, x) => {
                let x = self.read_operand(x);
                use OpX::*;
                match op {
                    Out => { output = Some(x); }
                    ARB => { self.rel_base += x; }
                }
            }

            M(op, ptr) => {
                use OpM::*;
                match op {
                    Inp => {
                        self.state = VMState::WaitInput { dest: ptr, resume_ip: next_ip };
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
}

pub fn run_vm(mem: Vec<i64>, input: impl IntoIterator<Item = i64>) -> Vec<i64> {
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

pub fn parse_programs<'a> (src: &'a str) -> impl Iterator<Item = Vec<i64>> + 'a {
    src.lines()
        .map(|line| {
            let mem: Vec<i64> = line.split(',')
                .map(|digits| digits.parse::<i64>().expect("parsing intcode program"))
                .collect();
            mem
        })
}

