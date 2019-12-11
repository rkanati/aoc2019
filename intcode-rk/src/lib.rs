
/// A memory reference, i.e. a pointer.
#[derive(Clone, Copy)]
enum MemRef {
    /// An absolute reference. The effective address is stored directly.
    Abs(u64),
    /// A relative reference. The effective address is the current relative base address, plus this
    /// offset.
    Rel(i64)
}

/// An instruction operand.
#[derive(Clone, Copy)]
enum Operand {
    /// An operand stored in memory.
    Mem(MemRef),
    /// An immediate operand.
    Imm(i64)
}

impl Operand {
    /// Decodes an operand from the corresponding word in the instruction stream, and from a word
    /// containing the parameter mode digits from the instruction word.
    ///
    /// # Arguments
    ///
    /// * `word`  - The operand word, exracted left-to-right from the instruction stream.
    /// * `modes` - The the instruction word, with the opcode digits divided out. Contains parameter
    ///             mode digits right-to-left.
    ///
    /// # Returns
    ///
    /// If decoding fails, returns `None`. Otherwise, returns `Some(operand, remaining_modes)`,
    /// where
    ///
    /// * `operand`         - The decoded operand.
    /// * `remaining_modes` - A word containing the mode digits for any remaining operands.
    fn decode<'a>(word: i64, modes: i64) -> Option<(Operand, i64)> {
        use Operand::*;
        let mode = modes % 10;
        let operand = match mode {
            0 => Mem(MemRef::Abs(word as u64)),
            1 => Imm(word),
            2 => Mem(MemRef::Rel(word)),
            _ => { return None; }
        };

        Some((operand, modes / 10))
    }
}

/// Opcodes for instructions with the `XXM` encoding
#[derive(Clone, Copy)]
enum OpXXM {
    Add, // ADD
    Mul, // MULtiply
    Les, // LESs than
    Equ, // EQUal to
}

/// Opcodes for instructions with the `XX` encoding
#[derive(Clone, Copy)]
enum OpXX {
    JIT, // Jump If True
    JIF, // Jump If False
}

/// Opcodes for instructions with the `M` encoding
#[derive(Clone, Copy)]
enum OpM {
    Inp, // INPut
}

/// Opcodes for instructions with the `X` encoding
#[derive(Clone, Copy)]
enum OpX {
    Out, // OUTput
    ARB, // Adjust Relative Base
}

/// Opcodes for instructions with the `N` encoding
#[derive(Clone, Copy)]
enum OpN {
    Halt, // HALT machine
}

/// An intcode instruction, distinguished by encoding. Each variant carries a tuple of an opcode
/// followed by any operands.
#[derive(Clone, Copy)]
enum Instruction {
    /// XXM-encoded instructions: three operands, the third of which must be a memory reference.
    XXM(OpXXM, Operand, Operand, MemRef),
    /// XX-encoded instructions: two operands, with no restrictions.
    XX(OpXX, Operand, Operand),
    /// M-encoded instructions: one operand, which must be a memory reference.
    M(OpM, MemRef),
    /// X-encoded instructions: one operand of any kind.
    X(OpX, Operand),
    /// N-encoded instructions: no operands.
    N(OpN)
}

impl Instruction {
    /// Decodes an instruction from an instruction stream.
    ///
    /// # Arguments
    ///
    /// * `stream` - A slice of an instruction stream. Decoding proceeds from the beginning of this
    ///              slice.
    ///
    /// # Returns
    ///
    /// If decoding fails, returns `None`. Otherwise, returns `Some(instruction, length)`,
    /// where
    ///
    /// * `instruction` - The decoded instruction.
    /// * `length`      - The length of the instruction in words.
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
                    5 => JIT,
                    6 => JIF,
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

/// Internal state of the VM
#[derive(Clone, Copy)]
enum VMState {
    /// Executing may proceed normally from `ip`.
    Running { ip: u64 },
    /// Waiting for input. `feed_input` will store its input via `dest`, and execution will resume
    /// normally at `resume_ip`.
    WaitInput { dest: MemRef, resume_ip: u64 },
    /// A `Halt` instruction was executed. Execution may not be resumed.
    Stopped
}

/// An intermediate result of advancing execution, returned by `run`. Clients may need to react to
/// these before resuming.
#[derive(Clone, Copy)]
pub enum VMResult {
    /// A `Halt` instruction was executed. Execution may not be resumed.
    Stopped,
    /// The VM is waiting for input. This must be supplied with `feed_input` before resuming
    /// exection.
    WaitInput,
    /// The VM output a word. The client can use this as necessary, and resume execution.
    Output(i64)
}

/// An intcode virtual machine.
#[derive(Clone)]
pub struct VM {
    state: VMState,
    rel_base: i64,
    memory: Vec<i64>
}

impl VM {
    /// Create a new VM.
    ///
    /// # Arguments
    ///
    /// * `memory` - Initial memory contents, starting from address `0`. Usually contains an intcode
    /// program.
    pub fn new(memory: Vec<i64>) -> VM {
        VM { state: VMState::Running { ip: 0 }, rel_base: 0, memory }
    }

    /// Feed the VM an input word.
    ///
    /// # Panics
    ///
    /// Panics if the VM is not waiting for input. Thus, `feed_input` must only be called in respose
    /// to `run` returning `VMState::WaitInput`.
    pub fn feed_input(&mut self, input: i64) {
        if let VMState::WaitInput { dest, resume_ip } = self.state {
            *self.mem_mut(dest) = input;
            self.state = VMState::Running { ip: resume_ip };
        }
        else {
            panic!("not ready for input");
        }
    }

    /// Run the VM. Will execute until client intervention is required.
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

    /// Is the VM waiting for input?
    ///
    /// This is probably not useful at all.
    pub fn waiting(&self) -> bool {
        match self.state {
            VMState::WaitInput {..} => true,
            _                       => false
        }
    }

    /// Consumes the VM, and returns the contents of its memory.
    pub fn dump_memory(self) -> Vec<i64> {
        self.memory
    }

    /// Reads the memory word at the given address.
    fn mem(&mut self, addr: u64) -> i64 {
        let addr = addr as usize;
        if addr >= self.memory.len() {
            self.memory.resize(addr + 1, 0);
        }
        self.memory[addr]
    }

    /// Returns a mutable reference to the memory word via the given memory reference.
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

    /// Evaluate the given operand, loading from memory if necessary.
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

    /// Execute one instruction. Returns `Some(word)` if there is output, `None` otherwise.
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
                    JIT => if x1 != 0 { next_ip = x2 as u64; }
                    JIF => if x1 == 0 { next_ip = x2 as u64; }
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

/// Run a VM with the given initial memory contents and input to completion, and return the any
/// output it made.
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

/// Parse intcode programs, one per line, from a string.
pub fn parse_programs<'a> (src: &'a str) -> impl Iterator<Item = Vec<i64>> + 'a {
    src.lines()
        .map(|line| {
            let mem: Vec<i64> = line.split(',')
                .map(|digits| digits.parse::<i64>().expect("parsing intcode program"))
                .collect();
            mem
        })
}

