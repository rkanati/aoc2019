
use {
    std::convert::{TryFrom, TryInto},
};

/// A pointer into memory.
#[derive(Clone, Copy)]
enum Pointer {
    /// An absolute pointer. The effective address is stored directly.
    Abs(usize),
    /// A relative pointer. The effective address is the current relative base address, plus this
    /// offset.
    Rel(isize)
}

impl Pointer {
    /// Resolves the pointer into an effective address.
    fn resolve(self, rel_base: usize) -> Option<usize> {
        match self {
            Pointer::Abs(address) => Some(address),
            Pointer::Rel(offset) => {
                let rel_base: isize = rel_base.try_into().ok()?;
                let address = rel_base + offset;
                address.try_into().ok()
            }
        }
    }
}

/// An instruction operand.
#[derive(Clone, Copy)]
enum Operand {
    /// An operand stored in memory.
    Mem(Pointer),
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
            0 => Mem(Pointer::Abs(word.try_into().ok()?)),
            1 => Imm(word),
            2 => Mem(Pointer::Rel(word.try_into().ok()?)),
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
    /// XXM-encoded instructions: three operands, the third of which must be a pointer.
    XXM(OpXXM, Operand, Operand, Pointer),
    /// XX-encoded instructions: two operands, with no restrictions.
    XX(OpXX, Operand, Operand),
    /// M-encoded instructions: one operand, which must be a pointer.
    M(OpM, Pointer),
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

        let inst_word = stream[0];
        let (opcode, modes) = (inst_word % 100, inst_word / 100);

        let inst = match opcode {
            1 | 2 | 7 | 8 => {
                let (operand1, modes) = Operand::decode(stream[1], modes)?;
                let (operand2, modes) = Operand::decode(stream[2], modes)?;
                let (dest,     _    ) = Operand::decode(stream[3], modes)?;
                let dest = if let Operand::Mem(ptr) = dest { ptr }
                           else                            { return None; };
                use OpXXM::*;
                let op = match opcode {
                    1 => Add,
                    2 => Mul,
                    7 => Les,
                    8 => Equ,
                    _ => unreachable!()
                };
                (XXM(op, operand1, operand2, dest), 4)
            }

            5 | 6 => {
                let (operand1, modes) = Operand::decode(stream[1], modes)?;
                let (operand2, _    ) = Operand::decode(stream[2], modes)?;
                use OpXX::*;
                let op = match opcode {
                    5 => JIT,
                    6 => JIF,
                    _ => unreachable!()
                };
                (XX(op, operand1, operand2), 3)
            }

            3 => {
                let (operand, _) = Operand::decode(stream[1], modes)?;
                let ptr = if let Operand::Mem(ptr) = operand { ptr }
                          else { eprintln!("M instruction with immediate operand"); return None; };
                use OpM::*;
                let op = match opcode {
                    3 => Inp,
                    _ => unreachable!()
                };
                (M(op, ptr), 2)
            }

            4 | 9 => {
                let (operand, _) = Operand::decode(stream[1], modes)?;
                use OpX::*;
                let op = match opcode {
                    4 => Out,
                    9 => ARB,
                    _ => unreachable!()
                };
                (X(op, operand), 2)
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
    WaitInput { dest: Pointer, resume_ip: u64 },
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

impl VMResult {
    /// If `self` is an `Output`, return the wrapped word. Otherwise, panic with the given
    /// `message`.
    pub fn expect_output(self, message: &'static str) -> i64 {
        match self {
            VMResult::Output(word) => word,
            _                      => { panic!(message); }
        }
    }
}

/// An intcode virtual machine.
#[derive(Clone)]
pub struct VM {
    state:    VMState,
    rel_base: usize,
    memory:   Vec<i64>
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
            *self.mem(dest) = input;
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

    /// Returns a mutable reference to the memory word pointed to by `ptr`.
    fn mem(&mut self, ptr: Pointer) -> &mut i64 {
        let addr = ptr.resolve(self.rel_base).expect("resolving pointer");
        if addr >= self.memory.len() {
            self.memory.resize(addr + 1, 0);
        }
        &mut self.memory[addr]
    }

    /// Evaluate the given operand, loading from memory if necessary.
    fn read_operand(&mut self, operand: Operand) -> i64 {
        match operand {
            Operand::Imm(value) => value,
            Operand::Mem(ptr)   => *self.mem(ptr)
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

        let ip = ip + len;
        self.state = VMState::Running { ip };

        use {Instruction::*};
        match inst {
            XXM(op, src1, src2, dest) => {
                let src1 = self.read_operand(src1);
                let src2 = self.read_operand(src2);
                use OpXXM::*;
                let result = match op {
                    Add => src1 + src2,
                    Mul => src1 * src2,
                    Les => if src1 <  src2 { 1 } else { 0 },
                    Equ => if src1 == src2 { 1 } else { 0 },
                };
                *self.mem(dest) = result;
            },

            XX(op, operand1, operand2) => {
                let operand1 = self.read_operand(operand1);
                let operand2 = self.read_operand(operand2);
                use OpXX::*;
                let ip = match op {
                    JIT if operand1 != 0 => { operand2 as u64 }
                    JIF if operand1 == 0 => { operand2 as u64 }
                    _                    => { ip }
                };
                self.state = VMState::Running { ip };
            }

            X(op, operand) => {
                let operand = self.read_operand(operand);
                use OpX::*;
                match op {
                    Out => { return Some(operand); }
                    ARB => {
                        self.rel_base = isize::try_from(self.rel_base)
                            .and_then(|rel_base| usize::try_from(rel_base + operand as isize))
                            .expect("rel_base out of range");
                    }
                }
            }

            M(op, ptr) => {
                use OpM::*;
                match op {
                    Inp => {
                        self.state = VMState::WaitInput { dest: ptr, resume_ip: ip };
                    }
                }
            }

            N(op) => {
                use OpN::*;
                match op {
                    Halt => {
                        self.state = VMState::Stopped;
                    }
                }
            }
        };

        None
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

