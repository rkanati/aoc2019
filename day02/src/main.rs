
use std::io::BufRead;

#[derive(Clone, Copy)]
enum VMState {
    Running { ip: usize },
    Stopped
}

struct VM {
    state: VMState,
    mem: Vec<u32>
}

impl VM {
    fn new(mem: Vec<u32>) -> VM {
        VM { state: VMState::Running { ip: 0 }, mem }
    }

    fn step(&mut self) {
        let ip = match self.state {
            VMState::Running { ip } => ip,
            VMState::Stopped        => { return; }
        };

        let inst = &self.mem[ip ..];
        let op = inst[0];

        if op == 99 {
            self.state = VMState::Stopped;
            return;
        }

        let s1 = inst[1] as usize;
        let s2 = inst[2] as usize;
        let d  = inst[3] as usize;

        match op {
            1 => { self.mem[d] = self.mem[s1] + self.mem[s2]; }
            2 => { self.mem[d] = self.mem[s1] * self.mem[s2]; }
            _ => { panic!("Invalid opcode: {}", inst[0]); }
        }

        self.state = VMState::Running { ip: ip + 4 };
    }

    fn run(mut self) -> Vec<u32> {
        while let VMState::Running{..} = self.state {
            self.step();
        }

        self.mem
    }
}

fn run_vm(mem: Vec<u32>) -> Vec<u32> {
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

    let initial_mem: Vec<u32> = line
        .split(',')
        .map(|string| {
            string.parse::<u32>().expect(&format!("parsing input {}", string))
        })
        .collect();

    {   let mut mem = initial_mem.clone();
        mem[1] = 12;
        mem[2] = 2;
        let final_mem = run_vm(mem);
        println!("Location 0 after baseline execution: {}", final_mem[0]);
    }

    let mut needed_inputs = None;

    'noun_loop: for noun in 0 ..= 99 {
        for verb in 0 ..= 99 {
            let mut mem = initial_mem.clone();
            mem[1] = noun as u32;
            mem[2] = verb as u32;
            let final_mem = run_vm(mem);
            let output = final_mem[0];
            if output == 19690720 {
                needed_inputs = Some((noun, verb));
                break 'noun_loop;
            }
        }
    }

    let (noun, verb) = needed_inputs.expect("correct noun/verb combination not found");
    println!("Obtain output 19690720 with noun {}, verb {}", noun, verb);
    println!("(answer: {})", 100 * noun + verb);

}

