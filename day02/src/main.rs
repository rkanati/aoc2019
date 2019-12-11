
use {
    intcode_rk::{VM, VMResult, parse_programs},
};

fn run_and_dump(mem: Vec<i64>) -> Vec<i64> {
    let mut vm = VM::new(mem);
    if let VMResult::Stopped = vm.run() {
        vm.dump_memory()
    }
    else {
        panic!("unexpected vm behaviour")
    }
}

fn main() {
    let input = include_str!("../input");
    let initial_mem = parse_programs(input).nth(0).expect("loading program");

    {   let mut mem = initial_mem.clone();
        mem[1] = 12;
        mem[2] = 2;
        let final_mem = run_and_dump(mem);
        println!("Location 0 after baseline execution: {}", final_mem[0]);
    }

    let mut needed_inputs = None;

    'noun_loop: for noun in 0 ..= 99 {
        for verb in 0 ..= 99 {
            let mut mem = initial_mem.clone();
            mem[1] = noun as i64;
            mem[2] = verb as i64;
            let final_mem = run_and_dump(mem);
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

