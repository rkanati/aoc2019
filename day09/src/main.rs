
use {
    intcode_rk::{run_vm, parse_programs},
};

fn main() {
    let input = include_str!("../input");
    let mem = parse_programs(input).nth(0).expect("loading program");

    let output = run_vm(mem.clone(), vec![1i64]);
    println!("Part 1:");
    for out in output.iter() {
        println!("{}", out);
    }
    println!("");

    println!("Part 2:");
    let output = run_vm(mem.clone(), vec![2i64]);
    for out in output.iter() {
        println!("{}", out);
    }
}

