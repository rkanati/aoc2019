
use {
    std::io::Write,
    intcode_rk::{VM, VMResult, parse_programs},
};

fn main() {
    let input = include_str!("../input");
    let mem = parse_programs(input).nth(0).expect("loading program");

    let mut vm = VM::new(mem);
    loop {
        match vm.run() {
            VMResult::Output(out) => { println!("{}", out); }
            VMResult::WaitInput => {
                print!("> ");
                std::io::stdout().flush().unwrap();
                let mut buf = String::new();
                std::io::stdin().read_line(&mut buf).expect("reading stdin");
                let value: i64 = buf.trim().parse().expect("parsing input");
                vm.feed_input(value);
            }
            VMResult::Stopped => { break; }
        }
    }
}

