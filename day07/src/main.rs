
#![feature(vec_remove_item)]

use {
    intcode_rk::{VM, VMResult, run_vm, parse_programs},
};

fn permutations(set: Vec<i64>) -> Vec<Vec<i64>> {
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
    let mem = parse_programs(input).nth(0).expect("loading program");

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

