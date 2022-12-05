use std::collections::HashSet;

#[derive(Copy, Clone, Debug)]
enum Instr {
    Nop(i32),
    Acc(i32),
    Jmp(i32),
}
use Instr::*;

impl Instr {
    fn parse(line: String) -> Option<Self> {
        if line.is_empty() {
            return None;
        }
        let vals = line.splitn(2, " ").collect::<Vec<_>>();
        vals[1].parse::<i32>().ok().and_then(|a| match vals[0] {
            "nop" => Some(Nop(a)),
            "acc" => Some(Acc(a)),
            "jmp" => Some(Jmp(a)),
            _ => None,
        })
    }
}

fn eval(instructions: &Vec<Instr>, eip: Option<i32>) -> (i32, bool) {
    let mut cache = HashSet::<i32>::new();
    let mut eip = eip.unwrap_or(0);
    let mut acc = 0;
    while (eip as usize) < instructions.len() && !cache.contains(&eip) {
        cache.insert(eip);
        match instructions[eip as usize] {
            Jmp(v) => {
                eip += v;
            }
            Acc(v) => {
                acc += v;
                eip += 1;
            }
            _ => eip += 1,
        }
    }
    (acc, (eip as usize) >= instructions.len())
}

fn p1(instructions: &Vec<Instr>) {
    let acc = eval(instructions, None);
    println!("Part 1: {}", acc.0);
}

fn p2(instructions: &Vec<Instr>) {
    let mut acc = 0i32;
    let mut eip = 0i32;
    loop {
        match instructions[eip as usize] {
            Jmp(v) => {
                let mut inss = instructions.clone();
                inss[eip as usize] = Nop(v);
                let (_acc, finished) = eval(&inss, Some(eip));
                if finished {
                    acc += _acc;
                    break;
                }
                eip += v;
            }
            Nop(v) => {
                let mut inss = instructions.clone();
                inss[eip as usize] = Jmp(v);
                let (_acc, finished) = eval(&inss, Some(eip));
                if finished {
                    acc += _acc;
                    break;
                }
                eip += 1;
            }
            Acc(v) => {
                eip += 1;
                acc += v;
            }
        }
    }

    // println!("eip: {}; acc: {}", eip, acc);
    println!("Part 2: {}", acc);
}

fn main() {
    let instructions = utils::parse(Instr::parse);
    p1(&instructions);
    p2(&instructions);
}
