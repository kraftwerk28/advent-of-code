use std::collections::HashSet;

fn p2() {
    let mut sets: Vec<Vec<&str>> = vec![vec![]];
    let lines = utils::parse(Some);
    for l in lines.iter() {
        if l.is_empty() {
            sets.push(vec![]);
            continue;
        }
        let last = sets.last_mut().unwrap();
        last.push(l);
    }

    let ans = sets
        .iter()
        .map(|v| {
            v.first()
                .unwrap()
                .chars()
                .filter(|c| v.iter().all(|cc| cc.chars().any(|ccc| ccc == *c)))
                .count()
        })
        .sum::<usize>();
    println!("Part 1: {}", ans);
}

fn p1() {
    let mut sets: Vec<HashSet<char>> = vec![HashSet::new()];
    for l in utils::parse(Some).iter() {
        if l.is_empty() {
            sets.push(HashSet::new());
            continue;
        }
        let last = sets.last_mut().unwrap();
        for c in l.chars() {
            last.insert(c);
        }
    }
    println!("Part 2: {:?}", sets.iter().map(HashSet::len).sum::<usize>());
}

fn main() {
    p1();
    p2();
}
