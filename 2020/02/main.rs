use regex::Regex;

struct Rule((usize, usize), char, String);

fn p1(rules: &Vec<Rule>) {
    let ans = rules
        .iter()
        .filter(|&rule| {
            let ccount = rule.2.chars().filter(|&c| c == rule.1).count();
            ((rule.0).0..=(rule.0).1).contains(&ccount)
        })
        .count();
    println!("{}", ans);
}

fn p2(rules: &Vec<Rule>) {
    let ans = rules
        .iter()
        .filter(|&rule| {
            let ch = rule.2.chars().collect::<Vec<_>>();
            let (a, b) = (ch[(rule.0).0 - 1] == rule.1, ch[(rule.0).1 - 1] == rule.1);
            a ^ b
        })
        .count();
    println!("{}", ans);
}

fn main() {
    let re = Regex::new(r"^(\d+)-(\d+)\s+(\w):\s+(.+)$").unwrap();
    let rules = utils::parse(|s| {
        re.captures(s.as_str()).map(|c| {
            let r = (
                c[1].parse::<usize>().unwrap(),
                c[2].parse::<usize>().unwrap(),
            );
            Rule(r, c[3].parse::<char>().unwrap(), c[4].to_string())
        })
    });

    p1(&rules);
    p2(&rules);
}
