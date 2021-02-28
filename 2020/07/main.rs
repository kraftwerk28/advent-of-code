use std::collections::HashMap;

use regex::Regex;
#[derive(Default, Debug)]
struct BagDesc(HashMap<String, Vec<(i32, String)>>);

impl BagDesc {
    fn parse() -> Self {
        let r1 = Regex::new(r"^([\w ]+) bags contain").unwrap();
        let r2 = Regex::new(r"(\d+) ([\w ]+) bags?").unwrap();
        utils::parse(Some)
            .iter()
            .fold(BagDesc::default(), |mut acc, l| {
                if let Some(bagname) = r1.captures(l) {
                    let bagnames = r2
                        .captures_iter(l)
                        .map(|cap| {
                            (cap[1].parse::<i32>().unwrap(), cap[2].to_string())
                        })
                        .collect();
                    acc.0.insert(bagname[1].to_string(), bagnames);
                }
                acc
            })
    }
    fn has(&self, target: &str, bag: &str) -> bool {
        self.0
            .get(bag)
            .unwrap_or(&vec![])
            .iter()
            .any(|(_, v)| v == target || self.has(target, v))
    }
    fn countprice(&self, bag: &str) -> i32 {
        self.0
            .get(bag)
            .unwrap()
            .iter()
            .map(|(c, v)| c + c * self.countprice(v))
            .sum::<i32>()
    }
}

fn main() {
    let bags = BagDesc::parse();
    let my_bag = "shiny gold";
    let ans1 = bags.0.keys().filter(|k| bags.has(my_bag, k)).count();
    let ans2 = bags.countprice(my_bag);
    println!("p1 = {}; p2 = {}", ans1, ans2);
}
