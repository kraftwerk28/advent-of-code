use regex::Regex;
use std::collections::HashMap;

const REQUIRED: &[&str] = &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

#[derive(Debug)]
struct Pass(HashMap<String, String>);
impl Pass {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn is_valid(&self) -> bool {
        REQUIRED.iter().all(|fd| self.0.keys().any(|k| k == fd))
    }
    fn add(&mut self, tag: &str, val: &str) {
        self.0.insert(tag.into(), val.into());
    }
}

fn p1(passports: &Vec<Pass>) {
    let ans = passports.iter().filter(|p| p.is_valid()).count();
    println!("{}", ans);
}

type Validator<'a> = (&'a str, &'a dyn Fn(&str) -> bool);

fn p2(passports: &Vec<Pass>, validators: &[Validator]) {
    let ans = passports
        .iter()
        .filter(|p| p.is_valid())
        .filter(|p| {
            p.0.iter()
                .filter(|(fname, _)| REQUIRED.contains(&fname.as_str()))
                .all(|(fname, fvalue)| {
                    validators
                        .iter()
                        .find(|(pname, _)| *pname == fname)
                        .map(|(_, val)| val(fvalue))
                        .unwrap_or(false)
                })
        })
        .count();

    println!("{}", ans);
}

fn main() {
    let entity_re = Regex::new(r"(\w{3}):([^\s]+)").unwrap();
    let hgt_re = Regex::new(r"^(\d+)(cm|in)$").unwrap();
    let clr_re = Regex::new(r"^#[a-f0-9]{6}$").unwrap();
    let eye_colors = &["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
    let pass_re = Regex::new(r"^[0-9]{9}$").unwrap();

    let validators: &[Validator] = &[
        ("byr", &|s| {
            s.parse::<i32>()
                .map(|n| (1920..=2002).contains(&n))
                .unwrap_or(false)
        }),
        ("iyr", &|s| {
            s.parse::<i32>()
                .map(|n| (2010..=2020).contains(&n))
                .unwrap_or(false)
        }),
        ("eyr", &|s| {
            s.parse::<i32>()
                .map(|n| (2020..=2030).contains(&n))
                .unwrap_or(false)
        }),
        ("hgt", &|s| {
            hgt_re
                .captures(s)
                .map(|cap| {
                    let unit = &cap[2];
                    let val = cap[1].parse::<i32>().unwrap();
                    match unit {
                        "cm" => (150..=193).contains(&val),
                        "in" => (59..=76).contains(&val),
                        _ => unreachable!(),
                    }
                })
                .unwrap_or(false)
        }),
        ("hcl", &|s| clr_re.is_match(s)),
        ("ecl", &|s| eye_colors.contains(&s)),
        ("pid", &|s| pass_re.is_match(s)),
    ];

    let passports = utils::parse(Some)
        .iter()
        .fold::<Vec<Pass>, _>(vec![], |mut acc, line| {
            if line.trim().is_empty() {
                acc.push(Pass::new());
                return acc;
            }
            if acc.is_empty() {
                acc.push(Pass::new());
            }
            let last = acc.last_mut().unwrap();
            entity_re.captures_iter(line).for_each(|cap| {
                last.add(&(cap[1]), &(cap[2]));
            });
            acc
        });

    p1(&passports);
    p2(&passports, validators);
}
