fn s1(numbers: &Vec<i32>) {
    for n in numbers.iter() {
        for n2 in numbers.iter() {
            if n + n2 == 2020 {
                println!("{} + {} = {}; {}", n, n2, n + n2, n * n2);
                return;
            }
        }
    }
}

fn s2(numbers: &Vec<i32>) {
    for n in numbers.iter() {
        for n2 in numbers.iter() {
            for n3 in numbers.iter() {
                let s = n + n2 + n3;
                if s == 2020 {
                    let m = n * n2 * n3;
                    println!("{} + {} + {} = {}; {}", n, n2, n3, s, m);
                    return;
                }
            }
        }
    }
}
fn main() {
    let numbers = utils::parse("input.txt", |s| s.parse::<i32>().ok());
    s1(&numbers);
    s2(&numbers);
}
