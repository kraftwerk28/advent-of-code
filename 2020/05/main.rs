const RANGE_VERT: (u32, u32) = (0, 128);
const RANGE_HOR: (u32, u32) = (0, 8);

#[derive(Debug)]
struct Seat(u32, u32);

impl Seat {
    fn parse(code: &str) -> Self {
        let (r, c) = code.split_at(7);
        let row = get_seat(r, RANGE_VERT);
        let col = get_seat(c, RANGE_HOR);
        Self(row, col)
    }
    fn idx(&self) -> u32 {
        self.0 * 8 + self.1
    }
}

fn get_seat(code: &str, (l, h): (u32, u32)) -> u32 {
    if h - l <= 1 {
        return l;
    }
    let mid = (h + l) / 2;
    match code.chars().next().unwrap() {
        'F' | 'L' => get_seat(&code[1..], (l, mid)),
        'B' | 'R' => get_seat(&code[1..], (mid, h)),
        _ => unreachable!(),
    }
}

fn p1(seats: &Vec<Seat>) {
    println!("Part 1: {}", seats.iter().map(Seat::idx).max().unwrap());
}

fn p2(seats: &Vec<Seat>) {
    let mut idxs = seats
        .iter()
        .map(Seat::idx)
        .filter(|idx| !(0..8).contains(idx) && !(1016..1024).contains(idx))
        .collect::<Vec<_>>();
    idxs.sort();
    let mut st = *idxs.first().unwrap();
    for &idx in idxs.iter() {
        if idx - st > 1 {
            break;
        }
        st = idx;
    }
    println!("Part 2: {}", st + 1);
}

fn main() {
    let seats = utils::parse(|s| Some(Seat::parse(s.as_str())));
    p1(&seats);
    p2(&seats);
}
