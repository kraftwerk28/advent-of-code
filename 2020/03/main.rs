#[derive(PartialEq)]
enum Tile {
    Tree,
    Grass,
}

type Forest = Vec<Vec<Tile>>;

fn p1(forest: &Forest) {
    let mut pos: (usize, usize) = (0, 0);
    let mut ans = 0;
    let forest_w = forest[0].len();
    while pos.1 < forest.len() - 1 {
        pos = ((pos.0 + 3) % forest_w, pos.1 + 1);
        ans += match forest[pos.1][pos.0] {
            Tile::Tree => 1,
            _ => 0,
        };
    }
    println!("{}", ans);
}

fn count_slopes(forest: &Forest, dir: (usize, usize)) -> usize {
    let mut pos = (0, 0);
    let mut ans = 0;
    let forest_w = forest[0].len();
    while pos.1 < forest.len() - 1 {
        pos = ((pos.0 + dir.0) % forest_w, pos.1 + dir.1);
        let tile = &forest[pos.1][pos.0];
        ans += match tile {
            Tile::Tree => 1,
            _ => 0,
        };
    }
    ans
}

fn p2(forest: &Forest) {
    let dirs: Vec<(usize, usize)> = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    let ans = dirs
        .iter()
        .map(|dir| count_slopes(&forest, *dir))
        .fold(1, |acc, c| acc * c);
    println!("{}", ans);
}

fn main() {
    let forest: Forest = utils::parse(|s| {
        let ss = s
            .chars()
            .map(|c| match c {
                '#' => Tile::Tree,
                '.' => Tile::Grass,
                char => panic!("{}", char),
            })
            .collect::<Vec<_>>();
        Some(ss)
    });
    p1(&forest);
    p2(&forest);
}
