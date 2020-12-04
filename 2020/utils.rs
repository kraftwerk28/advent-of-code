use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
};

pub fn parse<T, F>(f: F) -> Vec<T>
where
    F: Fn(String) -> Option<T>,
{
    env::args()
        .skip(1)
        .next()
        .map(|fpath| File::open(&fpath).expect(format!("Failed to open {}", fpath).as_str()))
        .map(BufReader::new)
        .map(BufReader::lines)
        .map(|lines| {
            lines
                .filter(Result::is_ok)
                .map(Result::unwrap)
                .map(f)
                .filter(Option::is_some)
                .map(Option::unwrap)
                .collect::<Vec<_>>()
        })
        .expect("Usage <executable> <path-to-input")
}
