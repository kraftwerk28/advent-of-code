use std::io::{stdin, BufRead};

pub fn parse<T, F>(f: F) -> Vec<T>
where
    F: FnMut(String) -> Option<T>,
{
    stdin()
        .lock()
        .lines()
        .filter_map(Result::ok)
        .filter_map(f)
        .collect()
}
