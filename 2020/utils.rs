use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub fn parse<W, T, F>(f: F, tr: T) -> Vec<W>
where
    T: Fn(String) -> Option<W>,
    F: Into<&'static str>,
{
    let fpath = f.into();
    File::open(fpath)
        .map(BufReader::new)
        .map(BufReader::lines)
        .map(|lines| {
            lines
                .filter(Result::is_ok)
                .map(Result::unwrap)
                .map(tr)
                .filter(Option::is_some)
                .map(Option::unwrap)
                .collect::<Vec<_>>()
        })
        .expect(format!("Failed to open {}", fpath).as_str())
}
