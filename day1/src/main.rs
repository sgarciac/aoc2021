use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, ErrorKind};

fn larger(path: &str) -> Result<i64, Error> {
    let file = File::open(path)?;
    let br = BufReader::new(file);
    let mut larger = 0;

    let mut buffer: VecDeque<i64> = VecDeque::from(vec![0, 0, 0]);
    let mut prev_sum = 0;

    for (i, line) in br.lines().enumerate() {
        let line = line?;
        let n: i64 = line
            .trim()
            .parse()
            .map_err(|e| Error::new(ErrorKind::InvalidData, e))?;

        buffer.push_back(n);
        let o = buffer.pop_front().unwrap();
        let sum = prev_sum + n - o;

        if i > 2 && sum > prev_sum {
            larger += 1;
        }
        prev_sum = sum;
    }
    Ok(larger)
}

fn main() {
    let pepe = larger("input");
    println!("{:?}", pepe);
}
