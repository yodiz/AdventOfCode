
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

fn lines_from_file(filename: impl AsRef<Path>, myFun : String -> i32) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|line| line.expect("Could not parse line"))
        .collect()
}

fn x (a:String) -> String {
    a + "b"
}


fn test() {
    let u = vec![1, 2, 3];
    let v = u.iter().map(|&x| x + 1).collect::<Vec<_>>();

    let lines = lines_from_file("c:\\Drive\\Projects\\Adventcode\\2019\\Rust\\day1.input");
    lines.iter().map(x).collect::<Vec<_>>();

}

#[test]
fn testCase1() { 
    assert!(fuel(12) == 2); 
    assert!(fuel(14) == 2);
    assert!(fuel(1969) == 654);
    assert!(fuel(100756) == 33583); 
}
