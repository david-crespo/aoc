use std::collections::HashMap;

type Dat = (i32, i32, i32);
type Cmd = fn(i32, &Dat) -> Dat;

fn c0(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 13;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 15;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c1(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 13;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 16;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c2(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 10;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 4;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c3(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 15;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 14;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c4(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -8;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 1;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c5(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -10;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 5;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c6(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 11;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 1;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c7(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -3;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 3;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c8(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 14;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 3;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c9(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -4;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 7;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c10(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 1;
    x = x + 14;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 5;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c11(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -5;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 13;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c12(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -8;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 3;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn c13(num: i32, dat: &Dat) -> Dat {
    let w = num;
    let (mut x, mut y, mut z) = dat;
    x = x * 0;
    x = x + z;
    x = x % 26;
    z = z / 26;
    x = x + -11;
    x = (x == w) as i32;
    x = (x == 0) as i32;
    y = y * 0;
    y = y + 25;
    y = y * x;
    y = y + 1;
    z = z * y;
    y = y * 0;
    y = y + w;
    y = y + 10;
    y = y * x;
    z = z + y;
    (x, y, z)
}

fn main() {
    let chunks: Vec<Cmd> = vec![c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13];
    let mut dats: HashMap<Dat, i64> = HashMap::new();
    dats.insert((0, 0, 0), 0);
    let mut new_dats: HashMap<Dat, i64>;

    for (i, chunk) in chunks.iter().enumerate() {
        new_dats = HashMap::new();
        for (dat, high) in dats.iter() {
            let base = high * 10;
            for d in 1..10 {
                let new_dat = chunk(d, dat);
                let n = base + (d as i64);
                if new_dats.get(&new_dat).unwrap_or(&0) < &n {
                    new_dats.insert(new_dat, n);
                }
            }
        }
        dats = new_dats;
        dbg!(i, dats.len());
    }
    for (k, v) in dats.iter() {
        if k.2 == 0 {
            dbg!(k, v);
        }
    }
}

// solves part 1 in 21s in release mode
// compare to 100s for python
