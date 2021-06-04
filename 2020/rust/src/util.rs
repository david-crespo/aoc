pub fn print_vec<T>(vec: &Vec<T>)
where
    T: std::fmt::Debug,
{
    for v in vec.iter() {
        println!("{:?}", v)
    }
}
