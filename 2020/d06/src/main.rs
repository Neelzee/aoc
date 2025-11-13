#[cfg(feature = "p1")]
pub mod p1;
#[cfg(feature = "p2")]
pub mod p2;

#[allow(unused)]
const INPUT: &str = include_str!("../input");

fn main() {
    if cfg!(not(feature = "p1")) && cfg!(not(feature = "p2")) {
        panic!("No feature choosen");
    }

    #[cfg(feature = "p1")]
    p1::run(INPUT);
    #[cfg(feature = "p2")]
    p2::run(INPUT);
}

