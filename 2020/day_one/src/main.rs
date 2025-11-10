#[cfg(feature = "one")]
pub mod one;
#[cfg(feature = "two")]
pub mod two;

#[allow(unused)]
const INPUT: &str = include_str!("../day1");

fn main() {

    if cfg!(not(feature = "one")) && cfg!(not(feature = "two")) {
        panic!("No feature choosen");
    }

    #[cfg(feature = "one")]
    one::run(INPUT);
    #[cfg(feature = "two")]
    two::run(INPUT);
}