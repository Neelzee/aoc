#!/bin/bash

gen() {
  local day
  day="$1"
  local project="d$day"
  if [[ -d "$project" ]]; then
    echo "Project exists"
    exit
  fi

  cargo new "$project" || exit 1

  local cargo="$project/Cargo.toml"

  echo '
[features]
p1 = []
p2 = []
' >> "$cargo"

  local input="$project/input"

  touch "$input"

  local main="$project/src/main.rs"

  local mainTxt='#[cfg(feature = "p1")]
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
'

  echo "$mainTxt" > "$main"

  local modTxt="pub fn run(input: &str) { todo!() }"

  local p1="$project/src/p1.rs";
  local p2="$project/src/p2.rs";

  echo "$modTxt" > "$p1"
  echo "$modTxt" > "$p2"
}

main() {
  local day
  day="$1"
  if [[ ! "$day" ]]; then
    echo "No day choosen"
    exit 1
  fi

  gen "$day"
}

main "$1"
