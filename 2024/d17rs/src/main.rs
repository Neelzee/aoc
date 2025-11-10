use std::{fs::File, io::Read};

pub enum Literal {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
}

impl Literal {
    pub fn new(i: u32) -> Self {
        match i {
            0 => Self::Zero,
            1 => Self::One,
            2 => Self::Two,
            3 => Self::Three,
            4 => Self::Four,
            5 => Self::Five,
            6 => Self::Six,
            7 => Self::Seven,
            _ => panic!("Invalid integer"),
        }
    }

    pub fn get_lit(&self) -> u32 {
        match self {
            Self::Zero => 0,
            Self::One => 1,
            Self::Two => 2,
            Self::Three => 3,
            Self::Four => 4,
            Self::Five => 5,
            Self::Six => 6,
            Self::Seven => 8,
        }
    }

    pub fn get_combo(&self, prg: &Program) -> u32 {
        match self {
            Self::Zero => 0,
            Self::One => 1,
            Self::Two => 2,
            Self::Three => 3,
            Self::Four => prg.reg_a,
            Self::Five => prg.reg_b,
            Self::Six => prg.reg_c,
            Self::Seven => panic!("Seven is a reserved keyword"),
        }
    }
}

pub enum OpCode {
    /// The adv instruction (opcode 0) performs division. The numerator is the value in the A register.
    /// The denominator is found by raising 2 to the power of the instruction's combo operand.
    /// (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
    /// The result of the division operation is truncated to an integer and then written to the A 
    /// register.
    Adv,
    /// The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's
    /// literal operand, then stores the result in register B.
    Bxl,
    /// The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
    /// (thereby keeping only its lowest 3 bits), then writes that value to the B register.
    Bst,
    /// The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register
    /// is not zero, it jumps by setting the instruction pointer to the value of its literal operand;
    /// if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
    Jnz,
    /// The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then 
    /// stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
    Bxc,
    /// The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs
    /// that value. (If a program outputs multiple values, they are separated by commas.)
    Out,
    /// The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is
    /// stored in the B register. (The numerator is still read from the A register.)
    Bdv,
    /// The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is
    /// stored in the C register. (The numerator is still read from the A register.)
    Cdv,
}

impl OpCode {
    pub fn new(lit: Literal) -> Self {
        match lit {
            Literal::Zero => Self::Adv,
            Literal::One => Self::Bxl,
            Literal::Two => Self::Bst,
            Literal::Three => Self::Jnz,
            Literal::Four => Self::Bxc,
            Literal::Five => Self::Out,
            Literal::Six => Self::Bdv,
            Literal::Seven => Self::Cdv,
        }
    }
}

pub struct Instruction(OpCode, Literal);

impl Instruction {
    pub fn new(slice: &[u32], ptr: usize) -> Self {
        Self(OpCode::new(Literal::new(slice[ptr])), Literal::new(slice[ptr + 1]))
    }

    pub fn eval(&self, prg: Program) -> Program {
        match self {
            Self(OpCode::Adv, lit) => Program { 
                reg_a: (prg.reg_a as f32 / 2_u32.pow(lit.get_combo(&prg)) as f32).trunc() as u32,
                ptr: prg.ptr + 2,
                ..prg 
            },
            Self(OpCode::Bxl, lit) => Program { 
                reg_b: prg.reg_b ^ lit.get_lit(),
                ptr: prg.ptr + 2,
                ..prg 
            },
            Self(OpCode::Bst, lit) => Program { 
                reg_b: lit.get_combo(&prg) % 8,
                ptr: prg.ptr + 2,
                ..prg
            },
            Self(OpCode::Jnz, _) if prg.reg_a == 0 => Program { ptr: prg.ptr + 2, ..prg }, 
            Self(OpCode::Jnz, lit) => Program { ptr: lit.get_lit() as usize, ..prg },
            Self(OpCode::Bxc, _) => Program { 
                reg_b: prg.reg_b ^ prg.reg_c,
                ptr: prg.ptr + 2,
                ..prg
            },
            Self(OpCode::Out, lit) => { 
                let mut prg = Program {  ptr: prg.ptr + 2, ..prg };
                prg.terminal.push(lit.get_combo(&prg) % 8);
                prg
            },
            Self(OpCode::Bdv, lit) => Program { 
                reg_b: (prg.reg_a as f32 / 2_u32.pow(lit.get_combo(&prg)) as f32).trunc() as u32,
                ptr: prg.ptr + 2,
                ..prg 
            },
            Self(OpCode::Cdv, lit) => Program { 
                reg_c: (prg.reg_a as f32 / 2_u32.pow(lit.get_combo(&prg)) as f32).trunc() as u32,
                ptr: prg.ptr + 2,
                ..prg 
            },
        }
    }
}

pub struct Program {
    reg_a: u32,
    reg_b: u32,
    reg_c: u32,
    ptr: usize,
    program: Vec<u32>,
    terminal: Vec<u32>,
}

impl Program {
    pub fn step(self) -> Result<Self, Self> {
        if self.ptr >= self.program.len() - 1 {
            return Err(self);
        }
        let ins = Instruction::new(&self.program, self.ptr);


        Ok(ins.eval(self))
    }
}

fn get_input() -> Program {
    let mut file = File::open("input.txt").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let mut lines = buf.lines();
    let reg_a = get_reg(lines.next().unwrap());
    let reg_b = get_reg(lines.next().unwrap());
    let reg_c = get_reg(lines.next().unwrap());
    lines.next();
    let program = lines.next().unwrap().replace("Program: ", "").trim().split(",").map(|c| c.parse::<u32>().unwrap()).collect();

    Program { 
        reg_a,
        reg_b,
        reg_c,
        ptr: 0,
        program,
        terminal: Vec::new(),
    }
}

fn get_reg(s: &str) -> u32 {
    s.replace("Register ", "")
        .replace("A:", "")
        .replace("B:", "")
        .replace("C:", "")
        .trim()
        .parse::<u32>()
        .unwrap()
}


fn main() {
    let mut prg = get_input();

    loop {
        match prg.step() {
            Ok(p) => prg = p,
            Err(p) => {
                prg = p;
                break;
            }
        }
    }
    println!("{}", prg.terminal.into_iter().map(|i| i.to_string()).collect::<Vec<_>>().join(","));
}
