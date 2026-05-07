use std::collections::{HashMap, HashSet};

use crate::ir::{IRGen, IRInstruction, Operation};

pub struct Lower<'a> {
    pub irgen: &'a IRGen<'a>,
    pub functions: Vec<LoweredFunc>,
}

#[derive(Debug)]
pub struct LoweredFunc {
    pub ident: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
}

#[derive(Debug)]
pub enum Opcode {
    Add,
    Ret,
    Mov,
}

impl Instruction {}

#[derive(Debug)]
pub struct Operand {
    pub reg: VReg,
    pub operation: VRegOperation,
}

#[derive(Debug)]
pub enum VRegOperation {
    Read,
    Write,
    Modify,
}

#[derive(Debug)]
pub struct VReg {
    pub real: bool,
    pub class: RegClass,
    pub id: usize,
}

impl VReg {
    const RAX: VReg = VReg {
        real: true,
        class: RegClass::Int,
        id: 0,
    };
    const RDX: VReg = VReg {
        real: true,
        class: RegClass::Int,
        id: 3,
    };
}

#[derive(Debug)]
pub enum RegClass {
    Int,
    Float,
}

impl<'a> Lower<'a> {
    pub fn new(irgen: &'a IRGen<'a>) -> Lower<'a> {
        Lower {
            irgen,
            functions: Vec::new(),
        }
    }

    pub fn lower(&mut self) {
        for function in &self.irgen.functions {
            let mut instructions = Vec::new();
            // let mut vregs = HashMap::new();
            let mut used = HashSet::new();

            for IRInstruction {
                id,
                r#type,
                operation,
            } in function.scope.instructions.iter().rev()
            {
                match operation {
                    Operation::ConstInt(_) => todo!(),
                    Operation::ConstFloat(_) => todo!(),
                    Operation::Arithmetic { op, left, right } => todo!(),
                    Operation::Comparison { op, left, right } => todo!(),
                    Operation::Return(a, b) => {
                        let mut operands = Vec::new();
                        if *a != 0 {
                            operands.push(Operand {
                                reg: VReg::RAX,
                                operation: VRegOperation::Read,
                            });
                        }
                        if *b != 0 {
                            operands.push(Operand {
                                reg: VReg::RDX,
                                operation: VRegOperation::Read,
                            });
                        }
                        instructions.push(Instruction {
                            opcode: Opcode::Ret,
                            operands,
                        });

                        // SystemV ABI requires the usage of RAX & RDX for integers
                        // TODO: SSA vector registers return
                        if *a != 0 {
                            instructions.push(Instruction {
                                opcode: Opcode::Mov,
                                operands: vec![
                                    Operand {
                                        reg: VReg::RAX,
                                        operation: VRegOperation::Write,
                                    },
                                    Operand {
                                        reg: VReg {
                                            real: false,
                                            class: RegClass::Int,
                                            id: *a,
                                        },
                                        operation: VRegOperation::Read,
                                    },
                                ],
                            });
                            used.insert(*a);
                        }
                        if *b != 0 {
                            instructions.push(Instruction {
                                opcode: Opcode::Mov,
                                operands: vec![
                                    Operand {
                                        reg: VReg::RDX,
                                        operation: VRegOperation::Write,
                                    },
                                    Operand {
                                        reg: VReg {
                                            real: false,
                                            class: RegClass::Int,
                                            id: *b,
                                        },
                                        operation: VRegOperation::Read,
                                    },
                                ],
                            });
                            used.insert(*b);
                        }
                    }
                    Operation::Label(_) => todo!(),
                    Operation::JumpLabelIfNot { cond, label } => todo!(),
                    Operation::JumpLabel(_) => todo!(),
                    Operation::Upsilon { temp, shadow } => todo!(),
                    Operation::Phi(_) => todo!(),
                    Operation::AllocStack(_) => todo!(),
                    Operation::Store { ptr, value } => todo!(),
                    Operation::Load { ptr } => todo!(),
                }
            }

            // instructions.reverse();
            self.functions.push(LoweredFunc {
                ident: function.ident.clone(),
                instructions,
            });
        }
    }
}
