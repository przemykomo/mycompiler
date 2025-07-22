use super::Register;

#[derive(Debug, Clone)]
pub struct TempEntry {
    reg: Register,
    temp_index: usize,
}

#[derive(Debug, Clone)]
pub struct RegMap {
    values: Vec<TempEntry>,
}

impl RegMap {
    pub fn new() -> Self {
        RegMap { values: Vec::new() }
    }

    pub fn contains_key(&self, reg: &Register) -> bool {
        self.values.iter().find(|e| e.reg == *reg).is_some()
    }

    pub fn insert(&mut self, reg: Register, temp_index: usize) {
        assert!(!self.contains_key(&reg));
        self.values.push(TempEntry { reg, temp_index });
    }

    pub fn remove(&mut self, reg: &Register) -> Option<usize> {
        if let Some(pos) = self.values.iter().position(|e| e.reg == *reg) {
            Some(self.values.remove(pos).temp_index)
        } else {
            None
        }
    }

    pub fn get_oldest(&self, ignore: &[Register]) -> Option<usize> {
        if let Some(e) = self.values.iter().find(|e| !ignore.contains(&e.reg)) {
            Some(e.temp_index)
        } else {
            None
        }
    }

    pub fn get_any_free_register(&mut self) -> Option<Register> {
        for reg in USED_REGISTERS {
            if !self.contains_key(&reg) {
                return Some(reg);
            }
        }
        return None;
    }
}

const USED_REGISTERS: [Register; 14] = {
    use Register::*;
    [
        R15, R14, R13, R12, R11, R10, RBX, RAX, R9, R8, RCX, RDX, RSI, RDI,
    ]
};

/*
fn force_get_any_free_register(
    state: &mut ScopeState,
    protected_registers: &[Register],
    hint: Option<Register>,
) -> Register {
    if let Some(reg) = hint {
        if !state.used_registers.contains_key(&reg) {
            return reg;
        }
    }
    if let Some(reg) = get_any_free_register(state) {
        reg
    } else {
        let Some(oldest_temp) = state.used_registers.get_oldest(protected_registers) else {
            unreachable!()
        };
        let TempVariable::Register(reg) = oldest_temp.borrow().clone() else {
            unreachable!()
        };
        state.stack_size_current += 8;
        state.asm.mov(
            RegPointer {
                reg: RBP,
                offset: -state.stack_size_current,
            },
            reg,
            Word::QWORD,
        );
        state.used_registers.remove(&reg);
        *oldest_temp.borrow_mut() = TempVariable::Stack(state.stack_size_current);
        reg
    }
}

fn free_register(state: &mut ScopeState, target_reg: Register, protected_registers: &[Register]) {
    if !state.used_registers.contains_key(&target_reg) {
        return;
    };
    for free_reg in USED_REGISTERS {
        if !protected_registers.contains(&free_reg) && !state.used_registers.contains_key(&free_reg)
        {
            state.asm.mov(free_reg, target_reg, Word::QWORD);
            if let Some(temp) = state.used_registers.remove(&target_reg) {
                *temp.borrow_mut() = TempVariable::Register(free_reg);
                state.used_registers.insert(free_reg, temp);
            }
            return;
        }
    }
    let Some(temp) = state.used_registers.remove(&target_reg) else {
        unreachable!()
    };
    let TempVariable::Register(reg) = temp.borrow().clone() else {
        unreachable!()
    };
    state.stack_size_current += 8;
    state.asm.mov(
        RegPointer {
            reg: RBP,
            offset: -state.stack_size_current,
        },
        reg,
        Word::QWORD,
    );
    state.used_registers.remove(&reg);
    *temp.borrow_mut() = TempVariable::Stack(state.stack_size_current);
} */
