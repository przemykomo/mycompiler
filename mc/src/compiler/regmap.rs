use std::{cell::RefCell, rc::Rc};

use super::{Register, TempVariable};

#[derive(Debug, Clone)]
pub struct TempEntry {
    reg: Register,
    temp: Rc<RefCell<TempVariable>>
}

#[derive(Debug, Clone)]
pub struct RegMap {
    //map: HashMap<Register, Rc<RefCell<TempVariable>>>
    values: Vec<TempEntry>
}

impl RegMap {
    pub fn contains_key(&self, reg: &Register) -> bool {
        self.values.iter().find(|e| e.reg == *reg).is_some()
    }

    pub fn insert(&mut self, reg: Register, temp: Rc<RefCell<TempVariable>>) {
        assert!(!self.contains_key(&reg));
        self.values.push(TempEntry { reg, temp });
    }

    pub fn remove(&mut self, reg: &Register) -> Option<Rc<RefCell<TempVariable>>> {
        if let Some(pos) = self.values.iter().position(|e| e.reg == *reg) {
            Some(self.values.remove(pos).temp)
        } else {
            None
        }
    }

    pub fn get_oldest(&self, ignore: &[Register]) -> Option<Rc<RefCell<TempVariable>>> {
       if let Some(e) = self.values.iter().find(|e| !ignore.contains(&e.reg)) {
           Some(e.temp.clone())
       } else {
           None
       }
    }

    pub fn new() -> Self {
        RegMap {
            values: Vec::new()
        }
    }
}
