# My Compiler
A project made for exploring a compiler inner workings.
Currently working on an SSA intermediate representation.

From a test file `test/test.mclang`:
```
fn myfunc() -> i64 {
    return 0;
}

public fn main() -> i64 {
    let a: i64 = 5;
    let b: i64 = a - 1;
    while (b > 3) {
        b = b - 1;
    }
    return b;
}
```

Sample output:
```
fn myfunc:
0:    IRInstruction { id: 1, type: i64, operation: ConstInt(0) }
1:    IRInstruction { id: 2, type: Void, operation: Return(1, 0) }
fn main:
0:    IRInstruction { id: 3, type: i64, operation: AllocStack(8) }
1:    IRInstruction { id: 4, type: i64, operation: ConstInt(5) }
2:    IRInstruction { id: 5, type: Void, operation: Store { ptr: 3, value: 4 } }
3:    IRInstruction { id: 6, type: i64, operation: AllocStack(8) }
4:    IRInstruction { id: 7, type: i64, operation: Load { ptr: 3 } }
5:    IRInstruction { id: 8, type: i64, operation: ConstInt(1) }
6:    IRInstruction { id: 9, type: i64, operation: Arithmetic { op: Sub, left: 7, right: 8 } }
7:    IRInstruction { id: 10, type: Void, operation: Store { ptr: 6, value: 9 } }
8:    IRInstruction { id: 11, type: Void, operation: Label(0) }
9:    IRInstruction { id: 12, type: i64, operation: Load { ptr: 6 } }
10:    IRInstruction { id: 13, type: i64, operation: ConstInt(3) }
11:    IRInstruction { id: 14, type: i8, operation: Comparison { op: Larger, left: 12, right: 13 } }
12:    IRInstruction { id: 15, type: Void, operation: JumpLabelIfNot { cond: 14, label: 1 } }
13:    IRInstruction { id: 16, type: i64, operation: Load { ptr: 6 } }
14:    IRInstruction { id: 17, type: i64, operation: Load { ptr: 6 } }
15:    IRInstruction { id: 18, type: i64, operation: ConstInt(1) }
16:    IRInstruction { id: 19, type: i64, operation: Arithmetic { op: Sub, left: 17, right: 18 } }
17:    IRInstruction { id: 20, type: i64, operation: Load { ptr: 19 } }
18:    IRInstruction { id: 21, type: Void, operation: Store { ptr: 16, value: 20 } }
19:    IRInstruction { id: 22, type: Void, operation: JumpLabel(0) }
20:    IRInstruction { id: 23, type: Void, operation: Label(1) }
21:    IRInstruction { id: 24, type: i64, operation: Load { ptr: 6 } }
22:    IRInstruction { id: 25, type: Void, operation: Return(24, 0) }
```
