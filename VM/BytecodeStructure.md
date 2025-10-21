# GLaDOS VM: Bytecode Format Specification

This document describes the structure of the bytecode used by the GLaDOS virtual machine.

### Overall File Structure

1.  **Header**: Contains metadata to identify the file and its version.
2.  **Global Environment Section**: Contains the initial, read-only state of the global environment (`Env`). **The bytecode for functions is fully serialized here.**
3.  **Code Body**: A contiguous block containing only the instructions for the **main program**.

```
/---------------------\
|       Header        |
|---------------------|
|  Global Environment |
|  (includes global   |
|      functions)     |
|---------------------|
|      Code Body      |
|    (main program)   |
\---------------------/
```

---

### 1. Header Section

The header identifies the file format and provides essential metadata for the VM to start execution.

| Field | Size (bytes) | Description |
|---|---|---|
| Magic Number | 4 | A unique constant value (`0x42414B41`) to identify the file as your language's bytecode. |
| Version | 4 | The version of the bytecode format (e.g., `0x00000001` for version 1). |

---

### 2. Global Environment Section

This section serializes the initial **global environment**, which is a list of `(String, Val)` pairs. This is where standard library functions and global definitions are stored. This environment is considered **read-only** during program execution.

| Field | Size (bytes) | Description |
|---|---|---|
| Entry Count | 4 | The number of key-value pairs in the environment. |
| **Entries** | Variable | A sequence of the specified number of entries. |
| *-- For each entry --* | | |
| Key Length | 4 | The length in bytes of the variable/function name. |
| Key (String) | Variable (UTF-8) | The variable/function name. |
| Value (`Val`) | Variable | The serialized `Val` data (see the "`Val` Serialization" section below). |

---

### 3. Code Body

This section contains the executable instructions of the main program. A new, empty **local environment** is created for the main program, and for each subsequent function call.

#### Instruction Format

Each instruction is encoded with a 1-byte **opcode**, followed by its arguments, if any.

| Opcode (Hex) | Instruction | Arguments | Description |
|---|---|---|---|
| `0x01` | `Push Val` | 1 byte (`Val` tag) + variable | Pushes a value onto the stack. |
| `0x02` | `Pop` | None | Pops a value from the stack. |
| `0x03` | `Return` | None | Ends execution of the current function and returns the value on top of the stack. Destroys the current local environment. |
| `0x04` | `Jump Int` | 4 bytes (integer) | Jumps forward by a relative number of `n` instructions. |
| `0x05` | `JumpIfFalse Int` | 4 bytes (integer) | Jumps forward by `n` instructions if the top of the stack is `Bool False`. |
| `0x06` | `Call ArgCount` | 4 bytes (integer) | Calls a function with a specified number of arguments. Creates a new, empty local environment for the called function. |
| `0x07` | `TailCall ArgCount`| 4 bytes (integer) | Performs a tail call, replacing the current execution context. |
| `0x08` | `PushFromArgs Int`| 4 bytes (integer index) | Pushes an argument from the current function's context by its index. |
| `0x09` | `PushFromEnv String`| 4 bytes (length) + variable (string) | Pushes a value from the environment. **It searches the current local scope first, then falls back to the read-only global environment.** |
| `0x0A` | `Define String` | 4 bytes (length) + variable (string) | Pops a value from the stack and creates a **new variable** in the **current local environment**. |
| `0x0B` | `Assign String` | 4 bytes (length) + variable (string) | Pops a value from the stack and **updates an existing variable** in the **current local environment**. Fails if the variable is not found in the local scope. |


#### `Val` Serialization

A `Val` is serialized using a 1-byte type tag to indicate the nature of the data that follows.

| Tag (Hex) | `Val` Type | Payload Format |
|---|---|---|
| `0x01` | `Num Int` | 4-byte signed integer (big-endian). |
| `0x02` | `Bool Bool` | 1 byte (`0x00` for `False`, `0x01` for `True`). |
| `0x03` | `Op Op` | 1-byte opcode for the operation (see `Op` serialization below). |
| `0x04` | `Func Program` | `[4-byte instruction count]` followed by the complete sequence of the function's instructions. |

*Note: The `List` type is a runtime-only value created by operations like `Cons` and `EmptyList`. It cannot be directly serialized in the bytecode.*

#### `Op` Serialization

When a `Val` is an `Op`, its payload is a single byte identifying the operation.

| Opcode (Hex) | Operation | Description |
|---|---|---|
| `0x01` | `Add` | Addition |
| `0x02` | `Sub` | Subtraction |
| `0x03` | `Mul` | Multiplication |
| `0x04` | `Div` | Integer Division |
| `0x05` | `Mod` | Modulo |
| `0x06` | `Neg` | Negation |
| `0x07` | `Eq` | Equality check |
| `0x08` | `Lt` | Less than check |
| `0x09` | `Gt` | Greater than check |
| `0x0A` | `Not` | Logical NOT |
| `0x0B` | `And` | Logical AND |
| `0x0C` | `Or` | Logical OR |
| `0x0D` | `Xor` | Logical XOR |
| `0x0E` | `Cons` | Constructs a list pair |
| `0x0F` | `Car` | Gets the head of a list |
| `0x10` | `Cdr` | Gets the tail of a list |
| `0x11` | `EmptyList` | Pushes an empty list onto the stack |

---

### Example: Local Variables

This example demonstrates the bytecode for the following pseudo-code, which tests the new local variable features.

**Pseudo-code:**
```
let x = 10;
x = x + 100;
return x;
```

**Global Environment:**
The global environment must contain the `+` operation.
*   Entry Count: `1`
*   Entry 1: `(Key: "+", Value: (Op Add))`

**Code Body (Main Program):**
```
// let x = 10;
Instruction 1: 0x01 (Push) + 0x01 (Num Tag) + 0x0000000A (value 10)
Instruction 2: 0x0A (Define) + 0x00000001 (length "x") + "x"

// Prepare call to '+'
Instruction 3: 0x09 (PushFromEnv) + 0x00000001 (length "+") + "+"
Instruction 4: 0x01 (Push) + 0x01 (Num Tag) + 0x00000064 (value 100)
Instruction 5: 0x09 (PushFromEnv) + 0x00000001 (length "x") + "x"
Instruction 6: 0x06 (Call) + 0x00000002 (arg count 2)

// Result (110) is on the stack. Now assign it back to x.
Instruction 7: 0x0B (Assign) + 0x00000001 (length "x") + "x"

// Return the final value of x
Instruction 8: 0x09 (PushFromEnv) + 0x00000001 (length "x") + "x"
Instruction 9: 0x03 (Return)
```
