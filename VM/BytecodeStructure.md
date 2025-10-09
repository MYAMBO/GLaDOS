# GLaDOS VM: Bytecode Format Specification

This document describes the structure of the bytecode used by the GLaDOS virtual machine.

### Overall File Structure

1.  **Header**: Contains metadata to identify the file and its version.
2.  **Environment Section**: Contains the initial state of the environment (`Env`). **The bytecode for functions is fully serialized here.**
3.  **Code Body**: A contiguous block containing only the instructions for the **main program**.

```
/---------------------\
|       Header        |
|---------------------|
| Environment Section |
| (includes function  |
|        code)        |
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
| Magic Number | 4 | A unique constant value (`0x42414b41`) to identify the file as your language's bytecode. |
| Version | 4 | The version of the bytecode format (e.g., `0x00000001` for version 1). |

---

### 2. Environment Section

This section serializes the initial environment (`Env`), which is a list of `(String, Val)` pairs. This is where functions and global variables are defined before execution.

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

This critical section contains only the executable instructions of the main program. It is a flat list of instructions that will be executed when the VM starts.

#### Instruction Format

Each instruction is encoded with a 1-byte **opcode**, followed by its arguments, if any.

| Opcode (Hex) | Instruction | Arguments | Description |
|---|---|---|---|
| `0x01` | `Push Val` | 1 byte (`Val` type tag) + variable | Pushes a value onto the stack. The value itself follows the opcode. |
| `0x02` | `Pop` | None | Pops a value from the stack. |
| `0x03` | `Return` | None | Ends execution of the current function and returns a value. |
| `0x04` | `Jump Int` | 4 bytes (integer) | Jumps forward by a relative number of `n` instructions. |
| `0x05` | `JumpIfFalse Int` | 4 bytes (integer) | Jumps forward by `n` instructions if the top of the stack is `Bool False`. |
| `0x06` | `Call ArgCount` | 4 bytes (integer) | Calls a function with a specified number of arguments. |
| `0x07` | `TailCall ArgCount`| 4 bytes (integer) | Performs a tail call (not implemented). |
| `0x08` | `PushFromArgs Int`| 4 bytes (integer index) | Pushes an argument from the current function's context. |
| `0x09` | `PushFromEnv String`| 4 bytes (length) + variable (string) | Pushes a value from the environment. |

#### `Val` Serialization

A `Val` is serialized using a 1-byte type tag to indicate the nature of the data that follows.

| Tag (Hex) | `Val` Type | Payload Format |
|---|---|---|
| `0x01` | `Num Int` | 4-byte signed integer. |
| `0x02` | `Bool Bool` | 1 byte (`0x00` for `False`, `0x01` for `True`). |
| `0x03` | `Op Op` | 1-byte opcode for the operation (see `Op` serialization below). |
| `0x04` | `Func Program` | **[4-byte integer for the instruction count]** followed by the **complete sequence of the function's instructions**. |

#### `Op` Serialization

When a `Val` is an `Op`, its payload is a single byte identifying the operation. This is used when an operation is pushed to the stack to be called (e.g., `Push (Op Add)`).

| Opcode (Hex) | Operation |
|---|---|
| `0x01` | `Add` |
| `0x02` | `Sub` |
| `0x03` | `Mul` |
| `0x04` | `Div` |
| `0x05` | `Mod` |
| `0x06` | `Neg` |
| `0x07` | `Eq` |
| `0x08` | `Lt` |
| `0x09` | `Gt` |
| `0x0A` | `Not` |
| `0x0B` | `And` |
| `0x0C` | `Or` |
| `0x0D` | `Xor` |

---

### Example

Let's imagine a simple program: `add(10, 20)`.

**Calling Convention:** To call a function, its arguments are pushed onto the stack *after* the function itself. The sequence is: `Push (Func ...)` -> `Push arg1` -> `Push arg2` -> `Call 2`.

1.  **Header**:
    *   Magic: `0x4D59564D`
    *   Version: `0x00000001`

2.  **Environment Section**:
    *   Entry Count: `1`
    *   **Entry 1:**
        *   Key Length: `3`
        *   Key: `"add"`
        *   **Value (`Func`):**
            *   Type Tag: `0x04` (for `Func`)
            *   Instruction Count: `5`
            *   Instruction 1: `0x01` (Push) + `0x03` (Op Tag) + `0x01` (Op Add)
            *   Instruction 2: `0x08` (PushFromArgs) + `0x00000000` (index 0)
            *   Instruction 3: `0x08` (PushFromArgs) + `0x00000001` (index 1)
            *   Instruction 4: `0x06` (Call) + `0x00000002` (arg count 2)
            *   Instruction 5: `0x03` (Return)

3.  **Code Body (Main Program)**:
    *   Instruction 1: `0x09` (PushFromEnv) + `0x00000003` (length "add") + `"add"`
    *   Instruction 2: `0x01` (Push) + `0x01` (Num Tag) + `0x0000000A` (value 10)
    *   Instruction 3: `0x01` (Push) + `0x01` (Num Tag) + `0x00000014` (value 20)
    *   Instruction 4: `0x06` (Call) + `0x00000002` (arg count 2)
    *   Instruction 5: `0x03` (Return)
