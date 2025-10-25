# GLaDOS VM: Bytecode Format Specification

### Core Concepts

Before detailing the format, it's important to understand the VM's execution model:

*   **The Stack:** The VM is a stack-based machine. Most instructions manipulate a central data stack by pushing values onto it or popping them off. For example, to add `5` and `10`, the bytecode must first push `5`, then push `10`, then execute the `Call` instruction on the `Add` operation.
*   **Global Environment:** This is a read-only, key-value map loaded from the bytecode file. It is designed to hold globally accessible values, primarily the built-in functions and operators (like `+`, `-`, `if`, etc.) that the compiler makes available.
*   **Local Environment:** When a function is called, a new, mutable, local environment is created for it. This environment stores local variables defined with `Define` and updated with `Assign`. When the VM searches for a variable, it checks the local environment first before falling back to the global one.

---

### Overall File Structure

1.  **Header**: Contains metadata to identify the file and its version.
2.  **Global Environment Section**: Contains the initial, read-only state of the global environment.
3.  **Code Body**: A contiguous block of instructions for the **main program**.

```
/---------------------\
|       Header        |
|---------------------|
|  Global Environment |
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
| Magic Number | 4 | A unique constant value (`0x42414B41`) to identify the file as GLaDOS bytecode. |
| Version | 4 | The version of the bytecode format (e.g., `0x00000002` for version 2). |

---

### 2. Global Environment Section

This section serializes the initial **global environment**, which is a list of `(String, Val)` pairs.

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

This section contains the executable instructions of the main program.

#### `Val` Serialization

A `Val` is a typed value in the GLaDOS language. It is serialized using a 1-byte **type tag** to indicate the nature of the data that follows.

| Tag (Hex) | `Val` Type | Payload Format |
|---|---|---|
| `0x01` | `BoolVal` | 1 byte (`0x00` for `False`, `0x01` for `True`). |
| `0x02` | `Op` | 1-byte opcode for a built-in operation (see `Op` serialization). |
| `0x03` | `Func` | `[4-byte instruction count]` followed by the complete sequence of the function's instructions. |
| | | |
| `0x10` | `Int8Val` | 1-byte signed integer. |
| `0x11` | `Int16Val` | 2-byte signed integer (big-endian). |
| `0x12` | `Int32Val` | 4-byte signed integer (big-endian). |
| `0x13` | `Int64Val` | 8-byte signed integer (big-endian). |
| | | |
| `0x20` | `Word8Val` | 1-byte unsigned integer. |
| `0x21` | `Word16Val`| 2-byte unsigned integer (big-endian). |
| `0x22` | `Word32Val`| 4-byte unsigned integer (big-endian). |
| `0x23` | `Word64Val`| 8-byte unsigned integer (big-endian). |
| | | |
| `0x30` | `FltVal` | 4-byte IEEE 754 floating-point number (big-endian). |
| `0x31` | `DblVal` | 8-byte IEEE 754 floating-point number (big-endian). |

*Note on Lists: The `List` type is a runtime-only value. It cannot be directly serialized in the bytecode. To create a list, the bytecode must use the `EmptyList` operation to get an empty list, then use `Cons` to add elements.*

---

### 4. Instruction Format

Each instruction is encoded with a 1-byte **opcode**, followed by its arguments, if any.

| Opcode (Hex) | Instruction | Arguments | Description |
|---|---|---|---|
| `0x01` | `Push Val` | 1 byte (`Val` tag) + variable | Pushes a complete `Val` structure onto the stack. |
| `0x02` | `Pop` | None | Pops the top value from the stack and discards it. |
| `0x03` | `Return` | None | Ends execution of the current function. The value on top of the stack becomes the return value of the call. |
| `0x04` | `Jump Int` | 4 bytes (integer) | Unconditionally jumps forward by a relative number of `n` instructions. |
| `0x05` | `JumpIfFalse Int` | 4 bytes (integer) | Pops a value from the stack. If it is `BoolVal False`, jumps forward by `n` instructions. Otherwise, continues execution. |
| `0x06` | `Call ArgCount` | 4 bytes (integer) | Calls a function. It expects `ArgCount` arguments on the stack, with the function itself below them. Creates a new local environment for the call. |
| `0x07` | `TailCall ArgCount`| 4 bytes (integer) | Performs a tail call. Replaces the current execution context instead of creating a new one, optimizing for recursion. |
| `0x08` | `PushFromArgs Int`| 4 bytes (integer index) | Pushes an argument from the current function's context by its zero-based index. |
| `0x09` | `PushFromEnv String`| 4 bytes (length) + variable (string) | Pushes a value from an environment. It searches the **current local scope first**, then falls back to the read-only global environment. |
| `0x0A` | `Define String` | 4 bytes (length) + variable (string) | Pops a value from the stack and creates a **new variable** in the **current local environment**. |
| `0x0B` | `Assign String` | 4 bytes (length) + variable (string) | Pops a value from the stack and **updates an existing variable** in the **current local environment**. Fails if the variable is not found in the local scope. |

---

### 5. `Op` Serialization

When a `Val` is an `Op`, its payload is a single byte identifying the built-in operation. The VM performs automatic type promotion (e.g., Int -> Float) for most binary numeric operations.

| Opcode (Hex) | Operation | Description & Stack Signature `( ... -> ... )` |
|---|---|---|
| `0x01` | `Add` | `(Num A, Num B -> Num C)` Adds two numbers. |
| `0x02` | `Sub` | `(Num A, Num B -> Num C)` Subtracts B from A. |
| `0x03` | `Mul` | `(Num A, Num B -> Num C)` Multiplies two numbers. |
| `0x04` | `Div` | `(Num A, Num B -> Num C)` Divides A by B. Handles both integer `div` and float `/`. |
| `0x05` | `Mod` | `(Int A, Int B -> Int C)` Modulo operator. **Integer types only.** |
| `0x06` | `Neg` | `(SignedNum A -> SignedNum B)` Negates a signed number. |
| `0x07` | `Eq` | `(A, B -> Bool)` Checks for equality. Works on numeric types and booleans. |
| `0x08` | `Lt` | `(Num A, Num B -> Bool)` Less than check. |
| `0x09` | `Gt` | `(Num A, Num B -> Bool)` Greater than check. |
| `0x0A` | `Not` | `(Bool -> Bool)` Logical NOT. **Boolean only.** |
| `0x0B` | `And` | `(Bool, Bool -> Bool)` Logical AND. **Boolean only.** |
| `0x0C` | `Or` | `(Bool, Bool -> Bool)` Logical OR. **Boolean only.** |
| `0x0D` | `Xor` | `(Bool, Bool -> Bool)` Logical XOR. **Boolean only.** |
| `0x0E` | `Cons` | `(List, Element -> List)` Constructs a new list by adding an element to the front. |
| `0x0F` | `Car` | `(List -> Element)` Gets the head (first element) of a list. Fails on empty list. |
| `0x10` | `Cdr` | `(List -> List)` Gets the tail (all but the first element) of a list. Fails on empty list. |
| `0x11` | `EmptyList` | `( -> List)` Pushes a new, empty list onto the stack. |
