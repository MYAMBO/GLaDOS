### Overall Bytecode Structure

1.  **Header**: Contains metadata about the bytecode file, such as a magic number for file identification and the version of the bytecode format.
2.  **Environment Section**: Holds the initial state of the environment (`Env`).
3.  **Code Body**: A contiguous block of all the instructions (`Program`) for the main script.

```
/--------------------\
|       Header       |
|--------------------|
| Environment Section|
|--------------------|
|     Code Body      |
\--------------------/
```

---

### 1. Header Section

The header's purpose is to identify the file format and provide essential metadata for the VM to bootstrap the execution.

| Field         | Size (bytes) | Description                                                                                             |
|---------------|--------------|---------------------------------------------------------------------------------------------------------|
| Magic Number  | 4            | A unique constant value (e.g., `0x4D59564D` for "MYVM") to identify the file as your language's bytecode. |
| Version       | 4            | The version of the bytecode format (e.g., `0x00000001` for version 1).                                  |

---

### 2. Environment Section

This section serializes the initial `Env`, which is a list of `(String, Val)` pairs. It allows you to pre-populate the environment that the `PushFromEnv` instruction can draw from.

| Field                   | Size (bytes)        | Description                                       |
|-------------------------|---------------------|---------------------------------------------------|
| Environment Entry Count | 4                   | The number of key-value pairs in the environment. |
| **Entries**             | Variable            | A sequence of the specified number of entries.    |
| *-- For each entry --*  |                     |                                                   |
| Key Length              | 4                   | The length of the variable name string in bytes.  |
| Key String              | Variable (UTF-8)    | The variable name itself.                         |
| Value                   | Variable            | The serialized `Val` data (see `Val` serialization below). |

---

### 3. Code Body

This is the most critical section, containing all the executable instructions for the main program and any functions. It's essentially a flat list of all `Program`s concatenated together. A `Func` value will simply point to an offset within this section.

#### Instruction Format

Each instruction is encoded with a 1-byte **opcode**, followed by its arguments, if any.

| Opcode (Hex) | Instruction       | Arguments                               | Description                                                      |
|--------------|-------------------|-----------------------------------------|------------------------------------------------------------------|
| `0x01`       | `Push Val`        | 1 byte (`Val` type tag) + variable      | Pushes a value onto the stack. The value itself follows the opcode. |
| `0x02`       | `Pop`             | None                                    | Pops a value from the stack.                                     |
| `0x03`       | `Return`          | None                                    | Ends execution of the current function and returns a value.      |
| `0x04`       | `Jump Int`        | 4 bytes (integer offset)                | Unconditionally jumps to a new instruction address.              |
| `0x05`       | `JumpIfFalse Int` | 4 bytes (integer offset)                | Jumps if the top of the stack is `Bool False`.                   |
| `0x06`       | `Call ArgCount`   | 4 bytes (integer arg count)             | Calls a function with a specified number of arguments.           |
| `0x07`       | `TailCall ArgCount`| 4 bytes (integer arg count)             | Performs a tail call (not implemented).                                            |
| `0x08`       | `PushFromArgs Int`| 4 bytes (integer index)                 | Pushes an argument from the current function's context.          |
| `0x09`       | `PushFromEnv String`| 4 bytes (length) + variable (string)  | Pushes a value from the environment.                             |

#### `Val` Serialization

Since `Push Val` embeds a value directly into the instruction stream and the Environment Section also contains values, we need a clear way to serialize the `Val` data type. We can use a 1-byte type tag to indicate what kind of data follows.

| Tag (Hex) | `Val` Type     | Payload Format                                                                         |
|-----------|----------------|----------------------------------------------------------------------------------------|
| `0x01`    | `Num Int`      | 4-byte signed integer.                                                                 |
| `0x02`    | `Bool Bool`    | 1 byte (`0x00` for `False`, `0x01` for `True`).                                        |
| `0x03`    | `Op Op`        | 1-byte opcode for the operation (see `Op` serialization below).                        |
| `0x04`    | `Func Program` | 4-byte integer representing the byte offset into the Code Body where the function starts. |

#### `Op` Serialization

When a `Val` is an `Op`, its payload is a single byte identifying the specific operation. This is used when an operation is pushed to the stack and later called (e.g., `Push (Op Add), Call 2`).

| Opcode (Hex) | Operation |
|--------------|-----------|
| `0x01`       | `Add`     |
| `0x02`       | `Sub`     |
| `0x03`       | `Mul`     |
| `0x04`       | `Div`     |
| `0x05`       | `Mod`     |
| `0x06`       | `Neg`     |
| `0x07`       | `Eq`      |
| `0x08`       | `Lt`      |
| `0x09`       | `Gt`      |
| `0x0A`       | `Not`     |
| `0x0B`       | `And`     |
| `0x0C`       | `Or`      |
| `0x0D`       | `Xor`     |

### Example

Let's imagine a simple program: `add(10, 20)`. If `add` is a function defined in the environment, the bytecode might look something like this:

1.  **Header**:
    *   Magic: `0x4D59564D`
    *   Version: `0x00000001`
    *   Entry Point: `0x00000000` (starts at the beginning of the code body)

2.  **Environment Section**:
    *   Entry Count: `1`
    *   Entry 1:
        *   Key Length: `3`
        *   Key String: `"add"`
        *   Value:
            *   Type Tag: `0x04` (for `Func`)
            *   Payload: `0x0000000A` (address of the `add` function in the code body, e.g., offset 10)

3.  **Code Body**:
    *   **Main Program (starts at offset 0):**
        *   `0x09` (`PushFromEnv`)
        *   `0x00000003` (length of "add")
        *   `"add"` (the string itself)
        *   `0x01` (`Push`)
        *   `0x01` (type tag for `Num`)
        *   `0x0000000A` (value 10)
        *   `0x01` (`Push`)
        *   `0x01` (type tag for `Num`)
        *   `0x00000014` (value 20)
        *   `0x06` (`Call`)
        *   `0x00000002` (arg count 2)
        *   `0x03` (`Return`)
    *   **`add` Function (starts at offset 10):**
        *   ... (instructions for the add function) ...
