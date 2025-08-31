# MASM-like Interpreter in Kotlin

An interpreter for a subset of MASM-like assembly language, written in Kotlin. This project is an educational tool to understand the basics of CPU architecture, assembly language, and interpreter design.

## MVP Scope

-   **16-bit registers**: `AX`, `BX`, `CX`, `DX`, `SI`, `DI`, `BP`, `SP`
-   **Syntax**: Labels, comments (`;`), decimal and hex literals (e.g., `26`, `0x1A`, `1Ah`)
-   **Addressing modes**: `REG`, `IMM`, `MEM [disp]`, `[REG]`, `[REG+disp]`
-   **Operations**: Word-sized operations only (no 8-bit `AL`/`AH`, etc.)
-   **Instructions**: `MOV`, `ADD`, `SUB`, `INC`, `DEC`, `CMP`, `JMP`, `JE`, `JNE`, `JG`, `JL`, `JGE`, `JLE`, `PUSH`, `POP`, `CALL`, `RET`, `INT imm` (`INT 20h` halts program, `INT 30h` prints `AX`)
-   **Directives**: None yet – data must be built manually in memory via `MOV` to `[addr]`.

## Limitations

-   No segments, no `.data`/`.code` sections.
-   No macros, no `PROC`/`ENDP`, no `DUP`, no `EQU`.
-   All memory operations are 16-bit word-sized. Strings/bytes are not supported yet.
-   Flags (`ZF`/`SF`/`CF`) are updated for `ADD`/`SUB`/`CMP` in a simplified manner.
-   The Instruction Pointer (`IP`) is an instruction index, not a byte-based memory address.

## Example Program

Here is an example program that calculates the sum of numbers from 5 down to 1 and prints the result.

```assembly
; Sum numbers from 5 down to 1
; Result will be in AX

start:
    mov cx, 5       ; counter
    mov ax, 0       ; accumulator for the sum

sum_loop:
    add ax, cx      ; add current number to sum
    dec cx          ; decrement counter
    cmp cx, 0       ; is counter zero?
    jne sum_loop    ; if not, loop again

    int 30h         ; print value in AX (should be 15)
    int 20h         ; halt program
```

## How to Run

This project requires a parser to convert assembly source code into an `Instruction` list that the `Interpreter` can execute. Assuming a `Main.kt` file that handles file reading and parsing:

1.  Write your assembly code in a file (e.g., `program.asm`).
2.  Compile and run the interpreter, passing the path to your assembly file.

**Conceptual Usage:**

```bash
# (This is a conceptual command, the actual entry point may vary)
kotlinc src/**/*.kt -include-runtime -d interpreter.jar
java -jar interpretermini-masm.jar program.asm

```

The core execution logic resides in the `Interpreter` class. It processes a list of `Instruction` objects, simulating a 16-bit CPU and memory.