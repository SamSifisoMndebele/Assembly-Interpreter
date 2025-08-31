package model

/**
 * Represents the different base operations (opcodes) that can be executed by the CPU.
 * The actual operand size for these operations (8-bit, 16-bit, 32-bit) will be
 * determined by the instruction's mnemonic (e.g., MOV8, MOV16, MOV32) or
 * by operand prefixes if not explicitly part of the mnemonic.
 * determined during instruction decoding or specified by instruction prefixes.
 * Control flow instructions generally do not have explicit size suffixes as their
 * whose operand size is implicit or not directly tied to a data register width.
 */
enum class Op {
    // Data Transfer Instructions
    /** Move data */
    MOV,
    /** Push data onto stack */
    PUSH,
    /** Pop data from stack */
    POP,
    /** Exchange */
    XCHG,
    /** Load Effective Address */
    LEA,

    // Arithmetic Instructions
    /** Add */
    ADD,
    /** Subtract */
    SUB,
    /** Increment */
    INC,
    /** Decrement */
    DEC,
    /** Compare */
    CMP,
    /** Multiply (Unsigned) */
    MUL,
    /** Integer Multiply (Signed) */
    IMUL,
    /** Divide (Unsigned) */
    DIV,
    /** Integer Divide (Signed) */
    IDIV,
    /** Negate (Two's Complement) */
    NEG,

    // Logical Instructions
    /** Logical AND */
    AND,
    /** Logical OR */
    OR,
    /** Logical XOR */
    XOR,
    /** Logical NOT */
    NOT,
    /** Test (Logical Compare) */
    TEST,

    // Shift and Rotate Instructions
    /** Shift Logical Left */
    SHL,
    /** Shift Arithmetic Left (same as SHL) */
    SAL,
    /** Shift Logical Right */
    SHR,
    /** Shift Arithmetic Right */
    SAR,
    /** Rotate Left */
    ROL,
    /** Rotate Right */
    ROR,
    /** Rotate Left through Carry */
    RCL,
    /** Rotate Right through Carry */
    RCR,

    // Control Flow Instructions
    /** Unconditional Jump */
    JMP,
    /** Jump if Equal (ZF=1) */
    JE,
    /** Jump if Not Equal (ZF=0) */
    JNE,
    /** Jump if Greater (ZF=0 and SF=OF) */
    JG,
    /** Jump if Less (SF!=OF) */
    JL,
    /** Jump if Greater or Equal (SF=OF) */
    JGE,
    /** Jump if Less or Equal (ZF=1 or SF!=OF) */
    JLE,

    // Subroutine/Function Calls
    /** Call subroutine */
    CALL,
    /** Return from subroutine */
    RET,

    // String Operations
    /** Move String */
    MOVS, // MOVSB, MOVSW, MOVSD
    /** Compare Strings */
    CMPS, // CMPSB, CMPSW, CMPSD
    /** Scan String */
    SCAS, // SCASB, SCASW, SCASD
    /** Load String */
    LODS, // LODSB, LODSW, LODSD
    /** Store String */
    STOS, // STOSB, STOSW, STOSD

    // Miscellaneous
    /** Software Interrupt */
    INT,
    /** No Operation */
    NOP
}