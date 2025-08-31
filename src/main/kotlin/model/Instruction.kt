package model

/**
 * Represents a single assembly instruction.
 *
 * Each instruction consists of an operation (`op`), an optional destination operand (`dst`),
 * an optional source operand (`src`), and the line number in the source file where this
 * instruction was defined.
 *
 * The `dst` and `src` operands can be of various types, such as registers, memory locations,
 * or immediate values. The specific types of operands allowed depend on the operation.
 *
 * This class supports instructions that operate on different data sizes:
 * - **8-bit:** Operations typically suffixed with 'b' (e.g., `movb`).
 * - **16-bit:** Operations typically suffixed with 'w' (e.g., `movw`).
 * - **32-bit:** Operations typically suffixed with 'l' (e.g., `movl`).
 * The size of the operation is often implied by the register operands or can be explicitly
 * specified by the opcode mnemonic.
 *
 * @property op The operation code (e.g., MOV, ADD, JMP).
 * @property dst The destination operand. This is where the result of the operation is stored,
 *               or it can be the target of a jump/call. Can be null for instructions
 *               that do not have a destination (e.g., `nop`, `ret`).
 * @property src The source operand. This is the input value or address for the operation.
 *               Can be null for instructions that do not have a source (e.g., `inc`, `push`).
 * @property line The line number in the original assembly source file where this instruction
 *                was parsed from. Defaults to -1 if not applicable or unknown.
 *                Useful for error reporting and debugging.
 */
data class Instruction(
    val op: Op,
    val dst: Operand? = null,
    val src: Operand? = null,
    val line: Int = -1
)

// Example usage scenarios reflecting different data sizes:
// Instruction(Op.MOV_B, Register.AL, ImmediateValue(5)) // 8-bit move
// Instruction(Op.ADD_W, Register.BX, MemoryOperand(Register.SI)) // 16-bit add
// Instruction(Op.PUSH_L, Register.EAX) // 32-bit push
// Instruction(Op.JMP, Label("loop_start")) // Jump instruction (size often implicit or architecture-dependent)