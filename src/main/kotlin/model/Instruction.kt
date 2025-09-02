package model

/**
 * Represents a single assembly language instruction or a label definition.
 *
 * This sealed class is the base for different types of instructions based on the
 * number of operands they take: zero, one, or two. It also includes a type
 * for label definitions, which are essential for control flow in assembly.
 *
 * Each instruction, regardless of its type, can store the line number from the
 * source file where it was parsed. This is useful for error reporting and debugging.
 *
 * The specific operations and operand types are defined within nested data classes
 * ({@link InstructionZero}, {@link InstructionOne}, {@link InstructionTwo}, {@link LabelDefinition}).
 *
 * @property line The line number in the original assembly source file where this
 *                instruction or label was parsed from. Defaults to -1 if not
 *                applicable or unknown.
 */
sealed class Instruction(
    open val line: Int = -1
) {
    /**
     * Represents an instruction with no operands.
     * Examples: `nop`, `ret`.
     *
     * @property operation The operation code.
     * @property line The line number in the source file.
     */
    data class InstructionZero(
        val operation: Operation.OperationZero,
        override val line: Int = -1
    ) : Instruction(line)

    /**
     * Represents an instruction with one operand (typically a destination or a source).
     * Examples: `inc rax`, `push eax`, `jmp label`.
     *
     * @property operation The operation code.
     * @property operand The single operand.
     * @property line The line number in the source file.
     */
    data class InstructionOne(
        val operation: Operation.OperationOne,
        val operand: Operand,
        override val line: Int = -1
    ) : Instruction(line)

    /**
     * Represents an instruction with two operands (typically a destination and a source).
     * Examples: `mov rax, rbx`, `add eax, 5`.
     *
     * @property operation The operation code.
     * @property dst The destination operand.
     * @property src The source operand.
     * @property line The line number in the source file.
     */
    data class InstructionTwo(
        val operation: Operation.OperationTwo,
        val dst: Operand,
        val src: Operand,
        override val line: Int = -1
    ) : Instruction(line)
}