package model.mnemonic

import model.Bits

/**
 * Represents operations that typically take one operand.
 * One operand can be a single register, memory location, or immediate value.
 *
 * Note: Some instructions listed here might have variations that take two operands
 * or an immediate value. This interface primarily covers their one-operand forms.
 */
sealed interface MnemonicOne : Mnemonic {

    /**
     * Pushes a 16-bit or 32-bit operand onto the stack.
     * The operand can be a register or a memory location.
     *
     * @property bits The size of the operand to be pushed (16 or 32 bits).
     */
    data class PUSH(
        override val bits: Bits  // 16 or 32
    ) : MnemonicOne {

        init {
            require(bits == Bits.B16 || bits == Bits.B32) {
                "PUSH only supports 16- or 32-bit operands: $bits"
            }
        }

        /**
         * Opcode is always 0xFF for PUSH r/m16/32.
         * The actual operand (reg/mem) is encoded in the ModR/M byte with /6.
         * 16-bit instructions require a 0x66 operand-size prefix (handled in encoder).
         */
        override val opcode: UByte = 0xFFu

        override fun toString(): String = "PUSH ${bits.name}"
    }

    /**
     * Represents the POP model.operation (Pop from Stack).
     * This model.operation pops a 16-bit or 32-bit value from the top of the stack
     * into the specified register or memory location.
     *
     * @property bits The size of the operand (16 or 32 bits).
     */
    data class POP(
        override val bits: Bits  // 16 or 32
    ) : MnemonicOne {

        init {
            require(bits == Bits.B16 || bits == Bits.B32) {
                "POP only supports 16- or 32-bit operands: $bits"
            }
        }

        /**
         * Opcode is always 0x8F for POP r/m16/32.
         * The actual operand (reg/mem) is encoded in the ModR/M byte with /0.
         * 16-bit instructions require a 0x66 operand-size prefix (handled in encoder).
         */
        override val opcode: UByte = 0x8Fu

        override fun toString(): String = "POP ${bits.name}"
    }

}