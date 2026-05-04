package isa

import kotlin.reflect.jvm.jvmName

/**
 * Represents operations that typically take one operand.
 * One operand can be a single register, memory location, or immediate value.
 *
 * Note: Some instructions listed here might have variations that take two operands
 * or an immediate value. This interface primarily covers their one-operand forms.
 */
sealed interface MnemonicOne : Mnemonic {

    data object PUSH : MnemonicOne {
        /**
         * Opcode is always 0xFF for PUSH r/m16/32.
         * The actual operand (reg/mem) is encoded in the ModR/M byte with /6.
         * 16-bit instructions require a 0x66 operand-size prefix (handled in encoder).
         */
        override val encoding = Encoding(
            opcode = listOf(0xFFu),
            modRmExtension = 6
        )
        override fun toString(): String = "push"
    }


    data object POP : MnemonicOne {
        /**
         * Opcode is always 0x8F for POP r/m16/32.
         * The actual operand (reg/mem) is encoded in the ModR/M byte with /0.
         * 16-bit instructions require a 0x66 operand-size prefix (handled in encoder).
         */
        override val encoding = Encoding(
            opcode = listOf(0x8Fu),
            modRmExtension = 6
        )
        override fun toString(): String = "pop"
    }

}