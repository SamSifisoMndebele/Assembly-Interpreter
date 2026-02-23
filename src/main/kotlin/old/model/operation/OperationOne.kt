package old.model.operation

import old.model.Bits

/**
 * Represents operations that typically take one operand.
 * One operand can be a single register, memory location, or immediate value.
 *
 * Note: Some instructions listed here might have variations that take two operands
 * or an immediate value. This interface primarily covers their one-operand forms.
 */
sealed interface OperationOne : Operation {

    /**
     * Pushes a 16-bit or 32-bit operand onto the stack.
     * The operand can be a register or a memory location.
     *
     * @property bits The size of the operand to be pushed (16 or 32 bits).
     */
    data class PUSH(
        override val bits: Bits  // 16 or 32
    ) : OperationOne {

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
     * Represents the POP operation (Pop from Stack).
     * This operation pops a 16-bit or 32-bit value from the top of the stack
     * into the specified register or memory location.
     *
     * @property bits The size of the operand (16 or 32 bits).
     */
    data class POP(
        override val bits: Bits  // 16 or 32
    ) : OperationOne {

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


//        // Increment
//        data object INC32 : OperationOne {
//            override val opcode: UByte = 0xFFu  // ModR/M /0
//            override val bits: Bits = Bits.BITS_32
//        }
//        data object INC16 : OperationOne {
//            override val opcode: UByte = 0xFFu  // ModR/M /0 + 0x66 prefix
//            override val bits: Bits = Bits.BITS_16
//        }
//        data object INC8 : OperationOne {
//            override val opcode: UByte = 0xFEu  // ModR/M /0
//            override val bits: Bits = Bits.BITS_8
//        }
//
//        // Decrement
//        data object DEC32 : OperationOne {
//            override val opcode: UByte = 0xFFu  // ModR/M /1
//            override val bits: Bits = Bits.BITS_32
//        }
//        data object DEC16 : OperationOne {
//            override val opcode: UByte = 0xFFu  // ModR/M /1 + 0x66 prefix
//            override val bits: Bits = Bits.BITS_16
//        }
//        data object DEC8 : OperationOne {
//            override val opcode: UByte = 0xFEu  // ModR/M /1
//            override val bits: Bits = Bits.BITS_8
//        }

    /*data object NOT : OperationOne {
        override val opcode: UByte = 0xF7u
    }

    data object NEG : OperationOne {
        override val opcode: UByte = 0xF7u
    }

    data object MUL : OperationOne {
        override val opcode: UByte = 0xF7u
    }

    data object DIV : OperationOne {
        override val opcode: UByte = 0xF7u
    }

    data object CALL : OperationOne {
        override val opcode: UByte = 0xE8u
    }

    data object RET : OperationOne {
        override val opcode: UByte = 0xC2u
    }
//        data object IMUL : OperationOne { override val opcode: UByte = 0xF7u }
//        data object IDIV : OperationOne { override val opcode: UByte = 0xF7u }
//        data object INC8 : OperationOne { override val opcode: UByte = 0xFEu }
//        data object DEC8 : OperationOne { override val opcode: UByte = 0xFEu }
//        data object NOT8 : OperationOne { override val opcode: UByte = 0xF6u }
//        data object NEG8 : OperationOne { override val opcode: UByte = 0xF6u }
//        data object MUL8 : OperationOne { override val opcode: UByte = 0xF6u }
//        data object IMUL8 : OperationOne { override val opcode: UByte = 0xF6u }
//        data object DIV8 : OperationOne { override val opcode: UByte = 0xF6u }
//        data object IDIV8 : OperationOne { override val opcode: UByte = 0xF6u }

    // Unconditional Jump
    data object JMP : OperationOne {
        override val opcode: UByte = 0xE9u
    }

    data object JO : OperationOne {
        override val opcode: UByte = 0x70u
    }

    data object JNO : OperationOne {
        override val opcode: UByte = 0x71u
    }

    data object JB : OperationOne {
        override val opcode: UByte = 0x72u
    }

    data object JC : OperationOne {
        override val opcode: UByte = 0x72u
    }

    data object JNAE : OperationOne {
        override val opcode: UByte = 0x72u
    }

    data object JAE : OperationOne {
        override val opcode: UByte = 0x73u
    }

    data object JNB : OperationOne {
        override val opcode: UByte = 0x73u
    }

    data object JNC : OperationOne {
        override val opcode: UByte = 0x73u
    }

    data object JE : OperationOne {
        override val opcode: UByte = 0x74u
    }

    data object JZ : OperationOne {
        override val opcode: UByte = 0x74u
    }

    data object JNE : OperationOne {
        override val opcode: UByte = 0x75u
    }

    data object JNZ : OperationOne {
        override val opcode: UByte = 0x75u
    }

    data object JBE : OperationOne {
        override val opcode: UByte = 0x76u
    }

    data object JNA : OperationOne {
        override val opcode: UByte = 0x76u
    }

    data object JA : OperationOne {
        override val opcode: UByte = 0x77u
    }

    data object JNBE : OperationOne {
        override val opcode: UByte = 0x77u
    }

    data object JS : OperationOne {
        override val opcode: UByte = 0x78u
    }

    data object JNS : OperationOne {
        override val opcode: UByte = 0x79u
    }

    data object JP : OperationOne {
        override val opcode: UByte = 0x7Au
    }

    data object JPE : OperationOne {
        override val opcode: UByte = 0x7Au
    }

    data object JNP : OperationOne {
        override val opcode: UByte = 0x7Bu
    }

    data object JPO : OperationOne {
        override val opcode: UByte = 0x7Bu
    }

    data object JL : OperationOne {
        override val opcode: UByte = 0x7Cu
    }

    data object JNGE : OperationOne {
        override val opcode: UByte = 0x7Cu
    }

    data object JGE : OperationOne {
        override val opcode: UByte = 0x7Du
    }

    data object JNL : OperationOne {
        override val opcode: UByte = 0x7Du
    }

    data object JLE : OperationOne {
        override val opcode: UByte = 0x7Eu
    }

    data object JNG : OperationOne {
        override val opcode: UByte = 0x7Eu
    }

    data object JG : OperationOne {
        override val opcode: UByte = 0x7Fu
    }

    data object JNLE : OperationOne {
        override val opcode: UByte = 0x7Fu
    }
//        // Less / Greater or Equal (unsigned)
//        data object JCXZ  : OperationOne { override val opcode: UByte = 0xE3u } // Jump if CX=0 (16-bit)
//        data object JECXZ : OperationOne { override val opcode: UByte = 0xE3u } // Jump if ECX=0 (32-bit)

    // Loops
    data object LOOP : OperationOne {
        override val opcode: UByte = 0xE2u
    }

    data object LOOPZ : OperationOne {
        override val opcode: UByte = 0xE1u
    }

    data object LOOPE : OperationOne {
        override val opcode: UByte = 0xE1u
    }

    data object LOOPNZ : OperationOne {
        override val opcode: UByte = 0xE0u
    }

    data object LOOPNE : OperationOne {
        override val opcode: UByte = 0xE0u
    }

    // String Instructions (implied operands, one-operand form)
//        data object MOVS : OperationOne { override val opcode: UByte = 0xA4u } // Move string
//        data object MOVZ : OperationOne { override val opcode: UByte = 0x0FBu } // Example for MOVZX/MOVSX (prefix + opcode)
//        data object LEA  : OperationOne { override val opcode: UByte = 0x8Du } // Load effective address

    // Logical Instructions (one operand via ModR/M)
    data object AND : OperationOne {
        override val opcode: UByte = 0x24u
    }

    data object OR : OperationOne {
        override val opcode: UByte = 0x08u
    }

    data object XOR : OperationOne {
        override val opcode: UByte = 0x30u
    }

    data object TEST : OperationOne {
        override val opcode: UByte = 0xA8u
    }

    // Shift/Rotate Instructions (shared opcode objects)
    data object SHL : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object SAL : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object SHR : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object SAR : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object ROL : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object ROR : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object RCL : OperationOne {
        override val opcode: UByte = 0xD0u
    }

    data object RCR : OperationOne {
        override val opcode: UByte = 0xD0u
    }*/

}