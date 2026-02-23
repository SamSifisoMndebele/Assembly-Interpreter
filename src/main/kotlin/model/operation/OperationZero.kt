package model.operation

import model.Bits

/**
 * Represents operations that do not take any operands.
 * These are typically single-byte instructions.
 */
sealed interface OperationZero : Operation {
    data object NOP : OperationZero {
        override val opcode: UByte = 0x90u
        override val bits: Bits = Bits.B32
    }

    data object RET : OperationZero {
        override val opcode: UByte = 0xC3u
        override val bits: Bits = Bits.B32
    }

    data object EXIT : OperationZero {
        override val opcode: UByte = 0x00u
        override val bits: Bits = Bits.B32
    }

    data object PUSHA : OperationZero {
        override val opcode: UByte = 0x60u
        override val bits: Bits = Bits.B16 // 16-bit registers
    }

    data object PUSHAD : OperationZero {
        override val opcode: UByte = 0x60u
        override val bits: Bits = Bits.B32 // 32-bit registers
    }

    data object POPA : OperationZero {
        override val opcode: UByte = 0x61u
        override val bits: Bits = Bits.B16 // 16-bit registers
    }

    data object POPAD : OperationZero {
        override val opcode: UByte = 0x61u
        override val bits: Bits = Bits.B32 // 32-bit registers
    }

}