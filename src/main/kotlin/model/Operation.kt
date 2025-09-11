package model

/**
 * Represents a generic CPU operation.
 * This is a sealed interface, meaning all possible implementations are defined within this file.
 * Operations are further categorized by the number of operands they take:
 *  - [OperationZero]: Operations with no explicit operands.
 *  - [OperationOne]: Operations with one explicit operand.
 *  - [OperationTwo]: Operations with two explicit operands.
 *
 * Each operation has an associated [opcode].
 */
sealed interface Operation {
    val opcode: UByte
    /**
     * Represents operations that do not take any operands.
     * These are typically single-byte instructions.
     */
    sealed interface OperationZero: Operation {
        data object NOP : OperationZero { override val opcode: UByte = 0x90u }
        data object RET : OperationZero { override val opcode: UByte = 0xC3u }
        data object EXIT : OperationZero { override val opcode: UByte = 0xF0u }
        data object PUSHA : OperationZero { override val opcode: UByte = 0x60u }
        data object PUSHAD : OperationZero { override val opcode: UByte = 0x60u }
        data object POPA : OperationZero { override val opcode: UByte = 0x61u }
        data object POPAD : OperationZero { override val opcode: UByte = 0x61u }
//        data object PUSHF : OperationZero { override val opcode: UByte = 0x9Cu }
//        data object POPF : OperationZero { override val opcode: UByte = 0x9Du }
//        data object HLT : OperationZero { override val opcode: UByte = 0xF4u }
//        data object CMC : OperationZero { override val opcode: UByte = 0xF5u }
//        data object STC : OperationZero { override val opcode: UByte = 0xF9u }
//        data object CLC : OperationZero { override val opcode: UByte = 0xF8u }
//        data object IRET : OperationZero { override val opcode: UByte = 0xCFu }
//        data object INT3 : OperationZero { override val opcode: UByte = 0xCCu }
//        data object WAIT : OperationZero { override val opcode: UByte = 0x9Bu }
//        data object CLD : OperationZero { override val opcode: UByte = 0xFCu }
//        data object STD : OperationZero { override val opcode: UByte = 0xFDu }
//        data object CLI : OperationZero { override val opcode: UByte = 0xFAu }
//        data object STI : OperationZero { override val opcode: UByte = 0xFBu }
//        data object CMPS : OperationZero { override val opcode: UByte = 0xA6u }
//        data object SCAS : OperationZero { override val opcode: UByte = 0xAEu }
//        data object LODS : OperationZero { override val opcode: UByte = 0xACu }
//        data object STOS : OperationZero { override val opcode: UByte = 0xAAu }

    }
    /**
     * Represents operations that typically take one operand.
     * One operand can be a single register, memory location, or immediate value.
     *
     * Note: Some instructions listed here might have variations that take two operands
     * or an immediate value. This interface primarily covers their one-operand forms.
     */
    sealed interface OperationOne: Operation {
        data object INC : OperationOne { override val opcode: UByte = 0xFFu }
        data object DEC : OperationOne { override val opcode: UByte = 0xFFu }
        data object NOT : OperationOne { override val opcode: UByte = 0xF7u }
        data object NEG : OperationOne { override val opcode: UByte = 0xF7u }
        data object MUL : OperationOne { override val opcode: UByte = 0xF7u }
        data object DIV : OperationOne { override val opcode: UByte = 0xF7u }
        data object POP : OperationOne { override val opcode: UByte = 0x8Fu }
        data object PUSH : OperationOne { override val opcode: UByte = 0xFFu }
        data object CALL : OperationOne { override val opcode: UByte = 0xE8u }
        data object RET : OperationOne { override val opcode: UByte = 0xC2u }
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
        data object JMP : OperationOne { override val opcode: UByte = 0xE9u }
        data object JO   : OperationOne { override val opcode: UByte = 0x70u }
        data object JNO  : OperationOne { override val opcode: UByte = 0x71u }
        data object JB   : OperationOne { override val opcode: UByte = 0x72u }
        data object JC   : OperationOne { override val opcode: UByte = 0x72u }
        data object JNAE   : OperationOne { override val opcode: UByte = 0x72u }
        data object JAE  : OperationOne { override val opcode: UByte = 0x73u }
        data object JNB  : OperationOne { override val opcode: UByte = 0x73u }
        data object JNC  : OperationOne { override val opcode: UByte = 0x73u }
        data object JE   : OperationOne { override val opcode: UByte = 0x74u }
        data object JZ   : OperationOne { override val opcode: UByte = 0x74u }
        data object JNE  : OperationOne { override val opcode: UByte = 0x75u }
        data object JNZ  : OperationOne { override val opcode: UByte = 0x75u }
        data object JBE  : OperationOne { override val opcode: UByte = 0x76u }
        data object JNA  : OperationOne { override val opcode: UByte = 0x76u }
        data object JA   : OperationOne { override val opcode: UByte = 0x77u }
        data object JNBE   : OperationOne { override val opcode: UByte = 0x77u }
        data object JS   : OperationOne { override val opcode: UByte = 0x78u }
        data object JNS  : OperationOne { override val opcode: UByte = 0x79u }
        data object JP   : OperationOne { override val opcode: UByte = 0x7Au }
        data object JPE   : OperationOne { override val opcode: UByte = 0x7Au }
        data object JNP  : OperationOne { override val opcode: UByte = 0x7Bu }
        data object JPO  : OperationOne { override val opcode: UByte = 0x7Bu }
        data object JL   : OperationOne { override val opcode: UByte = 0x7Cu }
        data object JNGE   : OperationOne { override val opcode: UByte = 0x7Cu }
        data object JGE  : OperationOne { override val opcode: UByte = 0x7Du }
        data object JNL  : OperationOne { override val opcode: UByte = 0x7Du }
        data object JLE  : OperationOne { override val opcode: UByte = 0x7Eu }
        data object JNG  : OperationOne { override val opcode: UByte = 0x7Eu }
        data object JG   : OperationOne { override val opcode: UByte = 0x7Fu }
        data object JNLE   : OperationOne { override val opcode: UByte = 0x7Fu }
//        // Less / Greater or Equal (unsigned)
//        data object JCXZ  : OperationOne { override val opcode: UByte = 0xE3u } // Jump if CX=0 (16-bit)
//        data object JECXZ : OperationOne { override val opcode: UByte = 0xE3u } // Jump if ECX=0 (32-bit)

        // Loops
        data object LOOP    : OperationOne { override val opcode: UByte = 0xE2u }
        data object LOOPZ   : OperationOne { override val opcode: UByte = 0xE1u }
        data object LOOPE   : OperationOne { override val opcode: UByte = 0xE1u }
        data object LOOPNZ  : OperationOne { override val opcode: UByte = 0xE0u }
        data object LOOPNE  : OperationOne { override val opcode: UByte = 0xE0u }

        // String Instructions (implied operands, one-operand form)
//        data object MOVS : OperationOne { override val opcode: UByte = 0xA4u } // Move string
//        data object MOVZ : OperationOne { override val opcode: UByte = 0x0FBu } // Example for MOVZX/MOVSX (prefix + opcode)
//        data object LEA  : OperationOne { override val opcode: UByte = 0x8Du } // Load effective address

        // Logical Instructions (one operand via ModR/M)
        data object AND : OperationOne { override val opcode: UByte = 0x24u }
        data object OR  : OperationOne { override val opcode: UByte = 0x08u }
        data object XOR : OperationOne { override val opcode: UByte = 0x30u }
        data object TEST: OperationOne { override val opcode: UByte = 0xA8u }

        // Shift/Rotate Instructions (shared opcode objects)
        data object SHL : OperationOne { override val opcode: UByte = 0xD0u }
        data object SAL : OperationOne { override val opcode: UByte = 0xD0u }
        data object SHR : OperationOne { override val opcode: UByte = 0xD0u }
        data object SAR : OperationOne { override val opcode: UByte = 0xD0u }
        data object ROL : OperationOne { override val opcode: UByte = 0xD0u }
        data object ROR : OperationOne { override val opcode: UByte = 0xD0u }
        data object RCL : OperationOne { override val opcode: UByte = 0xD0u }
        data object RCR : OperationOne { override val opcode: UByte = 0xD0u }

    }
    /**
     * Represents operations that involve two operands, typically a register and a register/memory location.
     *
     * Note: Some instructions might have variations that take one operand.
     * This interface primarily covers their two-operands forms.
     */
    sealed interface OperationTwo: Operation {
        // Data Movement
        data object MOV   : OperationTwo { override val opcode: UByte = 0x89u }
        data object XCHG  : OperationTwo { override val opcode: UByte = 0x87u }
        data object MOVZX : OperationTwo { override val opcode: UByte = 0xB6u }
        data object MOVSX : OperationTwo { override val opcode: UByte = 0xBEu }
//        data object MOVR  : OperationTwo { override val opcode: UByte = 0x8Bu } // Move r/m32 -> r32 (reverse)
//        data object MOVZXR: OperationOne { override val opcode: UByte = 0xB7u } // Move r/m16 -> r32 with zero-extend
//        data object MOVSXR: OperationOne { override val opcode: UByte = 0xBFu } // Move r/m16 -> r32 with sign-extend


        // Arithmetic Instructions
        data object ADD   : OperationTwo { override val opcode: UByte = 0x01u }
        data object SUB   : OperationTwo { override val opcode: UByte = 0x29u }
        data object CMP   : OperationTwo { override val opcode: UByte = 0x39u }
//        data object ADC   : OperationTwo { override val opcode: UByte = 0x11u } // Add with carry: r32 + r/m32 -> r/m32
//        data object SBB   : OperationTwo { override val opcode: UByte = 0x19u } // Subtract with borrow: r/m32 - r32 -> r/m32
//        data object ADDR  : OperationTwo { override val opcode: UByte = 0x03u } // r/m32 + r32 -> r32 (reverse)
//        data object SUBR  : OperationTwo { override val opcode: UByte = 0x2Bu } // r32 - r/m32 -> r32
//        data object CMPR  : OperationTwo { override val opcode: UByte = 0x3Bu } // Compare r32 with r/m32
//        data object ADCR  : OperationTwo { override val opcode: UByte = 0x13u } // Add with carry: r/m32 + r32 -> r32
//        data object SBBR  : OperationTwo { override val opcode: UByte = 0x1Bu } // Subtract with borrow: r32 - r/m32 -> r32

        // Logical Instructions
        data object AND   : OperationTwo { override val opcode: UByte = 0x21u } // r32 AND r/m32 -> r/m32
        data object OR    : OperationTwo { override val opcode: UByte = 0x09u } // r32 OR r/m32 -> r/m32
        data object XOR   : OperationTwo { override val opcode: UByte = 0x31u } // r32 XOR r/m32 -> r/m32
//        data object ANDR  : OperationTwo { override val opcode: UByte = 0x23u } // r/m32 AND r32 -> r32
//        data object ORR   : OperationTwo { override val opcode: UByte = 0x0Bu } // r/m32 OR r32 -> r32
//        data object XORR  : OperationTwo { override val opcode: UByte = 0x33u } // r/m32 XOR r32 -> r32


    }

    companion object {
        val allOperations: Set<Operation>
            get() {
                val opClasses = listOf(
                    OperationZero::class,
                    OperationOne::class,
                    OperationTwo::class
                )
                return opClasses.flatMap { opClass ->
                    opClass.nestedClasses.map { it.objectInstance as Operation }
                }.toSet()
            }

    }
}