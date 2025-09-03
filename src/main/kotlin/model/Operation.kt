package model

sealed interface Operation {
    val opcode: Byte
    sealed interface OperationZero: Operation {
        data object NOP : OperationZero {
            override val opcode: Byte = 0x90.toByte()
        }
        data object PUSHA : OperationZero {
            override val opcode: Byte = 0x60.toByte()
        }
        data object POPA : OperationZero {
            override val opcode: Byte = 0x61.toByte()
        }
        data object RET : OperationZero {
            override val opcode: Byte = 0xC3.toByte()
        }
//        data object PUSHAD : OperationZero
//        data object POPAD : OperationZero
//        data object EXIT : OperationZero
//        data object CMPS : OperationZero
//        data object SCAS : OperationZero
//        data object LODS : OperationZero
//        data object STOS : OperationZero
    }
    sealed interface OperationOne: Operation {
        // For operations like PUSH, POP, INC, DEC that can operate on a register,
        // the opcode often incorporates the register. Example: PUSH EAX is 0x50, PUSH ECX is 0x51.
        // So, the 'base' opcode is defined here, and encoding will add the register code.
        // Alternatively, you can have distinct objects like PUSH_REG, PUSH_IMM, PUSH_MEM.
        // Let's assume for now a base opcode that might be modified or part of a ModR/M scheme.

        data object PUSH : OperationOne {
            // This is tricky. PUSH r32 is 0x50+reg. PUSH imm32 is 0x68. PUSH r/m32 is 0xFF /6.
            // Let's use a placeholder and decide in the encoder/decoder.
            // Or, you make PUSH_REG, PUSH_IMM, PUSH_MEM distinct Operation objects.
            // For now, let's assume this 'PUSH' implies we need to look at the operand.
            // We'll primarily use specific opcodes defined in the constants for now,
            // and this 'opcode' field in the Operation object might serve as a primary byte if simple.
            override val opcode: Byte = 0xFF.toByte() // Placeholder, often PUSH r/m32 uses FF /6
        }
        data object POP : OperationOne {
            override val opcode: Byte = 0x8F.toByte() // Placeholder, POP r/m32 uses 8F /0
        }
        data object INC : OperationOne {
            override val opcode: Byte = 0xFE.toByte() // Placeholder, INC r/m32 uses FE /0
        }
        data object DEC : OperationOne {
            override val opcode: Byte = 0xFE.toByte() // Placeholder, DEC r/m32 uses FE /1
        }
        data object CALL : OperationOne {
            override val opcode: Byte = 0xE8.toByte() // Placeholder for CALL rel32. CALL r/m32 is FF /2
        }
//        data object MUL : OperationOne
//        data object IMUL : OperationOne
//        data object DIV : OperationOne
//        data object IDIV : OperationOne
//        data object NEG : OperationOne
//        data object NOT : OperationOne
//        data object INT : OperationOne
        data object LOOP : OperationOne {
            override val opcode: Byte = 0xE2.toByte() // LOOP rel8
        }
        data object JMP : OperationOne {
            override val opcode: Byte = 0xE9.toByte() // Placeholder for JMP rel32. JMP r/m32 is FF /4
        }
        data object JZ : OperationOne { // JE
            override val opcode: Byte = 0x74.toByte() // JZ rel8 (JE)
        }
        data object JNZ : OperationOne { // JNE
            override val opcode: Byte = 0x75.toByte() // JNZ rel8 (JNE)
        }
        data object JG : OperationOne { // JNLE
            override val opcode: Byte = 0x7F.toByte() // JG rel8 (JNLE)
        }
//        data object JE : OperationOne
//        data object JNE : OperationOne
//        data object JL : OperationOne // JNGE
//        data object JA : OperationOne // JNBE
//        data object JB : OperationOne // JNAE
//        data object JGE : OperationOne
//        data object JNL : OperationOne
//        data object JLE : OperationOne
//        data object JNG : OperationOne
//        data object JAE : OperationOne
//        data object JNB : OperationOne
//        data object JBE : OperationOne
//        data object JNA : OperationOne
//        data object JO : OperationOne
//        data object JNO : OperationOne
//        data object JS : OperationOne
//        data object JNS : OperationOne
//        data object JP : OperationOne
//        data object JPE : OperationOne
//        data object JNP : OperationOne
//        data object JPO : OperationOne
//        data object JCXZ : OperationOne
//        data object JECXZ : OperationOne
//        data object LOOPZ : OperationOne
//        data object LOOPE : OperationOne
//        data object LOOPNZ : OperationOne
//        data object LOOPNE : OperationOne
    }
    sealed interface OperationTwo: Operation {
        data object MOV : OperationTwo {
            // MOV has many forms: r/m,reg (89), reg,r/m (8B), r,imm (B8+reg)
            override val opcode: Byte = 0x89.toByte() // Placeholder, common for MOV r/m32, r32
        }
        data object ADD : OperationTwo {
            override val opcode: Byte = 0x01.toByte() // Placeholder, common for ADD r/m32, r32
        }
        data object SUB : OperationTwo {
            override val opcode: Byte = 0x29.toByte() // Placeholder, common for SUB r/m32, r32
        }
        data object XCHG : OperationTwo {
            override val opcode: Byte = 0x87.toByte() // Placeholder, XCHG r/m32, r32. XCHG EAX,r32 is 90+r
        }
        data object CMP : OperationTwo {
            override val opcode: Byte = 0x39.toByte() // Placeholder, common for CMP r/m32, r32
        }
//        data object MOVS : OperationZero
//        data object MOVZ : OperationZero
//        data object LEA : OperationTwo
//        data object AND : OperationTwo
//        data object OR : OperationTwo
//        data object XOR : OperationTwo
//        data object TEST : OperationTwo
//        data object SHL : OperationTwo
//        data object SAL : OperationTwo
//        data object SHR : OperationTwo
//        data object SAR : OperationTwo
//        data object ROL : OperationTwo
//        data object ROR : OperationTwo
//        data object RCL : OperationTwo
//        data object RCR : OperationTwo
    }
}