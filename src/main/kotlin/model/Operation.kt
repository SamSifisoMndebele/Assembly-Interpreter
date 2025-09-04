package model

sealed interface Operation {
    val opcode: UByte
    sealed interface OperationZero: Operation {
        //No Operation
        data object NOP : OperationZero {
            override val opcode: UByte = 0x90u
        }
        //Return from procedure (near return)
        data object RET : OperationZero {
            override val opcode: UByte = 0xC3u
        }
        //Halt CPU
        data object HLT : OperationZero {
            override val opcode: UByte = 0xF4u
        }
        //Complement Carry Flag
        data object CMC : OperationZero {
            override val opcode: UByte = 0xF5u
        }
        //Set Carry Flag
        data object STC : OperationZero {
            override val opcode: UByte = 0xF9u
        }
        // Clear Carry Flag
        data object CLC : OperationZero {
            override val opcode: UByte = 0xF8u
        }
        //Push FLAGS/EFLAGS/RFLAGS
        data object PUSHF : OperationZero {
            override val opcode: UByte = 0x9Cu
        }
        //Pop into FLAGS/EFLAGS/RFLAGS
        data object POPF : OperationZero {
            override val opcode: UByte = 0x9Du
        }
        //Push All General-Purpose Registers (32-bit)
        data object PUSHAD : OperationZero {
            override val opcode: UByte = 0x60u
        }
        //Push All General-Purpose Registers (16-bit)
        data object PUSHA : OperationZero {
            override val opcode: UByte = 0x60u // same byte, operand-size prefix decides 16/32
        }
        //Pop All General-Purpose Registers (16-bit)
        data object POPA : OperationZero {
            override val opcode: UByte = 0x61u
        }
        //Pop All General-Purpose Registers (32-bit)
        data object POPAD : OperationZero {
            override val opcode: UByte = 0x61u // same byte, operand-size prefix decides 16/32
        }
        //Custom "EXIT" instruction (not real x86, could be emulator-defined)
        data object EXIT : OperationZero {
            override val opcode: UByte = 0xF0u // example placeholder, can be any unused opcode
        }
        //Return from interrupt
        data object IRET : OperationZero {
            override val opcode: UByte = 0xCFu
        }

        //Software breakpoint (INT 3)
        data object INT3 : OperationZero {
            override val opcode: UByte = 0xCCu
        }

        //Wait for FPU (alias FWAIT)
        data object WAIT : OperationZero {
            override val opcode: UByte = 0x9Bu
        }
        //Clear Direction Flag
        data object CLD : OperationZero {
            override val opcode: UByte = 0xFCu
        }
        //Set Direction Flag
        data object STD : OperationZero {
            override val opcode: UByte = 0xFDu
        }
        //Clear Interrupt Flag
        data object CLI : OperationZero {
            override val opcode: UByte = 0xFAu
        }
        //Set Interrupt Flag
        data object STI : OperationZero {
            override val opcode: UByte = 0xFBu
        }
        //Compare String (byte/word/dword depending on operand size)
        data object CMPS : OperationZero {
            override val opcode: UByte = 0xA6u
        }
        //Scan String (byte/word/dword depending on operand size)
        data object SCAS : OperationZero {
            override val opcode: UByte = 0xAEu
        }
        //Load String into Accumulator
        data object LODS : OperationZero {
            override val opcode: UByte = 0xACu
        }
        //Store Accumulator into String
        data object STOS : OperationZero {
            override val opcode: UByte = 0xAAu
        }

    }
    sealed interface OperationOne: Operation {
        data object INC8 : OperationOne { override val opcode: UByte = 0xFEu }
        data object INC : OperationOne { override val opcode: UByte = 0xFFu }
        data object DEC8 : OperationOne { override val opcode: UByte = 0xFEu }
        data object DEC : OperationOne { override val opcode: UByte = 0xFFu }
        data object NOT8 : OperationOne { override val opcode: UByte = 0xF6u }
        data object NOT : OperationOne { override val opcode: UByte = 0xF7u }
        data object NEG8 : OperationOne { override val opcode: UByte = 0xF6u }
        data object NEG : OperationOne { override val opcode: UByte = 0xF7u }
        data object MUL8 : OperationOne { override val opcode: UByte = 0xF6u }
        data object MUL : OperationOne { override val opcode: UByte = 0xF7u }
        data object IMUL8 : OperationOne { override val opcode: UByte = 0xF6u }
        data object IMUL : OperationOne { override val opcode: UByte = 0xF7u }
        data object DIV8 : OperationOne { override val opcode: UByte = 0xF6u }
        data object DIV : OperationOne { override val opcode: UByte = 0xF7u }
        data object IDIV8 : OperationOne { override val opcode: UByte = 0xF6u }
        data object IDIV32 : OperationOne { override val opcode: UByte = 0xF7u }
        data object POP : OperationOne { override val opcode: UByte = 0x8Fu }
        data object PUSH : OperationOne { override val opcode: UByte = 0xFFu }
        data object CALL : OperationOne { override val opcode: UByte = 0xE8u }
        data object RET : OperationOne { override val opcode: UByte = 0xC2u }

        // Unconditional Jump
        data object JMP : OperationOne { override val opcode: UByte = 0xE9u }
        data object JZ : OperationOne { override val opcode: UByte = 0x74u }
        data object JNZ : OperationOne { override val opcode: UByte = 0x75u }

        // Overflow / No Overflow
        data object JO  : OperationOne { override val opcode: UByte = 0x70u } // Jump if Overflow
        data object JNO : OperationOne { override val opcode: UByte = 0x71u } // Jump if Not Overflow

        // Carry / No Carry
        data object JB  : OperationOne { override val opcode: UByte = 0x72u } // Jump if Below / Carry
        data object JAE : OperationOne { override val opcode: UByte = 0x73u } // Jump if Above or Equal / Not Carry

        // Zero / Not Zero
        data object JE  : OperationOne { override val opcode: UByte = 0x74u } // Jump if Equal / Zero
        data object JNE : OperationOne { override val opcode: UByte = 0x75u } // Jump if Not Equal / Not Zero

        // Sign / Not Sign
        data object JS  : OperationOne { override val opcode: UByte = 0x78u } // Jump if Sign
        data object JNS : OperationOne { override val opcode: UByte = 0x79u } // Jump if Not Sign

        // Parity / No Parity
        data object JP  : OperationOne { override val opcode: UByte = 0x7Au } // Jump if Parity / Even
        data object JNP : OperationOne { override val opcode: UByte = 0x7Bu } // Jump if No Parity / Odd

        // Less / Greater or Equal (signed)
        data object JL  : OperationOne { override val opcode: UByte = 0x7Cu } // Jump if Less / JNGE
        data object JGE : OperationOne { override val opcode: UByte = 0x7Du } // Jump if Greater or Equal / JNL

        // Less or Equal / Greater (signed)
        data object JLE : OperationOne { override val opcode: UByte = 0x7Eu } // Jump if Less or Equal / JNG
        data object JG  : OperationOne { override val opcode: UByte = 0x7Fu } // Jump if Greater / JNLE

        // Less / Greater or Equal (unsigned)
        data object JCXZ  : OperationOne { override val opcode: UByte = 0xE3u } // Jump if CX=0 (16-bit)
        data object JECXZ : OperationOne { override val opcode: UByte = 0xE3u } // Jump if ECX=0 (32-bit)

        // Loops
        data object LOOP    : OperationOne { override val opcode: UByte = 0xE2u } // Decrement CX/ECX and jump
        data object LOOPZ   : OperationOne { override val opcode: UByte = 0xE1u } // LOOPE: loop while zero
        data object LOOPE   : OperationOne { override val opcode: UByte = 0xE1u } // alias
        data object LOOPNZ  : OperationOne { override val opcode: UByte = 0xE0u } // LOOPNE: loop while not zero
        data object LOOPNE  : OperationOne { override val opcode: UByte = 0xE0u } // alias

        // String Instructions (implied operands, one-operand form)
        data object MOVS : OperationOne { override val opcode: UByte = 0xA4u } // Move string
        data object MOVZ : OperationOne { override val opcode: UByte = 0x0FBu } // Example for MOVZX/MOVSX (prefix + opcode)
        data object LEA  : OperationOne { override val opcode: UByte = 0x8Du } // Load effective address

        // Logical Instructions (one operand via ModR/M)
        data object AND : OperationOne { override val opcode: UByte = 0x24u } // 8-bit AL, imm8; 0x25 AX/EAX, imm16/32
        data object OR  : OperationOne { override val opcode: UByte = 0x08u } // r/m8,r8 or 0x09 r/m16/32,r16/32
        data object XOR : OperationOne { override val opcode: UByte = 0x30u } // r/m8,r8 or 0x31 r/m16/32,r16/32
        data object TEST: OperationOne { override val opcode: UByte = 0xA8u } // AL/AX/EAX, imm8/16/32

        // Shift/Rotate Instructions (shared opcode objects)
        data object SHL : OperationOne { override val opcode: UByte = 0xD0u } // /4 r/m8, D1/D3 for 16/32-bit
        data object SAL : OperationOne { override val opcode: UByte = 0xD0u } // SAL is same as SHL
        data object SHR : OperationOne { override val opcode: UByte = 0xD0u } // /5 r/m8, D1/D3 for 16/32-bit
        data object SAR : OperationOne { override val opcode: UByte = 0xD0u } // /7 r/m8, D1/D3 for 16/32-bit
        data object ROL : OperationOne { override val opcode: UByte = 0xD0u } // /0 r/m8, D1/D3 for 16/32-bit
        data object ROR : OperationOne { override val opcode: UByte = 0xD0u } // /1 r/m8, D1/D3 for 16/32-bit
        data object RCL : OperationOne { override val opcode: UByte = 0xD0u } // /2 r/m8, D1/D3 for 16/32-bit
        data object RCR : OperationOne { override val opcode: UByte = 0xD0u } // /3 r/m8, D1/D3 for 16/32-bit

    }
    sealed interface OperationTwo: Operation {
        // Data Movement
        data object MOV   : OperationTwo { override val opcode: UByte = 0x89u } // Move r32 -> r/m32
        data object MOVR  : OperationTwo { override val opcode: UByte = 0x8Bu } // Move r/m32 -> r32 (reverse)
        data object MOVZX : OperationOne { override val opcode: UByte = 0xB6u } // Move r/m8 -> r16/32 with zero-extend
        data object MOVZXR: OperationOne { override val opcode: UByte = 0xB7u } // Move r/m16 -> r32 with zero-extend
        data object MOVSX : OperationOne { override val opcode: UByte = 0xBEu } // Move r/m8 -> r16/32 with sign-extend
        data object MOVSXR: OperationOne { override val opcode: UByte = 0xBFu } // Move r/m16 -> r32 with sign-extend
        data object XCHG  : OperationTwo { override val opcode: UByte = 0x87u } // Swap r32 and r/m32


        // Arithmetic Instructions
        data object ADD   : OperationTwo { override val opcode: UByte = 0x01u } // r32 + r/m32 -> r/m32
        data object ADDR  : OperationTwo { override val opcode: UByte = 0x03u } // r/m32 + r32 -> r32 (reverse)
        data object SUB   : OperationTwo { override val opcode: UByte = 0x29u } // r/m32 - r32 -> r/m32
        data object SUBR  : OperationTwo { override val opcode: UByte = 0x2Bu } // r32 - r/m32 -> r32
        data object CMP   : OperationTwo { override val opcode: UByte = 0x39u } // Compare r/m32 with r32 (flags)
        data object CMPR  : OperationTwo { override val opcode: UByte = 0x3Bu } // Compare r32 with r/m32
        data object ADC   : OperationTwo { override val opcode: UByte = 0x11u } // Add with carry: r32 + r/m32 -> r/m32
        data object ADCR  : OperationTwo { override val opcode: UByte = 0x13u } // Add with carry: r/m32 + r32 -> r32
        data object SBB   : OperationTwo { override val opcode: UByte = 0x19u } // Subtract with borrow: r/m32 - r32 -> r/m32
        data object SBBR  : OperationTwo { override val opcode: UByte = 0x1Bu } // Subtract with borrow: r32 - r/m32 -> r32

        // Logical Instructions
        data object AND   : OperationTwo { override val opcode: UByte = 0x21u } // r32 AND r/m32 -> r/m32
        data object ANDR  : OperationTwo { override val opcode: UByte = 0x23u } // r/m32 AND r32 -> r32
        data object OR    : OperationTwo { override val opcode: UByte = 0x09u } // r32 OR r/m32 -> r/m32
        data object ORR   : OperationTwo { override val opcode: UByte = 0x0Bu } // r/m32 OR r32 -> r32
        data object XOR   : OperationTwo { override val opcode: UByte = 0x31u } // r32 XOR r/m32 -> r/m32
        data object XORR  : OperationTwo { override val opcode: UByte = 0x33u } // r/m32 XOR r32 -> r32


    }
}