@file:OptIn(ExperimentalUnsignedTypes::class)

package asm

import model.Instruction
import model.Operand
import model.Operation
import model.Reg

fun encode(instruction: Instruction): UByteArray = when (instruction) {
    is Instruction.InstructionZero -> when (instruction.operation) {
        Operation.OperationZero.NOP -> ubyteArrayOf(0x90.toUByte())
        else -> error("Unsupported zero-op instruction: ${instruction.operation}")
    }

    is Instruction.InstructionOne -> when (instruction.operation) {
        Operation.OperationOne.PUSH -> {
            val imm = (instruction.operand as Operand.Immediate).value
            ubyteArrayOf(0x68.toUByte()) + uIntToBytes(imm)
        }
        else -> error("Unsupported one-op instruction: ${instruction.operation}")
    }

    is Instruction.InstructionTwo -> when (instruction.operation) {
        Operation.OperationTwo.MOV -> {
            val dst = instruction.dst
            val src = instruction.src
            when (dst) {
                is Operand.Register -> {
                    when (src) {
                        is Operand.Immediate -> { // MOV reg32, imm32
                            val imm = src.value
                            // For MOV r32, imm32, the register is part of the opcode: 0xB8+reg
                            val opcode = (0xB8 + dst.reg.code.toInt()).toUByte()
                            ubyteArrayOf(opcode) + uIntToBytes(imm)
                        }
                        is Operand.Register -> { // MOV r/m32, r32  (here specifically MOV reg_dst, reg_src)
                            // Opcode 89 /r: MOV r/m32, r32
                            // ModR/M byte: mod=11 (register direct)
                            // reg field contains source register (src.reg.code)
                            // r/m field contains destination register (dst.reg.code)
                            val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                            ubyteArrayOf(0x89.toUByte(), modRM)
                        }
                        is Operand.Memory -> TODO("MOV reg32, mem32 not implemented yet")
                        is Operand.LabelOp -> TODO("MOV reg32, mem32 not implemented yet")
                    }
                }
                is Operand.Memory -> error("MOV mem32, ... not implemented yet") // TODO
                else -> error("Unsupported MOV destination operand: $dst")
            }
        }
        else -> error("Unsupported two-op instruction: ${instruction.operation}")
    }
}

private fun uIntToBytes(value: UInt): UByteArray =
    ubyteArrayOf(
        (value and 0xFFu).toUByte(),
        ((value shr 8) and 0xFFu).toUByte(),
        ((value shr 16) and 0xFFu).toUByte(),
        ((value shr 24) and 0xFFu).toUByte()
    )


fun decode(bytes: UByteArray): List<Instruction> {
    val result = mutableListOf<Instruction>()
    var i = 0
    val r32OpcodeMap = Reg.entries
        .filter { it.name.startsWith("E") }
        .associateBy { it.code.toInt() }

    while (i < bytes.size) {
        when (val currentOpcode = bytes[i].toInt() and 0xFF) {
            0x90 -> {
                result.add(Instruction.InstructionZero(Operation.OperationZero.NOP, i))
                i += 1
            }
            0x68 -> { // PUSH imm32
                val imm = bytes.copyOfRange(i + 1, i + 5).toUInt()
                result.add(Instruction.InstructionOne(Operation.OperationOne.PUSH, Operand.Immediate(imm), i))
                i += 5
            }
            in 0xB8..0xBF -> { // MOV r32, imm32
                val regOpcode = currentOpcode - 0xB8 // Opcode contains the destination register
                val imm = bytes.copyOfRange(i + 1, i + 5).toUInt()
                val dstReg = r32OpcodeMap[regOpcode] 
                    ?: error("Unknown 32-bit destination register opcode: $regOpcode for MOV r32, imm32 instruction at offset $i")
                result.add(Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(dstReg), Operand.Immediate(imm), i))
                i += 5
            }
            0x89 -> { // MOV r/m32, r32
                val modRM = bytes[i + 1].toInt()
                val mod = (modRM shr 6) and 0b11
                if (mod != 0b11) {
                    error("Unsupported MOV addressing mode (mod != 11): ${modRM.toString(16)} at offset $i")
                }
                // For 0x89 /r: MOV r/m32, r32
                // r/m field (bits 0-2 of ModR/M) is the destination register
                // reg field (bits 3-5 of ModR/M) is the source register
                val dstRegOpcode = modRM and 0b111
                val srcRegOpcode = (modRM shr 3) and 0b111

                val dstReg = r32OpcodeMap[dstRegOpcode] 
                    ?: error("Unknown 32-bit destination register opcode: $dstRegOpcode in ModR/M ${modRM.toString(16)} at offset $i")
                val srcReg = r32OpcodeMap[srcRegOpcode] 
                    ?: error("Unknown 32-bit source register opcode: $srcRegOpcode in ModR/M ${modRM.toString(16)} at offset $i")
                
                val instr = Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(dstReg), Operand.Register(srcReg), i)
                result.add(instr)
                i += 2
            }
            else -> error("Unknown opcode: 0x${bytes[i].toUByte().toString(16)} at offset $i")
        }
    }
    return result
}

private fun UByteArray.toUInt(): UInt = (this[0].toUInt() and 0xFFu) or ((this[1].toUInt() and 0xFFu) shl 8) or
        ((this[2].toUInt() and 0xFFu) shl 16) or ((this[3].toUInt() and 0xFFu) shl 24)

fun main() {
    val instrs = listOf(
        Instruction.InstructionOne(Operation.OperationOne.PUSH, Operand.Immediate(0x12abu), 0),
        Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(Reg.EBX), Operand.Immediate(50u), 0),
        // Test case: MOV EBX, ECX (dest: EBX (opcode 3), src: ECX (opcode 1))
        // Expected ModR/M: 11 (src:001) (dst:011) = 11001011 = CB
        // Expected machine code: 89 CB
        Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(Reg.EBX), Operand.Register(Reg.ECX), 0),
        // Test case: MOV ECX, EBX (dest: ECX (opcode 1), src: EBX (opcode 3))
        // Expected ModR/M: 11 (src:011) (dst:001) = 11011001 = D9
        // Expected machine code: 89 D9
        Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(Reg.ECX), Operand.Register(Reg.EBX), 0)

    )

    val machineCodeParts = mutableListOf<String>()
    val machineCodeFull = mutableListOf<UByte>()

    println("Encoding instructions:")
    instrs.forEachIndexed { index, instruction ->
        val encodedBytes = encode(instruction)
        val hexString = encodedBytes.joinToString(" ") { "%02X".format(it.toInt()) }
        println("$index: $instruction -> $hexString")
        machineCodeParts.add(hexString)
        machineCodeFull.addAll(encodedBytes.asIterable())
    }
    
    println("\nFull Raw x86 machine code: " + machineCodeFull.toUByteArray().joinToString(" ") { "%02X".format(it.toInt()) })

    val decoded = decode(machineCodeFull.toUByteArray())
    println("\nDecoded instructions:")
    decoded.forEach(::println)

    // Test specific MOV EBX, ECX
    println("\nTesting MOV EBX, ECX (should be 89 CB):")
    val movEbxEcx = encode(Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(Reg.EBX), Operand.Register(Reg.ECX), 0))
    println("Encoded: " + movEbxEcx.joinToString(" ") { "%02X".format(it.toInt()) })
    val decodedMovEbxEcx = decode(movEbxEcx)
    println("Decoded: ${decodedMovEbxEcx.firstOrNull()}")

    // Test specific MOV ECX, EBX
    println("\nTesting MOV ECX, EBX (should be 89 D9):")
    val movEcxEbx = encode(Instruction.InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(Reg.ECX), Operand.Register(Reg.EBX), 0))
    println("Encoded: " + movEcxEbx.joinToString(" ") { "%02X".format(it.toInt()) })
    val decodedMovEcxEbx = decode(movEcxEbx)
    println("Decoded: ${decodedMovEcxEbx.firstOrNull()}")

}
