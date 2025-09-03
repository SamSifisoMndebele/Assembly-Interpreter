@file:OptIn(ExperimentalUnsignedTypes::class)

package asm

import model.Instruction
import model.Instruction.*
import model.Operand.*
import model.Operation.*
import model.Reg

fun main() {
    val instrs = listOf(
        InstructionOne(OperationOne.PUSH, Immediate(0x12abu), 1),
        InstructionTwo(OperationTwo.MOV, Register(Reg.EBX), Immediate(50u), 2),
        InstructionTwo(OperationTwo.MOV, Register(Reg.EBX), Register(Reg.ECX), 3),
        InstructionTwo(OperationTwo.MOV, Register(Reg.ECX), Register(Reg.EBX), 4),
        InstructionTwo(OperationTwo.MOV, Register(Reg.EBX), Register(Reg.ECX), 5),
        InstructionTwo(OperationTwo.MOV, Register(Reg.ECX), Register(Reg.EBX), 6),
    )

    val machineCodeParts = mutableListOf<String>()
    val machineCodeFull = mutableListOf<UByte>()

    println("\nInstructions:")
    instrs.forEachIndexed { index, instruction ->
        val encodedBytes = instruction.encode() // Changed here
        val hexString = encodedBytes.joinToString(" ") { "%02X".format(it.toInt()) }
        println("$instruction -> $hexString")
        machineCodeParts.add(hexString)
        machineCodeFull.addAll(encodedBytes.asIterable())
    }
    
    println("\n Full Raw x86 machine code: " + machineCodeFull.toUByteArray().joinToString(" ") { "%02X".format(it.toInt()) })

    val decoded = Instruction.decode(machineCodeFull.toUByteArray()) // Changed here
    println(" Decoded instructions:")
    decoded.forEach(::println)

}
