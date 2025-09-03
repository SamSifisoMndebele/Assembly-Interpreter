package cpu

import model.Operand
import model.Operand.ImmOp
import model.Operation
import model.Reg
import model.Reg.AH
import model.Reg.AL
import model.Reg.AX
import model.Reg.BH
import model.Reg.BL
import model.Reg.BP
import model.Reg.BX
import model.Reg.CH
import model.Reg.CL
import model.Reg.CX
import model.Reg.DH
import model.Reg.DI
import model.Reg.DL
import model.Reg.DX
import model.Reg.EAX
import model.Reg.EBP
import model.Reg.EBX
import model.Reg.ECX
import model.Reg.EDI
import model.Reg.EDX
import model.Reg.ESI
import model.Reg.ESP
import model.Reg.SI
import model.Reg.SP
import kotlin.reflect.KClass

// These are standard x86 register encodings for ModR/M operations.
fun encodeRegister(reg: Reg): Byte {
    return when (reg) {
        EAX, AX, AL -> 0x00
        ECX, CX, CL -> 0x01
        EDX, DX, DL -> 0x02
        EBX, BX, BL -> 0x03
        ESP, SP, AH -> 0x04 // AH is not usually here, but x86 reuses encoding with REX
        EBP, BP, CH -> 0x05
        ESI, SI, DH -> 0x06
        EDI, DI, BH -> 0x07
    }
}
fun decodeRegister(value: Byte): Reg {
    return when (value.toInt() and 0x07) { // Mask to 3 bits
        0x00 -> EAX
        0x01 -> ECX
        0x02 -> EDX
        0x03 -> EBX
        0x04 -> ESP // Or AH if used with REX prefix, not handled here
        0x05 -> EBP // Or CH
        0x06 -> ESI // Or DH
        0x07 -> EDI // Or BH
        else -> throw IllegalArgumentException("Invalid register encoding: $value")
    }
}

// Helper to write a 32-bit UInt to a ByteArray (Little Endian)
fun uintToLittleEndianByteArray(value: UInt): Array<Byte> {
    val bytes = ByteArray(4)
    bytes[0] = (value and 0xFFu).toByte()
    bytes[1] = ((value shr 8) and 0xFFu).toByte()
    bytes[2] = ((value shr 16) and 0xFFu).toByte()
    bytes[3] = ((value shr 24) and 0xFFu).toByte()
    return bytes.toTypedArray()
}

// Helper to read a 32-bit UInt from a ByteArray (Little Endian)
fun littleEndianByteArrayToUInt(bytes: ByteArray, offset: Int = 0): UInt {
    return (bytes[offset].toUInt() and 0xFFu) or
            ((bytes[offset + 1].toUInt() and 0xFFu) shl 8) or
            ((bytes[offset + 2].toUInt() and 0xFFu) shl 16) or
            ((bytes[offset + 3].toUInt() and 0xFFu) shl 24)
}

// Helper to create a ModR/M byte
// MOD: 2 bits (00: [reg+disp], 01: [reg+disp8], 10: [reg+disp32], 11: reg)
// REG: 3 bits (register code or opcode extension)
// R/M: 3 bits (register code or memory addressing mode)
private fun createModRM(mod: Int, regOpcode: Byte, rm: Byte): Byte {
    return (((mod and 0x03) shl 6) or ((regOpcode.toInt() and 0x07) shl 3) or (rm.toInt() and 0x07)).toByte()
}

// Helper for relative jumps (very simplified)
// This needs to be called *after* the main opcode byte(s) for the jump are added to byteList
// currentInstructionBaseAddress is the address of the START of the current instruction being encoded.
// For more accuracy, this should be done by an assembler during a linking phase or knowing the final load address.
private fun operandToRelativeOffset8(operand: Operand): Byte {
    if (operand !is ImmOp) throw IllegalArgumentException("Relative jump target must be ImmOp for this simplified encoder.")
    // This is a placeholder. True relative offset calculation is:
    // val targetAbsoluteAddress = operand.value
    // val nextInstructionAddress = currentEncodedBaseAddressInMemory + instructionBaseLength.toUInt()
    // val offset = targetAbsoluteAddress.toLong() - nextInstructionAddress.toLong()
    // if (offset < -128 || offset > 127) throw IllegalArgumentException("Short jump offset out of range: $offset")
    // return offset.toByte()

    // For now, if operand.value is small and POSITIVE, assume it's a forward offset
    // This is NOT how it should be done in a real assembler.
    if (operand.value <= 127u && operand.value >= 0u) { // Extremely naive
        return operand.value.toByte()
    }
    // If it's a larger number, we can't represent it as a byte offset here without knowing the current EIP.
    // The decoder will need to be aware of how this is handled.
    // For now, let's just truncate, which will be wrong.
    println("Warning: Relative offset calculation for JMP/CALL is highly simplified in encoder.")
    return (operand.value and 0xFFu).toByte()
}

// --- Create a lookup map for opcodes ---
// This should be initialized once, perhaps in the companion object or an init block.
// It maps the first byte of an instruction to a list of possible operations,
// as some opcodes are prefixes or part of a multi-byte sequence.
// For now, a simpler map for direct opcodes.
private val opcodeToOperationMap: Map<Byte, Operation> by lazy {
    // Recursively find all Operation objects
    fun findOps(kClass: KClass<out Operation>): List<Operation> {
        return if (kClass.isSealed) {
            kClass.sealedSubclasses.flatMap { findOps(it) }
        } else if (kClass.objectInstance != null) {
            listOf(kClass.objectInstance!!)
        } else {
            emptyList()
        }
    }
    // Filter out operations that might not be uniquely identified by their 'opcode' field alone
    // or require more complex decoding (e.g. ModR/M based like PUSH r/m)
    // This map is best for *primary* opcodes of simple instructions.
    findOps(Operation::class)
        .filter {
            // Only include those that are uniquely identified by their main opcode byte
            // for simple lookup. Complex ones will be handled by ranges or specific checks.
            when (it) {
                is Operation.OperationZero -> true // NOP, RET, PUSHA, POPA have unique opcodes
                // For others, their 'opcode' field in the Operation object might be a base
                // and not the full story.
                // Example: Operation.OperationOne.JNZ has opcode 0x75, this is fine.
                Operation.OperationOne.JNZ, Operation.OperationOne.JZ,
                Operation.OperationOne.JG, Operation.OperationOne.LOOP -> true
                // MOV, ADD, PUSH etc. are too complex for this simple map entry based on their
                // single 'opcode' field in the sealed class, as they have many forms.
                else -> false
            }
        }
        .associateBy { it.opcode }
}

