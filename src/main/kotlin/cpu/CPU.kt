package cpu

import model.Flags
import model.Reg

/**
 * Represents the CPU (Central Processing Unit) of a system.
 *
 * This data class holds the state of the CPU, including its registers,
 * instruction pointer, and flags. It provides methods to read from and write to
 * these registers, handling both full registers and their sub-components (e.g., AH/AL for AX).
 *
 * @property regs A mutable map storing the current values of the CPU registers.
 *                 The keys are [Reg] enum values representing different registers,
 *                 and the values are [Long] representing the register's content.
 *                 Registers are initialized to 0. This map includes entries for
 *                 8-bit, 16-bit, and 32-bit registers.
 *                 Note: 8-bit registers like AL, AH are not directly stored here
 *                 but are accessed/modified via their parent 16-bit register (e.g., AX).
 * @property IP The Instruction Pointer, a [Long] value indicating the memory address
 *              (or index in an instruction list) of the next instruction to be executed.
 *              Initialized to 0.
 * @property flags An instance of the [Flags] data class, representing the current
 *                 state of the CPU's status flags (e.g., zero flag, carry flag).
 *                 Initialized with default flag values.
 *                 The [Flags] data class contains the following boolean properties:
 *                 - `CF` (Carry Flag): Set if an arithmetic operation generates a carry or a borrow out of the most significant bit.
 *                 - `PF` (Parity Flag): Set if the least significant byte of the result has an even number of 1 bits.
 *                 - `AF` (Auxiliary Carry Flag): Set if an arithmetic operation generates a carry or a borrow out of bit 3. Used for BCD arithmetic.
 *                 - `ZF` (Zero Flag): Set if the result of an arithmetic operation is zero.
 *                 - `SF` (Sign Flag): Set if the most significant bit of the result is 1 (indicating a negative number for signed integers).
 *                 - `TF` (Trap Flag): If set, the processor generates a single-step interrupt after each instruction.
 */
data class CPU(
    val regs: MutableMap<Reg, Long> = mutableMapOf(
        // 16-bit registers
        Reg.AX to 0, Reg.BX to 0, Reg.CX to 0, Reg.DX to 0,
        Reg.SI to 0, Reg.DI to 0, Reg.BP to 0, Reg.SP to 0,
        // 32-bit registers
        Reg.EAX to 0, Reg.EBX to 0, Reg.ECX to 0, Reg.EDX to 0,
        Reg.ESI to 0, Reg.EDI to 0, Reg.EBP to 0, Reg.ESP to 0
    ),
    @Suppress("PropertyName")
    var IP: Long = 0, // instruction pointer (index into an instruction list)
    val flags: Flags = Flags()
) {
    /**
     * Reads the value of a specified register.
     *
     * @param reg The [Reg] enum representing the register to read.
     * @return The [Long] value currently stored in the specified register.
     * @throws NoSuchElementException if the specified register does not exist.
     */
    fun readReg(reg: Reg): Long {
        return when (reg) {
            Reg.AL -> regs[Reg.AX]!! and(0xFFL)
            Reg.AH -> (regs[Reg.AX]!! shr (8) and(0xFFL))
            Reg.BL -> (regs[Reg.BX]!! and (0xFFL))
            Reg.BH -> (regs[Reg.BX]!! shr (8) and(0xFFL))
            Reg.CL -> (regs[Reg.CX]!! and (0xFFL))
            Reg.CH -> (regs[Reg.CX]!! shr (8) and(0xFFL))
            Reg.DL -> (regs[Reg.DX]!! and (0xFFL))
            Reg.DH -> (regs[Reg.DX]!! shr (8) and(0xFFL))
            else -> regs[reg] ?: throw NoSuchElementException("Register $reg not found")
        }
    }

    /**
     * Writes a value to a specified register.
     *
     * @param reg The [Reg] enum representing the register to write to.
     * @param value The [Long] value to store in the specified register.
     * @throws NoSuchElementException if the specified register does not exist.
     */
    fun writeReg(reg: Reg, value: Long) {
        when (reg) {
            Reg.AL -> regs[Reg.AX] = (regs[Reg.AX]!! and(0xFF00L)) or(value.and(0xFFL))
            Reg.AH -> regs[Reg.AX] = (regs[Reg.AX]!! and(0x00FFL)) or(value.and(0xFFL) shl(8)) 
            Reg.BL -> regs[Reg.BX] = (regs[Reg.BX]!! and(0xFF00L)) or(value.and(0xFFL))
            Reg.BH -> regs[Reg.BX] = (regs[Reg.BX]!! and(0x00FFL)) or(value.and(0xFFL) shl(8))
            Reg.CL -> regs[Reg.CX] = (regs[Reg.CX]!! and(0xFF00L)) or(value.and(0xFFL))
            Reg.CH -> regs[Reg.CX] = (regs[Reg.CX]!! and(0x00FFL)) or(value.and(0xFFL) shl(8))
            Reg.DL -> regs[Reg.DX] = (regs[Reg.DX]!! and(0xFF00L)) or(value.and(0xFFL))
            Reg.DH -> regs[Reg.DX] = (regs[Reg.DX]!! and(0x00FFL)) or(value.and(0xFFL) shl(8))
            else -> if (regs.containsKey(reg)) regs[reg] = value else throw NoSuchElementException("Register $reg not found")
        }
    }
}

