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
 *                 8-bit, 16-bit, 32-bit, and segment registers.
 *                 Note: 8-bit registers like AL, AH are not directly stored here
 *                 but are accessed/modified via their parent 16-bit register (e.g., AX).
 * @property IP The Instruction Pointer, a [Long] value indicating the memory address
 *              (or index in an instruction list) of the next instruction to be executed.
 *              Initialized to 0.
 * @property flags An instance of the [Flags] data class, representing the current
 *                 state of the CPU's status flags (e.g., zero flag, carry flag).
 *                 Initialized with default flag values.
 */
data class CPU1(
    val regs: MutableMap<Reg, Long> = mutableMapOf(
        Reg.EAX to 0, Reg.EBX to 0, Reg.ECX to 0, Reg.EDX to 0,
        Reg.ESI to 0, Reg.EDI to 0, Reg.EBP to 0, Reg.ESP to 0,
        Reg.CS to 0L, Reg.DS to 0x1000L, Reg.SS to 0L, Reg.ES to 0L // Initialize segment registers
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
            Reg.AL -> regs[Reg.EAX]!! and 0xFFL
            Reg.AH -> (regs[Reg.EAX]!! shr 8) and 0xFFL
            Reg.AX -> regs[Reg.EAX]!! and 0xFFFFL
            Reg.BL -> regs[Reg.EBX]!! and 0xFFL
            Reg.BH -> (regs[Reg.EBX]!! shr 8) and 0xFFL
            Reg.BX -> regs[Reg.EBX]!! and 0xFFFFL
            Reg.CL -> regs[Reg.ECX]!! and 0xFFL
            Reg.CH -> (regs[Reg.ECX]!! shr 8) and 0xFFL
            Reg.CX -> regs[Reg.ECX]!! and 0xFFFFL
            Reg.DL -> regs[Reg.EDX]!! and 0xFFL
            Reg.DH -> (regs[Reg.EDX]!! shr 8) and 0xFFL
            Reg.DX -> regs[Reg.EDX]!! and 0xFFFFL
            Reg.SI -> regs[Reg.ESI]!! and 0xFFFFL
            Reg.DI -> regs[Reg.EDI]!! and 0xFFFFL
            Reg.BP -> regs[Reg.EBP]!! and 0xFFFFL
            Reg.SP -> regs[Reg.ESP]!! and 0xFFFFL
            // Segment Registers (16-bit)
            Reg.CS -> regs[Reg.CS]!! and 0xFFFFL
            Reg.DS -> regs[Reg.DS]!! and 0xFFFFL
            Reg.SS -> regs[Reg.SS]!! and 0xFFFFL
            Reg.ES -> regs[Reg.ES]!! and 0xFFFFL
            // 32-bit General Purpose Registers
            Reg.EAX, Reg.EBX, Reg.ECX, Reg.EDX, Reg.ESI, Reg.EDI, Reg.EBP, Reg.ESP -> regs[reg]!!
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
            Reg.AL -> regs[Reg.EAX] = (regs[Reg.EAX]!! and 0xFFFFFF00L) or (value and 0xFFL)
            Reg.AH -> regs[Reg.EAX] = (regs[Reg.EAX]!! and 0xFFFF00FFL) or ((value and 0xFFL) shl 8)
            Reg.AX -> regs[Reg.EAX] = (regs[Reg.EAX]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.BL -> regs[Reg.EBX] = (regs[Reg.EBX]!! and 0xFFFFFF00L) or (value and 0xFFL)
            Reg.BH -> regs[Reg.EBX] = (regs[Reg.EBX]!! and 0xFFFF00FFL) or ((value and 0xFFL) shl 8)
            Reg.BX -> regs[Reg.EBX] = (regs[Reg.EBX]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.CL -> regs[Reg.ECX] = (regs[Reg.ECX]!! and 0xFFFFFF00L) or (value and 0xFFL)
            Reg.CH -> regs[Reg.ECX] = (regs[Reg.ECX]!! and 0xFFFF00FFL) or ((value and 0xFFL) shl 8)
            Reg.CX -> regs[Reg.ECX] = (regs[Reg.ECX]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.DL -> regs[Reg.EDX] = (regs[Reg.EDX]!! and 0xFFFFFF00L) or (value and 0xFFL)
            Reg.DH -> regs[Reg.EDX] = (regs[Reg.EDX]!! and 0xFFFF00FFL) or ((value and 0xFFL) shl 8)
            Reg.DX -> regs[Reg.EDX] = (regs[Reg.EDX]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.SI -> regs[Reg.ESI] = (regs[Reg.ESI]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.DI -> regs[Reg.EDI] = (regs[Reg.EDI]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.BP -> regs[Reg.EBP] = (regs[Reg.EBP]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            Reg.SP -> regs[Reg.ESP] = (regs[Reg.ESP]!! and 0xFFFF0000L) or (value and 0xFFFFL)
            // Segment Registers (16-bit)
            Reg.CS -> regs[Reg.CS] = value and 0xFFFFL
            Reg.DS -> regs[Reg.DS] = value and 0xFFFFL
            Reg.SS -> regs[Reg.SS] = value and 0xFFFFL
            Reg.ES -> regs[Reg.ES] = value and 0xFFFFL
            // 32-bit General Purpose Registers
            Reg.EAX, Reg.EBX, Reg.ECX, Reg.EDX, Reg.ESI, Reg.EDI, Reg.EBP, Reg.ESP -> {
                if (regs.containsKey(reg)) regs[reg] = value
                else throw NoSuchElementException("Register $reg not found (should be in map)")
            }
        }
    }

    /**
     * Generates a string representation of the current state of the CPU registers and flags.
     */
    fun printRegisters(): String {
        val sb = StringBuilder()
        sb.appendLine("Registers:")
        sb.appendLine("  EAX: 0x%08X (AX: 0x%04X, AH: 0x%02X, AL: 0x%02X)".format(readReg(Reg.EAX), readReg(Reg.AX), readReg(Reg.AH), readReg(Reg.AL)))
        sb.appendLine("  EBX: 0x%08X (BX: 0x%04X, BH: 0x%02X, BL: 0x%02X)".format(readReg(Reg.EBX), readReg(Reg.BX), readReg(Reg.BH), readReg(Reg.BL)))
        sb.appendLine("  ECX: 0x%08X (CX: 0x%04X, CH: 0x%02X, CL: 0x%02X)".format(readReg(Reg.ECX), readReg(Reg.CX), readReg(Reg.CH), readReg(Reg.CL)))
        sb.appendLine("  EDX: 0x%08X (DX: 0x%04X, DH: 0x%02X, DL: 0x%02X)".format(readReg(Reg.EDX), readReg(Reg.DX), readReg(Reg.DH), readReg(Reg.DL)))
        sb.appendLine("  ESI: 0x%08X (SI: 0x%04X)".format(readReg(Reg.ESI), readReg(Reg.SI)))
        sb.appendLine("  EDI: 0x%08X (DI: 0x%04X)".format(readReg(Reg.EDI), readReg(Reg.DI)))
        sb.appendLine("  EBP: 0x%08X (BP: 0x%04X)".format(readReg(Reg.EBP), readReg(Reg.BP)))
        sb.appendLine("  ESP: 0x%08X (SP: 0x%04X)".format(readReg(Reg.ESP), readReg(Reg.SP)))
        sb.appendLine("  IP:  0x%08X".format(IP))
        sb.appendLine("Segment Registers:")
        sb.appendLine("  CS: 0x%04X   DS: 0x%04X   SS: 0x%04X   ES: 0x%04X".format(readReg(Reg.CS), readReg(Reg.DS), readReg(Reg.SS), readReg(Reg.ES)))
        sb.appendLine("Flags: CF=${flags.CF.toInt()} PF=${flags.PF.toInt()} AF=${flags.AF.toInt()} ZF=${flags.ZF.toInt()} SF=${flags.SF.toInt()} OF=${flags.OF.toInt()}")
        return sb.toString()
    }

    private fun Boolean.toInt() = if (this) 1 else 0
}



