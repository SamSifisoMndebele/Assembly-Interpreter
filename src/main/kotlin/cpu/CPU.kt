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
            Reg.AL -> regs[Reg.EAX]!! and(0xFFL)
            Reg.AH -> (regs[Reg.EAX]!! shr (8) and(0xFFL))
            Reg.AX -> regs[Reg.EAX]!! and(0xFFFFL)
            Reg.BL -> (regs[Reg.EBX]!! and (0xFFL))
            Reg.BH -> (regs[Reg.EBX]!! shr (8) and(0xFFL))
            Reg.BX -> regs[Reg.EBX]!! and(0xFFFFL)
            Reg.CL -> (regs[Reg.ECX]!! and (0xFFL))
            Reg.CH -> (regs[Reg.ECX]!! shr (8) and(0xFFL))
            Reg.CX -> regs[Reg.ECX]!! and(0xFFFFL)
            Reg.DL -> (regs[Reg.EDX]!! and (0xFFL))
            Reg.DH -> (regs[Reg.EDX]!! shr (8) and(0xFFL))
            Reg.DX -> regs[Reg.EDX]!! and(0xFFFFL)
            Reg.SI -> regs[Reg.ESI]!! and(0xFFFFL)
            Reg.DI -> regs[Reg.EDI]!! and(0xFFFFL)
            Reg.BP -> regs[Reg.EBP]!! and(0xFFFFL)
            Reg.SP -> regs[Reg.ESP]!! and(0xFFFFL)
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
            Reg.AL -> regs[Reg.EAX] = (regs[Reg.EAX]!! and(0xFFFFFF00L)) or(value.and(0xFFL))
            Reg.AH -> regs[Reg.EAX] = (regs[Reg.EAX]!! and(0xFFFF00FFL)) or(value.and(0xFFL) shl(8))
            Reg.AX -> regs[Reg.EAX] = (regs[Reg.EAX]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.BL -> regs[Reg.EBX] = (regs[Reg.EBX]!! and(0xFFFFFF00L)) or(value.and(0xFFL))
            Reg.BH -> regs[Reg.EBX] = (regs[Reg.EBX]!! and(0xFFFF00FFL)) or(value.and(0xFFL) shl(8))
            Reg.BX -> regs[Reg.EBX] = (regs[Reg.EBX]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.CL -> regs[Reg.ECX] = (regs[Reg.ECX]!! and(0xFFFFFF00L)) or(value.and(0xFFL))
            Reg.CH -> regs[Reg.ECX] = (regs[Reg.ECX]!! and(0xFFFF00FFL)) or(value.and(0xFFL) shl(8))
            Reg.CX -> regs[Reg.ECX] = (regs[Reg.ECX]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.DL -> regs[Reg.EDX] = (regs[Reg.EDX]!! and(0xFFFFFF00L)) or(value.and(0xFFL))
            Reg.DH -> regs[Reg.EDX] = (regs[Reg.EDX]!! and(0xFFFF00FFL)) or(value.and(0xFFL) shl(8))
            Reg.DX -> regs[Reg.EDX] = (regs[Reg.EDX]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.SI -> regs[Reg.ESI] = (regs[Reg.ESI]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.DI -> regs[Reg.EDI] = (regs[Reg.EDI]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.BP -> regs[Reg.EBP] = (regs[Reg.EBP]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            Reg.SP -> regs[Reg.ESP] = (regs[Reg.ESP]!! and(0xFFFF0000L)) or(value.and(0xFFFFL))
            else -> if (regs.containsKey(reg)) regs[reg] = value else throw NoSuchElementException("Register $reg not found")
        }
    }

    /**
     * Generates a string representation of the current state of the CPU registers and flags.
     *
     * This function formats the values of all major general-purpose registers (EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP),
     * their 16-bit counterparts (AX, BX, CX, DX, SI, DI, BP, SP), and for the first four, their 8-bit
     * high (AH, BH, CH, DH) and low (AL, BL, CL, DL) components. It also includes the current value
     * of the Instruction Pointer (IP) and the status of the CPU flags (CF, PF, AF, ZF, SF, OF).
     *
     * Register values are displayed in hexadecimal format.
     * Flag values are displayed as 0 (false) or 1 (true).
     *
     * @return A [String] containing a formatted multi-line representation of the CPU's register and flag states.
     *         Each line details a specific register or the set of flags.
     *         Example format for a register:
     *         `  EAX: 0x00000000 (AX: 0x0000, AH: 0x00, AL: 0x00)`
     *         Example format for flags:
     *         `Flags: CF=0 PF=0 AF=0 ZF=0 SF=0 OF=0`
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
        sb.appendLine("Flags: CF=${flags.CF.toInt()} PF=${flags.PF.toInt()} AF=${flags.AF.toInt()} ZF=${flags.ZF.toInt()} SF=${flags.SF.toInt()} OF=${flags.OF.toInt()}")
        return sb.toString()
    }

    private fun Boolean.toInt() = if (this) 1 else 0
}

