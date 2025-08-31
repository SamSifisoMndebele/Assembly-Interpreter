package cpu

import model.Flags
import model.Reg

/**
 * Represents the CPU (Central Processing Unit) of a system.
 *
 * This data class holds the state of the CPU, including its registers,
 * instruction pointer, and flags.
 *
 * @property regs A mutable map storing the current values of the CPU registers.
 *                 The keys are [Reg] enum values representing different registers,
 *                 and the values are [Long] representing the register's content.
 *                 Registers are initialized to 0.
 * @property IP The Instruction Pointer, a [Long] value indicating the memory address
 *              (or index in an instruction list) of the next instruction to be executed.
 *              Initialized to 0.
 * @property flags An instance of the [Flags] data class, representing the current
 *                 state of the CPU's status flags (e.g., zero flag, carry flag).
 *                 Initialized with default flag values.
 */
data class CPU(
    val regs: MutableMap<Reg, Long> = mutableMapOf(
        // 8-bit registers
        Reg.AL to 0, Reg.AH to 0, Reg.BL to 0, Reg.BH to 0,
        Reg.CL to 0, Reg.CH to 0, Reg.DL to 0, Reg.DH to 0,
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
)