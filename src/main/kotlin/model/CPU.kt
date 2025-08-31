package model

data class CPU(
    val regs: MutableMap<Reg, Int> = mutableMapOf(
        Reg.AX to 0, Reg.BX to 0, Reg.CX to 0, Reg.DX to 0,
        Reg.SI to 0, Reg.DI to 0, Reg.BP to 0, Reg.SP to 0xFFFE // stack grows down
    ),
    var IP: Int = 0, // instruction pointer (index into instruction list)
    val flags: Flags = Flags()
)