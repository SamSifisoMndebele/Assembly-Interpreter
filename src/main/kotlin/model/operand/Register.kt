package model.operand

import model.CpuRegister

/**
 * Represents a register operand.
 *
 * @property cpuRegister The register being used as an operand.
 */
data class Register(val cpuRegister: CpuRegister) : Operand {
    override fun toString(): String = cpuRegister.name
}