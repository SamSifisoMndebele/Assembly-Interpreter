package ast

import isa.CpuRegister

/**
 * Represents a register operand.
 *
 * @property cpuRegister The register being used as an operand.
 */
data class Register(val cpuRegister: CpuRegister) : Operand {
    override fun toString(): String = cpuRegister.name
}