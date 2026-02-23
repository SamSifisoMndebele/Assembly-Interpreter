package model.operation

import model.Bits
import old.model.operation.OperationOne
import old.model.operation.OperationTwo
import old.model.operation.OperationZero

/**
 * Represents a generic CPU model.operation.
 * This is a sealed interface, meaning all possible implementations are defined within this file.
 * Operations are further categorized by the number of operands they take:
 *  - [OperationZero]: Operations with no explicit operands.
 *  - [OperationOne]: Operations with one explicit operand.
 *  - [OperationTwo]: Operations with two explicit operands.
 *
 * Each model.operation has an associated [opcode].
 */
sealed interface Operation {
    val opcode: UByte
    val bits: Bits

    companion object {
        val allOperations: Set<Operation>
            get() {
                val opClasses = listOf(
                    OperationZero::class,
                    OperationOne::class,
                    OperationTwo::class
                )
                return opClasses.flatMap { opClass ->
                    opClass.nestedClasses.map { it.objectInstance as Operation }
                }.toSet()
            }

    }
}