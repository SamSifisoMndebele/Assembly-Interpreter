package model.mnemonic

import model.Bits

/**
 * Represents a generic CPU model.operation.
 * This is a sealed interface, meaning all possible implementations are defined within this file.
 * Operations are further categorized by the number of operands they take:
 *  - [MnemonicZero]: Operations with no explicit operands.
 *  - [MnemonicOne]: Operations with one explicit operand.
 *  - [MnemonicTwo]: Operations with two explicit operands.
 *
 * Each model.operation has an associated [opcode].
 */
sealed interface Mnemonic {
    val opcode: UByte
    val bits: Bits

    companion object {
        val allMnemonics: Set<Mnemonic?>
            get() {
                val opClasses = listOf(
                    MnemonicZero::class,
                    MnemonicOne::class,
                    MnemonicTwo::class
                )
                return opClasses.flatMap { opClass ->
                    opClass.nestedClasses.map { it.objectInstance as Mnemonic? }
                }.toSet()
            }

    }
}