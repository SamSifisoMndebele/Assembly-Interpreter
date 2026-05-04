package isa

import kotlin.reflect.jvm.jvmName

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
sealed interface Mnemonic: Comparable<Mnemonic> {
    val encoding: Encoding

    companion object {
        val allMnemonics: Set<Mnemonic> by lazy {
            listOf(
                MnemonicZero::class,
                MnemonicOne::class,
                MnemonicTwo::class
            ).flatMap { parent ->
                parent.sealedSubclasses.mapNotNull { it.objectInstance }
            }.toSortedSet()
        }
    }

    override fun compareTo(other: Mnemonic): Int = toString().compareTo(other.toString())
}

fun main() {
    println(Mnemonic.allMnemonics)
}