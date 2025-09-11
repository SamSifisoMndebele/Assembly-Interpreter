package parsing

import cpu.Memory
import lexical.Lexer
import model.Section

class Parser(private val lexer: Lexer, private val memory: Memory) {
    private var currentSection = Section.CODE
    private val symbolTable = mutableMapOf<String, UInt>() // symbol -> offset within data segment
    private var dataOffset = 0L // Current offset within the data segment
}