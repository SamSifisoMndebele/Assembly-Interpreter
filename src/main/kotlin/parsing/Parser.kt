package parsing

import cpu.Memory
import lexical.Lexer
import model.Segment
import java.io.File
import java.io.FileNotFoundException
import kotlin.system.exitProcess

class Parser(source: String, private val memory: Memory) : Lexer(source) {
    private var segment = Segment.CODE


    init {
        println("Tokens:")
        getTokens().forEach { println(it) }
    }
}


fun main() {
    val src = try {
        File("src/main/kotlin/main.asm").readText()
    } catch (e: FileNotFoundException) {
        println("Error: ${e.message}, Source file not found: src/main/kotlin/main.asm")
        println("Please provide a valid path as a command-line argument or make sure the default file exists.")
        exitProcess(1)
    }

    val memory = Memory(64L)
    Parser(src, memory)

    memory.printMemory()
}