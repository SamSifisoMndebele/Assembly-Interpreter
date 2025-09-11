package parsing

import cpu.Memory
import lexical.Lexer
import model.Section
import java.io.File
import java.io.FileNotFoundException
import kotlin.system.exitProcess

class Parser(private val lexer: Lexer, private val memory: Memory) {
    private var currentSection = Section.CODE
    private val symbolTable = mutableMapOf<String, UInt>() // symbol -> offset within data segment
    private var dataOffset = 0L // Current offset within the data segment

    init {
        println("Tokens:")
        lexer.getTokens().forEach {
            println(it)
        }
    }
}


fun main() {
    val src = try {
        File("src/main/kotlin/main.asm").readText()
    } catch (e: FileNotFoundException) {
        println("Error: ${e.message}, Source file not found.")
        println("Please provide a valid path as a command-line argument or make sure the default file exists.")
        exitProcess(1)
    }

    val lexer = Lexer(src)
    val memory = Memory()
    val parser = Parser(lexer, memory)


}