package parsing

import cpu.Memory
import lexical.Lexer
import lexical.Token
import model.Segment
import instruction.Instruction
import instruction.InstructionZero
import instruction.InstructionOne
import instruction.InstructionTwo
import model.CpuRegister
import model.Operation
import model.Operand
import model.Operand.*
import java.io.File
import java.io.FileNotFoundException
import kotlin.system.exitProcess

class Parser(source: String, private val memory: Memory) : Lexer(source) {
    private var currentSegment = Segment.CODE
    val instructions = mutableListOf<Instruction>()

    init {
        println("Tokens:")
        getTokens().forEach { println(it) } // Keep this for debugging if you like
        parseInstructions()
        // Later, we'll add encoding and memory writing steps here
    }

    private fun parseInstructions() {
        while (hasToken()) {
            val token = nextToken()
            when (token.kind) {
                Token.Kind.SEGMENT -> {
                    // Handle segment directives like .code, .data
                    currentSegment = Segment.valueOf(token.text.removePrefix(".").uppercase())
                    println("Switched to segment: $currentSegment")
                }
                Token.Kind.OPERATION -> {
                    // The token.text is the mnemonic (e.g., "MOV", "ADD", "NOP")
                    parseOperation(token)
                }
                Token.Kind.LABEL -> {
                    // Handle label definitions (e.g., "myLabel:")
                    // For now, we'll just print it. Label handling will be more complex.
                    println("Found label: ${token.text}")
                }
                // Potentially handle other token kinds if necessary, or ignore
                else -> {
                    //println("Ignoring token: $token")
                }
            }
        }
        println("Parsed Instructions:")
        instructions.forEach { println(it) }
    }

    private fun parseOperation(token: Token) {
        require(token.kind == Token.Kind.OPERATION)
        val operationName = token.text.uppercase()
        val line = token.line

        val operationZero = Operation.OperationZero::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as Operation.OperationZero?
        if (operationZero != null) {
            parseInstructionZero(operationZero, line).let { instructions.add(it) }
            return
        }

        val operationOne = Operation.OperationOne::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as Operation.OperationOne?
        if (operationOne != null) {
            parseInstructionOne(operationOne, line)?.let { instructions.add(it) }
            return
        }

        val operationTwo = Operation.OperationTwo::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as Operation.OperationTwo?
        if (operationTwo != null) {
            parseInstructionTwo(operationTwo, line)?.let { instructions.add(it) }
            return
        }


        error("Unknown operation '${token.text}' at line $line")
    }

    private fun parseInstructionZero(op: Operation.OperationZero, line: Int): InstructionZero {
        // InstructionZero has no operands to parse from tokens
        return InstructionZero(op, line)
    }

    private fun parseInstructionOne(op: Operation.OperationOne, line: Int): InstructionOne? {
        println("Parsing one-operand instruction: $op at line $line.")
        if (hasToken()) {
            val operand = nextOperand()
            return InstructionOne(op, operand, line)
        }
        error("Expected operand for $op at line $line, but found no more tokens.")
    }

    private fun parseInstructionTwo(op: Operation.OperationTwo, line: Int): InstructionTwo? {
        println("Parsing two-operand instruction: $op at line $line. Operand parsing NOT IMPLEMENTED.")

        if (!hasToken()) error("Missing or invalid destination operand for $op at line $line")
        val destOperand = nextOperand()

        if (!hasToken() || nextToken().kind != Token.Kind.COMMA) {
            // Backtrack if we consumed a token that wasn't a comma
            if (hasPrevious()) previousToken()
            error("Expected comma after destination operand for $op at line $line")
        }

        if (!hasToken()) error("Missing or invalid source operand for $op at line $line")
        val srcOperand = nextOperand()

        return InstructionTwo(op, destOperand, srcOperand, line)
    }

    private fun nextOperand(): Operand {
        val token = nextToken()
        return when (token.kind) {
            Token.Kind.REGISTER -> Register(CpuRegister.valueOf(token.text.uppercase()))
            Token.Kind.NUMBER_HEX -> Immediate(token.text.removePrefix("0x").removeSuffix("h").toUInt(16))
            Token.Kind.NUMBER_DEC -> Immediate(token.text.toUInt())
            Token.Kind.IDENTIFIER -> Identifier(token.text)
            Token.Kind.NUMBER_BIN -> Immediate(token.text.removePrefix("0b").removeSuffix("b").toUInt(2))
            Token.Kind.NUMBER_OCT -> Immediate(token.text.removePrefix("0o").removeSuffix("o").toUInt(8))
            Token.Kind.STRING -> TODO("String operand parsing not yet implemented")
            Token.Kind.LABEL -> Label(token.text)
            Token.Kind.LBRACKET -> {
                // Basic memory operand parsing: [imm32] or [reg] or [reg+imm32]
                // This is a simplified version and needs to be much more robust
                if (!hasToken()) error("Incomplete memory operand at line ${token.line}")
                var next = nextToken()
                var base: Operand? = null
                var displacement: UInt? = null

                if (next.kind != Token.Kind.REGISTER && next.kind != Token.Kind.IDENTIFIER && !next.isNumber)
                    error("Expected register, identifier, or immediate value for memory operand at line ${token.line}")

                if (next.kind == Token.Kind.REGISTER) {
                    val baseReg = CpuRegister.valueOf(next.text.uppercase())
                    base = Register(baseReg)
                    if (!hasToken()) error("Expected ']' or '+' after register in memory operand at line ${token.line}")
                    next = nextToken() // Consume for '+' or ']'
                } else if (next.kind == Token.Kind.IDENTIFIER) {
                    base = Identifier(next.text)
                    if (!hasToken()) error("Expected ']' or '+' after identifier in memory operand at line ${token.line}")
                    next = nextToken() // Consume for '+' or ']'
                }

                if (next.kind == Token.Kind.PLUS) {
                    if (!hasToken()) error("Expected displacement after '+' in memory operand at line ${token.line}")
                    next = nextToken() // Should be a number
                    if (next.isNumber) {
                        displacement = when (next.kind) {
                            Token.Kind.NUMBER_HEX -> next.text.removePrefix("0x").removeSuffix("h").toUInt(16)
                            Token.Kind.NUMBER_BIN -> next.text.removePrefix("0b").removeSuffix("b").toUInt(2)
                            Token.Kind.NUMBER_OCT -> next.text.removePrefix("0o").removeSuffix("o").toUInt(8)
                            else -> next.text.toUInt()
                        }
                        if (!hasToken()) error("Expected ']' after displacement in memory operand at line ${token.line}")
                        next = nextToken() // Consume for ']'
                    } else {
                        error("Expected number for displacement in memory operand at line ${token.line}")
                    }
                } else if (next.isNumber) {
                    // Case: [imm32]
                    displacement = when (next.kind) {
                        Token.Kind.NUMBER_HEX -> next.text.removePrefix("0x").removeSuffix("h").toUInt(16)
                        Token.Kind.NUMBER_BIN -> next.text.removePrefix("0b").removeSuffix("b").toUInt(2)
                        Token.Kind.NUMBER_OCT -> next.text.removePrefix("0o").removeSuffix("o").toUInt(8)
                        else -> next.text.toUInt()
                    }
                    if (!hasToken()) error("Expected ']' after displacement in memory operand at line ${token.line}")
                    next = nextToken() // Check for ']'
                }

                if (next.kind != Token.Kind.RBRACKET) error("Expected ']' to close memory operand at line ${next.line}")
                return Memory(base, displacement)
            }
            else -> error("Unknown operand type at line ${token.line}")
        }
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

    val memory = Memory(1024L)
    val parser = Parser(src, memory)

    println("Instructions:")
    parser.instructions.forEach { println(it) }

//    memory.printMemory()
}
