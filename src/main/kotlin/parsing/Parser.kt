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
import model.SymbolEntry
import java.io.File
import java.io.FileNotFoundException
import java.nio.ByteBuffer
import java.nio.ByteOrder
import kotlin.math.max
import kotlin.math.min
import kotlin.system.exitProcess

@OptIn(ExperimentalUnsignedTypes::class)
class Parser(source: String, private val memory: Memory) : Lexer(source) {
    private val instructions = mutableListOf<Instruction>()
    private val symbols = mutableListOf<SymbolEntry>()
    private var currentSegment = Segment.CODE // Tracks segment during parsing
    private var memoryCursor = 0L // For actual memory writing address
    private var dataSegmentOffset = 0L // Tracks offset within the data segment for symbol addresses

    // Segment base addresses and alignment
    val codeSegmentBase: Long = 0L
    var dataSegmentBase: Long = 0L
        private set
    val stackSegmentBase: Long = memory.bytes

    init {
        println("Tokens:")
        getTokens().forEach { println(it) } // Debug: Print all tokens first

        // 1. Parse source to populate `instructions` list and `symbols` list
        parseTokens()

        // 2. Calculate DATA segment base address
        dataSegmentBase = instructions.size * 16L
        if (dataSegmentBase >= memory.bytes) error("Calculated data segment base ($dataSegmentBase) is outside memory bounds (${memory.bytes}).")
        if (stackSegmentBase < dataSegmentBase) error("Stack segment base ($stackSegmentBase) is before data segment base ($dataSegmentBase).")

        // 3. Write DATA segment to memory

        // 4. Write CODE segment to memory
        /*memoryCursor = codeSegmentBase
        println("\nEncoding and writing CODE segment to memory, starting at address $codeSegmentBase...")
        instructions.forEach { instruction ->
            val encodedBytes = instruction.encode()
            val instructionStartAddress = memoryCursor
            encodedBytes.forEach { byte ->
                if (memoryCursor < memory.bytes) { // Check memory bounds
                    memory.writeByte(memoryCursor, byte)
                    memoryCursor++
                } else {
                    error("Memory overflow while writing code segment at address $memoryCursor. Max memory: ${memory.bytes}")
                }
            }
            println("Encoded ${instruction::class.simpleName} to ${encodedBytes.size} bytes at address $instructionStartAddress.")
        }
        val endOfCodeAddress = memoryCursor
        println("Finished writing CODE segment. Total bytes written to code segment: $endOfCodeAddress.")*/
    }


    private fun parseTokens() {
        currentSegment = Segment.CODE

        while (hasToken()) {
            val token = nextToken()
            when(currentSegment) {
                Segment.DATA -> when (token.kind) {
                    Token.Kind.SEGMENT -> {
                        currentSegment = Segment.valueOf(token.text.removePrefix(".").uppercase())
                        println("Switched to segment: $currentSegment. Resetting parsing address offset for this segment.")
                    }
                    Token.Kind.IDENTIFIER -> parseDataDefinition(token)
                    else -> error("Unexpected token kind: ${token.kind}")
                }
                Segment.CODE -> when (token.kind) {
                    Token.Kind.SEGMENT -> {
                        currentSegment = Segment.valueOf(token.text.removePrefix(".").uppercase())
                        println("Switched to segment: $currentSegment. Resetting parsing address offset for this segment.")
                    }
                    Token.Kind.OPERATION -> parseInstruction(token)
                    Token.Kind.LABEL -> {
                        // Handle label definitions (e.g., "myLabel:")
                        // For now, we'll just print it. Label handling will be more complex.
                        println("Found label: ${token.text}")
                    }
                    else -> error("Unexpected token kind: ${token.kind}")
                }
                Segment.STACK -> TODO("Stack segment not yet implemented")
            }
        }
        println("\nFinished parsing. Found ${instructions.size} instructions (assumed for CODE segment).")
    }

    private fun parseDataDefinition(token: Token) {
        require(token.kind == Token.Kind.IDENTIFIER)
        val name = token.text
        val line = token.line

        if (!hasToken() || peekToken().kind != Token.Kind.DATA_DIR) {
            error("Expected data directive (BYTE, WORD, DWORD, QWORD) after identifier '$name' at line $line, found ${if(hasToken()) peekToken().kind else "EOF"}")
        }
        println("Parsing data definition: $name at line $line, dataSegmentOffset: $dataSegmentOffset")

        val dataValue = nextDataValue()
        val symbol = SymbolEntry(name, dataSegmentOffset, dataValue.type, dataValue.bytes, line)
        symbols.add(symbol)
        dataSegmentOffset += symbol.length // Increment offset by the total size of this data entry
        println("Added data symbol: $symbol. New dataSegmentOffset: $dataSegmentOffset")
    }

    private fun nextDataValue(): DataValue {
        val token = nextToken() // Consume data directive
        require(token.kind == Token.Kind.DATA_DIR) { "Expected data directive, found ${token.kind}" }
        val values = mutableListOf<UByte>()
        val type = token.text.uppercase()
        val line = token.line
        var firstValue = true
        while (hasToken()) {
            val valueToken = peekToken()
            when (valueToken.kind) {
                Token.Kind.STRING -> {
                    nextToken() // Consume string token
                    if (type != "DB" && type != "BYTE") error("Strings can only be defined with BYTE directive at line ${valueToken.line}")
                    // Remove quotes and convert to UByteArray
                    valueToken.text.trim('\'', '"').forEach { char ->
                        values.add(char.code.toUByte())
                    }
                }
                Token.Kind.NUMBER_HEX, Token.Kind.NUMBER_BIN, Token.Kind.NUMBER_OCT, Token.Kind.NUMBER_DEC -> {
                    nextToken() // Consume number token
                    val bytes = valueToken.toUInt().toBytes(type, valueToken.line)
                    values.addAll(bytes)
                }
                Token.Kind.UNKNOWN -> { // Handle '?'
                    val bytes = 0u.toBytes(type, valueToken.line) // Use 0 as the placeholder value
                    values.addAll(bytes)
                }
                else -> {
                    // If not a value, it might be the end of this definition or a new line/segment
                    if (firstValue) error("Expected data value for $type at line $line, found ${valueToken.kind}")
                    return DataValue(type, values.toUByteArray()) // End of current data definition's values
                }
            }
            firstValue = false
            // Check for comma or end of line/definition
            if (hasToken() && peekToken().kind == Token.Kind.COMMA) {
                nextToken() // Consume comma
                if (!hasToken() || (!peekToken().isNumber && peekToken().kind != Token.Kind.STRING)) {
                    error("Expected data value after comma for $type at line $line")
                }
            } else break // No comma, so end of values for this definition
        }
        if (firstValue) error("Expected data value for $type at line $line but found none.")
        return DataValue(type, values.toUByteArray())
    }

    private fun parseInstruction(token: Token) {
        require(token.kind == Token.Kind.OPERATION)
        val operationName = token.text.uppercase()
        val line = token.line

        val operationZero = Operation.OperationZero::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as Operation.OperationZero?
        if (operationZero != null) {
            println("Parsing zero-operand instruction: $operationZero at line $line.")
            instructions.add(InstructionZero(operationZero, line))
            return
        }

        val operationOne = Operation.OperationOne::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as Operation.OperationOne?
        if (operationOne != null) {
            println("Parsing one-operand instruction: $operationOne at line $line.")
            if (!hasToken()) error("Expected operand for $operationOne at line $line, but found no more tokens.")
            val operand = nextOperand()
            instructions.add(InstructionOne(operationOne, operand, line))
            return
        }

        val operationTwo = Operation.OperationTwo::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as Operation.OperationTwo?
        if (operationTwo != null) {
            println("Parsing two-operand instruction: $operationTwo at line $line.")
            if (!hasToken()) error("Missing or invalid destination operand for $operationTwo at line $line")
            val destOperand = nextOperand()
            if (!hasToken() || nextToken().kind != Token.Kind.COMMA) {
                if (hasPrevious()) previousToken()
                error("Expected comma after destination operand for $operationTwo at line $line")
            }
            if (!hasToken()) error("Missing or invalid source operand for $operationTwo at line $line")
            val srcOperand = nextOperand()
            instructions.add(InstructionTwo(operationTwo, destOperand, srcOperand, line))
            return
        }

        error("Unknown operation '${token.text}' at line $line")
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
                if (!hasToken()) error("Incomplete memory operand at line ${token.line}")
                var next = nextToken()
                var base: Operand? = null
                var displacement: UInt? = null

                if (next.kind != Token.Kind.REGISTER && next.kind != Token.Kind.IDENTIFIER && !next.isNumber)
                    error("Expected register, identifier, or immediate value for memory operand at line ${token.line}")

                if (next.kind == Token.Kind.REGISTER) {
                    base = Register(CpuRegister.valueOf(next.text.uppercase()))
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
                        displacement = next.toUInt()
                        if (!hasToken()) error("Expected ']' after displacement in memory operand at line ${token.line}")
                        next = nextToken() // Consume for ']'
                    } else if (next.kind == Token.Kind.IDENTIFIER) {
                        // TODO: Handle [REG + IDENTIFIER] or [IDENTIFIER + IDENTIFIER] if needed (SIB-like)
                        error("Symbolic displacement (e.g., [REG + mySymbol]) not yet fully supported. Use immediate value. Line ${next.line}")
                    } else {
                        error("Expected number for displacement in memory operand at line ${next.line}")
                    }
                } else if (next.isNumber && base == null) { // Case: [imm32] - base was not set
                    displacement = next.toUInt()
                    if (!hasToken()) error("Expected ']' after immediate address in memory operand at line ${token.line}")
                    next = nextToken() // Check for ']'
                }
                // If 'next' is already RBRACKET here, it means [REG] or [IDENTIFIER] cases were handled

                if (next.kind != Token.Kind.RBRACKET) error("Expected ']' to close memory operand at line ${next.line}, found ${next.kind}")

                return Memory(base, displacement)
            }
            else -> error("Unknown or unexpected operand type: ${token.kind} ('${token.text}') at line ${token.line}")
        }
    }

    companion object {
        private fun Token.toUInt(): UInt {
            require(isNumber) { "Token is not a number: $this" }
            return when (kind) {
                Token.Kind.NUMBER_HEX -> text.removePrefix("0x").removeSuffix("h").toUInt(16)
                Token.Kind.NUMBER_BIN -> text.removePrefix("0b").removeSuffix("b").toUInt(2)
                Token.Kind.NUMBER_OCT -> text.removePrefix("0o").removeSuffix("o").toUInt(8)
                Token.Kind.NUMBER_DEC -> text.toUInt()
                else -> error("Invalid token kind for number conversion: $kind at line $line")
            }
        }

        private fun UInt.toBytes(type: String, line: Int): UByteArray {
            val size = when (type) {
                "BYTE", "DB" -> 1
                "WORD", "DW" -> 2
                "DWORD", "DD" -> 4
                "QWORD", "DQ" -> 8
                else -> error("Unknown data directive type: $type at line $line")
            }
            val buffer = ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN)
            when (size) {
                1 -> buffer.put(toByte())
                2 -> buffer.putShort(toShort())
                4 -> buffer.putInt(toInt())
                8 -> buffer.putLong(toLong())
            }
            return buffer.array().toUByteArray()
        }

        data class DataValue(
            val type: String,
            val bytes: UByteArray
        ) {
            override fun equals(other: Any?): Boolean {
                if (this === other) return true
                if (javaClass != other?.javaClass) return false

                other as DataValue

                if (type != other.type) return false
                if (!bytes.contentEquals(other.bytes)) return false

                return true
            }

            override fun hashCode(): Int {
                var result = type.hashCode()
                result = 31 * result + bytes.contentHashCode()
                return result
            }
        }
    }
}

fun dumpMemorySegments(memory: Memory, parser: Parser) {
    val dataSegmentBase = parser.codeSegmentBase.toString(16)
    val codeSegmentBase = parser.dataSegmentBase.toString(16)
    val stackSegmentBase = parser.stackSegmentBase.toString(16)

    println("\n--- Main function reporting --- ")
    println("Parser instance created. Check console output for parsing details and memory layout.")
    println("Code Segment Base (from parser): ${dataSegmentBase}h")
    println("Data Segment Base (from parser): ${codeSegmentBase}h")
    println("Stack Segment Base (from parser): ${stackSegmentBase}h")

    println("\nMemory content of Code Segment start (${dataSegmentBase}h):")
    if (parser.codeSegmentBase < memory.bytes) {
        val startAddr = parser.codeSegmentBase
        val endAddr = min(startAddr + 128, memory.bytes) // Print up to 128 bytes or end of memory
        memory.dumpMemory(start = startAddr, end = endAddr)
    }

    if (parser.dataSegmentBase < memory.bytes) {
        println("\nMemory content of Data Segment start (${codeSegmentBase}h):")
        val startAddr = parser.dataSegmentBase
        val endAddr = min(startAddr + 128, memory.bytes) // Print up to 128 bytes or end of memory
        memory.printMemory(start = startAddr, end = endAddr)
    }

    println("\nMemory content of Stack Segment start (${stackSegmentBase}h):")
    val endAddr = parser.stackSegmentBase
    val startAddr = max(endAddr - 128, 0) // Print up to 128 bytes or end of memory
    memory.printMemory(start = startAddr, end = endAddr)
}

fun main() {
    val src = try {
        File("src/main/kotlin/main.asm").readText()
    } catch (e: FileNotFoundException) {
        println("Error: ${e.message}, Source file not found: src/main/kotlin/main.asm")
        println("Please provide a valid path as a command-line argument or make sure the default file exists.")
        exitProcess(1)
    }

    val memory = Memory(1024L) // Example: 1KB of memory
    val parser = Parser(src, memory)

    // --- Memory dump ---
    dumpMemorySegments(memory, parser)

//    // --- Full memory dump ---
//    memory.printMemory()
}