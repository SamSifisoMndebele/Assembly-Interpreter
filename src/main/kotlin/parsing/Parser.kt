package parsing

import assemble.Symbol
import ast.*
import ast.Operand.Immediate
import isa.CpuRegister
import isa.MnemonicOne
import isa.MnemonicTwo
import isa.MnemonicZero
import machine.Memory
import utils.toUBytes
import utils.removePrefixes
import utils.removeSuffixes
import kotlin.math.min

@OptIn(ExperimentalUnsignedTypes::class)
class Parser(source: String, private val memory: Memory) : Lexer(source) {
    private val instructions = mutableListOf<Instruction>()
    private val dataEntries = mutableListOf<DataEntry>()

    private val symbols = mutableMapOf<String, Symbol>() // Stores Symbol objects
    fun getSymbols(): Map<String, Symbol> = symbols // Updated return type

    // Segment base addresses and alignment
    var codeSegmentBase: Long = 0L
    var dataSegmentBase: Long = 0L
        private set
    val stackSegmentBase: Long = memory.bytes

    init {
        println("Tokens:")
        getTokens().forEach { println(it) } // Debug: Print all tokens first

        parseStackSegment()

        // 1. Parse DATA segment
        parseDataSegment()

        // 2. Calculate DATA segment base address
        dataSegmentBase = instructions.size * 16L
        if (dataSegmentBase >= memory.bytes) error("Calculated data segment base ($dataSegmentBase) is outside memory bounds (${memory.bytes}).")
        if (stackSegmentBase < dataSegmentBase) error("Stack segment base ($stackSegmentBase) is before data segment base ($dataSegmentBase).")

        val dataSegStr = dataSegmentBase.toString(16)
        val codeSegStr = codeSegmentBase.toString(16)
        val stackSegStr = stackSegmentBase.toString(16)

        println("\n--- Segmentation --- ")
        println("Code Segment Base  : ${codeSegStr}h")
        println("Data Segment Base  : ${dataSegStr}h")
        println("Stack Segment Base : ${stackSegStr}h")

        // For actual memory writing address
        var memoryCursor = 0L

        // 3. Write DATA segment to memory
        memoryCursor = dataSegmentBase
        dataEntries.forEach { dataEntry ->
            symbols[dataEntry.name] = Symbol(dataEntry.type, dataEntry.length, memoryCursor)
            if (dataEntry.bytes != null) {
                for (byte in dataEntry.bytes) {
                    memory.writeByte(memoryCursor++, byte)
                }
            } else {
                memory.writeByte(memoryCursor++, 0u.toUByte())
            }
        }
        if (dataSegmentBase < memory.bytes) {
            println("\nMemory content of Data Segment [${dataSegStr}h, ${(memoryCursor-1).toString(16)}h]:")
            val startAddr = dataSegmentBase
            val endAddr = min(memoryCursor, memory.bytes)
            memory.dumpMemory(start = startAddr, end = endAddr)
        }
        println("\nSymbol table:")
        println("+------------------+----------+-----+")
        println("|$BOLD Name             $RESET|$BOLD Address  $RESET|$BOLD Len $RESET|")
        println("+------------------+----------+-----+")
        symbols.forEach { (name, symbol) ->
            val hex = String.format("%08X", symbol.address)
            val coloredHex = "$YELLOW${hex.substring(0, 7)}$BLUE${hex.last()}$RESET"
            println(String.format("| %-16s | $coloredHex | %-3d |", name, symbol.length))
        }
        println("+------------------+----------+-----+")

        // Set the code segment to start exactly where the data segment ended
        codeSegmentBase = memoryCursor

        // 4. Parse CODE segment
        parseCodeSegment()

        // 5. Write CODE segment to memory
        memoryCursor = codeSegmentBase
        instructions.forEach { instruction ->
            val encodedBytes = instruction.encode(symbols) // Pass Map<String, Symbol>
            encodedBytes.forEach { byte ->
                if (memoryCursor < memory.bytes) { // Check memory bounds
                    memory.writeByte(memoryCursor, byte)
                    memoryCursor++
                } else {
                    error("Memory overflow while writing code segment at address $memoryCursor. Max memory: ${memory.bytes}")
                }
            }
        }
        if (codeSegmentBase < memory.bytes) {
            println("\nMemory content of Code Segment start [${codeSegStr}h, ${(memoryCursor-1).toString(16)}h]:")
            val startAddr = codeSegmentBase
            val endAddr = min(memoryCursor, memory.bytes)
            memory.dumpMemory(start = startAddr, end = endAddr)
        }
    }

    private fun parseDataSegment() {
        if (toDataSegment()) {
            while (hasToken()) {
                when (val token = nextToken()) {
                    is Token.Segment -> break
                    is Token.Identifier -> parseDataDefinition(token)
                    else -> error("Unexpected token: $token")
                }
            }
        }
    }

    private fun parseCodeSegment() {
        if (toCodeSegment()) {
            while (hasToken()) {
                when (val token = nextToken()) {
                    is Token.Segment -> break
                    is Token.Mnemonic -> parseInstruction(token)
                    is Token.Label -> {
                        // Handle label definitions (e.g., "myLabel:")
                        // For now, we'll just print it. Label handling will be more complex.
                        println("Found label: ${token.text}")
                    }
                    else -> error("Unexpected token: $token")
                }
            }
        }
    }

    private fun parseStackSegment() {
        if (toStackSegment()) {
            println("Stack segment detected. Dynamic stack allocated at ${stackSegmentBase.toString(16)}h")

            // Consume tokens until the next segment or EOF to avoid parsing errors
            while (hasToken()) {
                val next = peekToken()
                if (next is Token.Segment) {
                    break // Stop consuming if we hit .code or .data
                }
                nextToken() // Consume the stack size/definitions (e.g., 4096)
            }
        }
    }
    private fun parseDataDefinition(token: Token) {
        require(token is Token.Identifier)
        val name = token.text
        val line = token.line

        if (!hasToken() || peekToken() !is Token.DataDir) {
            error("Expected data directive (BYTE, WORD, DWORD, QWORD) after identifier '$name' at line $line, found ${if(hasToken()) peekToken() else "EOF"}")
        }

        val dataValue = nextDataValue()
        val symbol = DataEntry(name, dataValue.type, dataValue.bytes, line)
        dataEntries.add(symbol)
    }

    private fun nextDataValue(): DataValue {
        val token = nextToken() // Consume data directive
        require(token is Token.DataDir) { "Expected data directive, found $token" }
        val values = mutableListOf<UByte>()
        val type = token.text.uppercase()
        val line = token.line
        var firstValue = true
        while (hasToken()) {
            when (val valueToken = peekToken()) {
                is Token.Text -> {
                    nextToken() // Consume text token
                    if (type != "DB" && type != "BYTE") error("Strings can only be defined with BYTE directive at line ${valueToken.line}")
                    // Remove quotes and convert to UByteArray
                    valueToken.text.trim('\'', '"').forEach { char ->
                        values.add(char.code.toUByte())
                    }
                }
                is Token.HexNumber, is Token.BinNumber, is Token.OctNumber, is Token.DecNumber -> {
                    nextToken() // Consume number token
                    val bytes = valueToken.toUInt().toUBytes(type, valueToken.line)
                    values.addAll(bytes)
                }
                is Token.UNKNOWN -> { // Handle '?'
                    nextToken() // Consume ? token
                    val bytes = 0u.toUBytes(type, valueToken.line) // Use 0 as the placeholder value
                    values.addAll(bytes)
                }
                else -> {
                    // If not a value, it might be the end of this definition or a new line/segment
                    if (firstValue) error("Expected data value for $type at line $line, found $valueToken")
                    return DataValue(type, values.toUByteArray()) // End of current data definition's values
                }
            }
            firstValue = false
            // Check for comma or end of line/definition
            if (hasToken() && peekToken() is Token.Comma) {
                nextToken() // Consume comma
                if (!hasToken() || (!peekToken().isNumber && peekToken() !is Token.Text)) {
                    error("Expected data value after comma for $type at line $line")
                }
            } else break // No comma, so end of values for this definition
        }

        if (firstValue) error("Expected data value for $type at line $line but found none.")
        return DataValue(type, values.toUByteArray())
    }

    private fun parseInstruction(token: Token) {
        require(token is Token.Mnemonic)
        val operationName = token.text.uppercase()
        val line = token.line

        val operationZero = MnemonicZero::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as MnemonicZero?
        if (operationZero != null) {
            instructions.add(InstructionZero(operationZero, line))
            return
        }

        val operationOne = MnemonicOne::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as MnemonicOne?
        if (operationOne != null) {
            if (!hasToken()) error("Expected operand for $operationOne at line $line, but found no more tokens.")
            val operand = nextOperand()
            instructions.add(InstructionOne(operationOne, operand, line))
            return
        }

        val operationTwo = MnemonicTwo::class.nestedClasses.find {
            it.simpleName?.uppercase() == operationName
        }?.objectInstance as MnemonicTwo?
        if (operationTwo != null) {
            if (!hasToken()) error("Missing or invalid destination operand for $operationTwo at line $line")
            val destOperand = nextOperand()
            if (!hasToken() || nextToken() !is Token.Comma) {
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
        return when (val token = nextToken()) {
            is Token.Register -> Register(CpuRegister.valueOf(token.text.uppercase()))
            is Token.HexNumber -> Immediate(token.text.removeSuffix("h").removePrefix("0x").toUInt(16))
            is Token.DecNumber -> Immediate(token.text.removeSuffix("b").toUInt())
            is Token.BinNumber -> Immediate(token.text.removeSuffix("b").removePrefix("0b").toUInt(2))
            is Token.OctNumber -> Immediate(token.text.removeSuffixes("o", "q").removePrefixes("0o", "0q").toUInt(8))
            is Token.Identifier -> Identifier(token.text)
            is Token.Text -> {
                val rawText = token.text.trim('\'', '"')

                if (rawText.length <= 4) {
                    // MASM allows 1 to 4-character strings as Immediate numbers.
                    // It packs them into a 32-bit integer (e.g., 'ABCD' -> 0x41424344)
                    var numericValue = 0u
                    for (char in rawText) {
                        numericValue = (numericValue shl 8) or char.code.toUInt()
                    }
                    Immediate(numericValue)
                } else {
                    // Strict Error: Standard x86 cannot push strings longer than 4 bytes directly.
                    error("String literal \"$rawText\" is too long for an instruction operand at line ${token.line}. Define long strings in the .data segment.")
                }
            }
            is Token.Label -> Label(token.text)
            is Token.LBracket -> {
                // Memory operand parsing: [base + index*scale + displacement]
                if (!hasToken()) error("Incomplete memory operand at line ${token.line}")

                var base: Register? = null
                var index: Register? = null
                var scale = 1u
                var displacement: Long? = null

                fun parseRegister(name: String) = Register(CpuRegister.valueOf(name.uppercase()))

                var expectOperand = true
                var next = nextToken()

                while (true) {
                    if (expectOperand) {
                        when {
                            next is Token.Register -> {
                                val peek = peekToken()
                                if (peek is Token.Multi) {
                                    if (index != null) error("Multiple index registers in memory operand at line ${next.line}")
                                    index = parseRegister(next.text)
                                    require(index.cpuRegister != CpuRegister.ESP) {
                                        "ESP cannot be used as an index register"
                                    }
                                    nextToken()
                                    scale = nextToken().toUInt()
                                    require(scale in listOf(1u, 2u, 4u, 8u))
                                } else {
                                    val reg = parseRegister(next.text)
                                    when {
                                        base == null -> base = reg
                                        index == null -> {
                                            index = reg
                                            require(index.cpuRegister != CpuRegister.ESP) {
                                                "ESP cannot be used as an index register"
                                            }
                                        }
                                        else -> error("Too many registers in memory operand at line ${next.line}")
                                    }
                                }
                            }
                            next is Token.Identifier -> {
                                val symbol = symbols[next.text] ?: error("Undefined symbol '${next.text}' in memory operand at line ${next.line}")
                                val value = when (symbol.size) {
                                    1 -> memory.readByte(symbol.address).toLong()
                                    2 -> memory.readWord(symbol.address).toLong()
                                    4 -> memory.readDWord(symbol.address).toLong()
                                    else -> error("Unknown data directive type: ${symbol.type} at line ${next.line}")
                                }
                                displacement = (displacement ?: 0) + value
                            }
                            next.isNumber -> displacement = (displacement ?: 0) + next.toLong()
                            else -> error("Expected register, identifier or number at line $next")
                        }
                        expectOperand = false
                        continue
                    }
                    if (!hasToken()) error("Unclosed memory operand at line ${token.line}")
                    next = nextToken()
                    when (next) {
                        is Token.RBracket -> break
                        is Token.Plus -> {
                            next = nextToken()
                            expectOperand = true
                        }
                        else -> error("Unexpected token $next in memory operand at line ${next.line}")
                    }
                }

                ast.Memory(base, index, scale, displacement)
            }
            else -> error("Unknown or unexpected operand type: ${token::class.simpleName} ('${token.text}') at line ${token.line}")
        }
    }

    companion object {
        const val RESET = "\u001B[0m"
        const val YELLOW = "\u001B[33m"
        const val BLUE = "\u001B[34m"
        const val BOLD = "\u001B[1m"

        private fun Token.toUInt(): UInt {
            require(isNumber) { "Token is not a number: $this" }
            return when (this) {
                is Token.HexNumber -> text.removePrefix("0x").removeSuffix("h").toUInt(16)
                is Token.BinNumber -> text.removePrefix("0b").removeSuffix("b").toUInt(2)
                is Token.OctNumber -> text.removePrefix("0o").removeSuffix("o").toUInt(8)
                is Token.DecNumber -> text.toUInt()
                else -> error("Invalid token kind for number conversion: $this at line $line")
            }
        }

        private fun Token.toLong(): Long {
            require(isNumber) { "Token is not a number: $this" }
            return when (this) {
                is Token.HexNumber -> text.removePrefix("0x").removeSuffix("h").toLong(16)
                is Token.BinNumber -> text.removePrefix("0b").removeSuffix("b").toLong(2)
                is Token.OctNumber -> text.removePrefix("0o").removeSuffix("o").toLong(8)
                is Token.DecNumber -> text.toLong()
                else -> error("Invalid token kind for number conversion: $this at line $line")
            }
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