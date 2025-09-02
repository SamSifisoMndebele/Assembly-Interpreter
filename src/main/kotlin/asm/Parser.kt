package asm

import cpu.Memory
import model.*

/**
 * Parses assembly source code into a list of instructions and a map of labels.
 *
 * The parser supports the following features:
 * - Sections: `.data` and `.code` for organizing data and instructions.
 * - Labels: For defining jump targets and data locations.
 * - Instructions: A subset of x86 assembly instructions (MOV, ADD, SUB, etc.).
 * - Operands: Registers (AX, BX, etc.), immediate values, memory addresses (direct and register-based).
 * - Data Directives: `DB` (Define Byte), `DW` (Define Word), `DD` (Define Double Word) for reserving and initializing memory.
 * - Comments: Lines starting with `;` or everything after a `:` in a label definition.
 *
 * The parser performs a two-pass process (conceptually):
 * 1. **Symbol Table Construction:** Identifies labels and data symbols, storing their addresses/offsets.
 * 2. **Instruction Generation:** Parses instructions, resolving symbols to their addresses/offsets.
 *
 * **Memory Layout:**
 * - Data segment starts at a defined base address (e.g., `0x1000u`).
 *
 * @property mem The [Memory] instance where data will be stored.
 * @param src The assembly source code as a string.
 */
class Parser(src: String, private val mem: Memory) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()
    private var currentSection = Section.CODE
    private val symbolTable = mutableMapOf<String, UInt>() // symbol -> offset within data segment
    private var currentDataOffset = 0u // Current offset within the data segment (UInt)
    
    companion object {
        private const val DATA_SEGMENT_BASE = 0x1000u // Data segment starts at 4KB (UInt)

        private val String.asOperation: Operation?
            get() =  when (this.uppercase()) {
                "MOV" -> Operation.OperationTwo.MOV
                "ADD" -> Operation.OperationTwo.ADD
                "SUB" -> Operation.OperationTwo.SUB
                "XCHG" -> Operation.OperationTwo.XCHG
                "INC" -> Operation.OperationOne.INC
                "DEC" -> Operation.OperationOne.DEC
                "PUSH" -> Operation.OperationOne.PUSH
                "POP" -> Operation.OperationOne.POP
                "JMP" -> Operation.OperationOne.JMP
                "CALL" -> Operation.OperationOne.CALL
                "RET" -> Operation.OperationZero.RET
                "NOP" -> Operation.OperationZero.NOP
                "CMP" -> Operation.OperationTwo.CMP
                "JG", "JNLE" -> Operation.OperationOne.JG
//                "AND" -> Operation.OperationTwo.AND
//                "OR" -> Operation.OperationTwo.OR
//                "XOR" -> Operation.OperationTwo.XOR
//                "TEST" -> Operation.OperationTwo.TEST
//                "LEA" -> Operation.OperationTwo.LEA
//                "NEG" -> Operation.OperationOne.NEG
//                "NOT" -> Operation.OperationOne.NOT
//                "MUL" -> Operation.OperationOne.MUL
//                "IMUL" -> Operation.OperationOne.IMUL
//                "DIV" -> Operation.OperationOne.DIV
//                "IDIV" -> Operation.OperationOne.IDIV
//                "JE", "JZ" -> Operation.OperationOne.JE
//                "JNE", "JNZ" -> Operation.OperationOne.JNE
//                "JL", "JNGE" -> Operation.OperationOne.JL
//                "JGE", "JNL" -> Operation.OperationOne.JGE
//                "JLE", "JNG" -> Operation.OperationOne.JLE
//                "JA", "JNBE" -> Operation.OperationOne.JA
//                "JAE", "JNB", "JNC" -> Operation.OperationOne.JAE
//                "JB", "JNAE", "JC" -> Operation.OperationOne.JB
//                "JBE", "JNA" -> Operation.OperationOne.JBE
//                "INT" -> Operation.OperationOne.INT
//                 "HLT" -> Operation.OperationZero.HLT

                // String operations
//                 "MOVSB", "MOVSW", "MOVSD" -> Operation.OperationZero.MOVS // Assuming a generic MOVS
//                 "CMPSB", "CMPSW", "CMPSD" -> Operation.OperationZero.CMPS
//                 "SCASB", "SCASW", "SCASD" -> Operation.OperationZero.SCAS
//                 "LODSB", "LODSW", "LODSD" -> Operation.OperationZero.LODS
//                 "STOSB", "STOSW", "STOSD" -> Operation.OperationZero.STOS

                // Shift and Rotate operations - Assuming they are two-operand for now
//                 "SHL", "SAL" -> Operation.OperationTwo.SHL
//                 "SHR" -> Operation.OperationTwo.SHR
//                 "SAR" -> Operation.OperationTwo.SAR
//                 "ROL" -> Operation.OperationTwo.ROL
//                 "ROR" -> Operation.OperationTwo.ROR
//                 "RCL" -> Operation.OperationTwo.RCL
//                 "RCR" -> Operation.OperationTwo.RCR
                else -> null
            }
    }

    private fun eat(kind: Token.Kind): Token {
        if (look.kind != kind) error("Parse error at line ${look.line}: expected $kind, got ${look.kind}")
        val t = look
        look = lex.nextToken()
        return t
    }

    private fun tryEat(kind: Token.Kind): Token? = if (look.kind == kind) eat(kind) else null

    private fun isNewlineOrEOF() = look.kind == Token.Kind.NEWLINE || look.kind == Token.Kind.EOF

    private fun parseImmediate(text: String): UInt {
        val t = text.lowercase().replace("_", "")
        return when {
            t.startsWith("0x") -> t.substring(2).toUInt(16)
            t.endsWith("h") -> t.dropLast(1).toUInt(16)
            t.startsWith("0b") -> t.substring(2).toUInt(2)
            t.endsWith("b") -> t.dropLast(1).toUInt(2)
            t.startsWith("0o") -> t.substring(2).toUInt(8)
            t.endsWith("o") || t.endsWith("q") -> t.dropLast(1).toUInt(8)
            // Handle character literals like 'A'
            t.length == 3 && t.startsWith("'") && t.endsWith("'") -> t[1].code.toUInt()
            // Try parsing as Long first to handle potential negative signs, then convert to UInt.
            // This allows parsing "-1" and getting MAX_UINT, for example.
            // Direct toUInt() would fail for negative strings.
            else -> try {
                t.toLong().toUInt()
            } catch (e: NumberFormatException) {
                // If toLong fails, try toUInt for positive numbers that might be too large for Long but fit UInt.
                // This is less common for typical assembly immediates but provides robustness.
                try {
                    t.toUInt()
                } catch (e2: NumberFormatException) {
                    throw NumberFormatException("Invalid number format: $text -> $t")
                }
            }
        }
    }

    private fun parseReg(id: String): Reg? = when(id.uppercase()) {
        "AL" -> Reg.AL
        "AH" -> Reg.AH
        "BL" -> Reg.BL
        "BH" -> Reg.BH
        "CL" -> Reg.CL
        "CH" -> Reg.CH
        "DL" -> Reg.DL
        "DH" -> Reg.DH
        "AX"-> Reg.AX
        "BX"-> Reg.BX
        "CX"-> Reg.CX
        "DX"-> Reg.DX
        "SI"-> Reg.SI
        "DI"-> Reg.DI
        "BP"-> Reg.BP
        "SP"-> Reg.SP
        "EAX" -> Reg.EAX
        "EBX" -> Reg.EBX
        "ECX" -> Reg.ECX
        "EDX" -> Reg.EDX
        "ESI" -> Reg.ESI
        "EDI" -> Reg.EDI
        "EBP" -> Reg.EBP
        "ESP" -> Reg.ESP
        else -> null
    }

    private fun parseOperand(): Operand {
        return when (look.kind) {
            Token.Kind.ID -> {
                val idTok = eat(Token.Kind.ID)
                val tokenText = idTok.text
                parseReg(tokenText)?.let { return Operand.RegOp(it) }
                symbolTable[tokenText]?.let { offset -> // offset is UInt
                    return Operand.MemOp(null, offset) // MemOp expects UInt?
                }
                return try {
                    val immediateValue = parseImmediate(tokenText) // returns UInt
                    Operand.ImmOp(immediateValue) // ImmOp expects UInt
                } catch (e: NumberFormatException) {
                    Operand.LabelOp(tokenText)
                }
            }
            Token.Kind.NUMBER -> Operand.ImmOp(parseImmediate(eat(Token.Kind.NUMBER).text)) // parseImmediate returns UInt
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: UInt? = null // Changed to UInt?
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) {
                            base = r
                            if (tryEat(Token.Kind.PLUS) != null) {
                                disp = parseImmediate(eat(Token.Kind.NUMBER).text) // returns UInt
                            }
                        } else {
                            val symbolOffset = symbolTable[idTok.text] // UInt?
                                ?: error("Line ${idTok.line}: Unknown symbol '${idTok.text}' in memory operand")
                            disp = symbolOffset
                            if (tryEat(Token.Kind.PLUS) != null) {
                                val immediateOffset = parseImmediate(eat(Token.Kind.NUMBER).text) // UInt
                                disp = (disp ?: 0u) + immediateOffset // disp becomes UInt
                            }
                        }
                    }
                    Token.Kind.NUMBER -> { 
                        disp = parseImmediate(eat(Token.Kind.NUMBER).text) // returns UInt
                    }
                    else -> error("Line ${look.line}: bad memory operand, expected ID or NUMBER inside brackets")
                }
                eat(Token.Kind.RBRACK)
                Operand.MemOp(base, disp) // disp is UInt?
            }
            else -> error("Line ${look.line}: unexpected token ${look.kind}")
        }
    }

    // Updated to return Operation
    private fun parseOp(id: String): Operation {
        val upperId = id.uppercase()
        // Direct lookup first
        upperId.asOperation?.let { return it }

        // Removed suffix logic as Operation objects should be distinct
        // If MOVB, MOVW, MOVD are needed, they should be distinct entries in opStringToOperationMap
        // mapping to specific Operation objects e.g. Operation.OperationTwo.MOVB if that exists.

        error("Line ${look.line}: unknown opcode '$id' (parsed as '$upperId')")
    }

    data class ParsedProgram(
        val instructions: List<Instruction>,
        val labels: MutableMap<String, UInt> // label -> instruction index (UInt)
    )

    /**
     * Parses the entire assembly program.
     *
     * This function iterates through the tokens provided by the lexer,
     * processing each line according to whether it's in the `.code` or `.data` section.
     *
     * In the `.code` section:
     * - It identifies labels and stores their corresponding instruction index.
     * - It parses instructions and their operands, creating [Instruction] objects.
     *
     * In the `.data` section:
     * - It identifies data symbols and stores their memory addresses (relative to `DATA_SEGMENT_BASE`).
     * - It parses data directives (like `DB`, `DW`, `DD`) and writes the specified values
     *   into the [mem] (Memory) instance at the calculated physical address.
     *   The `currentDataOffset` tracks the current position within the data segment.
     *
     * The function handles:
     * - End of file (EOF) to terminate parsing.
     * - Newlines to advance to the next line.
     * - Label definitions (e.g., `myLabel:`).
     * - Section directives (`.data`, `.code`).
     * - Instructions with varying numbers of operands (zero, one, or two).
     * - Data definitions with type specifiers and values.
     * - Consumption of comments or any remaining tokens on a line after processing
     *   the main part (label, directive, or instruction).
     *
     * Errors are thrown for syntax issues, unknown opcodes, incorrect operand counts,
     * or unsupported data directives.
     *
     * @return A [ParsedProgram] object containing the list of parsed [Instruction]s
     *         and a map of label names to their instruction indices.
     * @throws IllegalStateException if parsing errors occur (e.g., unexpected token,
     *         unknown opcode, incorrect operand count).
     */
    fun parseProgram(): ParsedProgram {
        val instructions = mutableListOf<Instruction>()
        val labels = mutableMapOf<String, UInt>() // label name -> instruction index (UInt)
        
        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instructions, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    val idTok = eat(Token.Kind.ID)
                    val id = idTok.text

                    if (tryEat(Token.Kind.COLON) != null) { // Label definition
                        if (currentSection == Section.CODE) {
                            labels[id] = instructions.size.toUInt()
                        } else if (currentSection == Section.DATA) {
                            symbolTable[id] = DATA_SEGMENT_BASE + currentDataOffset
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    if (id.startsWith(".")) { // Section directive
                        when(id.lowercase()) {
                            ".data" -> {
                                currentSection = Section.DATA
                                currentDataOffset = 0u 
                            }
                            ".code" -> currentSection = Section.CODE
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    if (currentSection == Section.CODE) {
                        val op = parseOp(id) // Returns Operation
                        var dst: Operand? = null
                        var src: Operand? = null
                        
                        // Parse operands based on arity suggested by Operation type if possible,
                        // or parse up to two and let the when block validate.
                        // For simplicity, parse up to two for now.
                        if (!isNewlineOrEOF() && look.kind != Token.Kind.COLON) { // Stop if label follows
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) {
                                if (!isNewlineOrEOF() && look.kind != Token.Kind.COLON) {
                                    src = parseOperand()
                                } else {
                                     error("Line ${idTok.line}: Expected second operand after comma for instruction '$id'")
                                }
                            }
                        }
                        
                        // Consume rest of the line (e.g. comments after instruction)
                        while (!isNewlineOrEOF()) look = lex.nextToken() 
                        tryEat(Token.Kind.NEWLINE)

                        when (op) {
                            is Operation.OperationZero -> {
                                if (dst != null || src != null) error("Line ${idTok.line}: Opcode '$id' expects 0 operands, got ${listOfNotNull(dst,src).size}")
                                instructions.add(Instruction.InstructionZero(op, idTok.line))
                            }
                            is Operation.OperationOne -> {
                                if (dst == null || src != null) error("Line ${idTok.line}: Opcode '$id' expects 1 operand, got ${listOfNotNull(dst,src).size}")
                                instructions.add(Instruction.InstructionOne(op, dst!!, idTok.line))
                            }
                            is Operation.OperationTwo -> {
                                if (dst == null || src == null) error("Line ${idTok.line}: Opcode '$id' expects 2 operands, got ${listOfNotNull(dst,src).size}")
                                instructions.add(Instruction.InstructionTwo(op, dst!!, src!!, idTok.line))
                            }
                            // else -> error("Line ${idTok.line}: Unhandled Operation type for opcode '$id'") // Should not happen if parseOp is correct
                        }

                    } else if (currentSection == Section.DATA) {
                        val symbolName = id 
                        val typeTok = eat(Token.Kind.ID) 

                        var value: UInt
                        // Check for '?' for uninitialized data.
                        // Assuming '?' is tokenized as Token.Kind.ID.
                        // If Lexer makes '?' a distinct token kind (e.g. Token.Kind.QUESTION),
                        // this check should be: if (look.kind == Token.Kind.QUESTION)
                        if (look.kind == Token.Kind.ID && look.text == "?") {
                            eat(Token.Kind.ID) // Consume '?'
                            value = 0u
                        } else if (look.kind == Token.Kind.NUMBER || look.kind == Token.Kind.ID) {
                            // Token is an actual number, a character literal ('A'), or a label.
                            val valueToken = eat(look.kind) // Consume the token for the value
                            value = parseImmediate(valueToken.text)
                        } else {
                            // No explicit value provided (e.g., "myVar DB" followed by newline or comment).
                            // Default to 0, which is standard for uninitialized data or BSS-like behavior.
                            value = 0u
                        }
                        
                        symbolTable[symbolName] = DATA_SEGMENT_BASE + currentDataOffset

                        val physicalAddress = (DATA_SEGMENT_BASE + currentDataOffset) 

                        when (typeTok.text.uppercase()) {
                            "DB", "BYTE" -> {
                                if (value > UByte.MAX_VALUE) error("Line ${typeTok.line}: Value $value out of 8-bit unsigned range for DB/BYTE")
                                mem.writeByte(physicalAddress.toInt(), value.toUByte())
                                currentDataOffset += 1u
                            }
                            "DW", "WORD" -> {
                                if (value > UShort.MAX_VALUE) error("Line ${typeTok.line}: Value $value out of 16-bit unsigned range for DW/WORD")
                                mem.writeWord(physicalAddress.toInt(), value.toUShort())
                                currentDataOffset += 2u
                            }
                            "DD", "DWORD", "LONG" -> {
                                // Value is already UInt, which matches writeDWord expectation
                                mem.writeDWord(physicalAddress.toInt(), value)
                                currentDataOffset += 4u
                            }
                            else -> error("Line ${typeTok.line}: Unsupported data directive '${typeTok.text}'. Supported: DB, BYTE, DW, WORD, DD, DWORD, LONG")
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                    }
                }
                else -> { 
                    while (!isNewlineOrEOF()) look = lex.nextToken()
                    tryEat(Token.Kind.NEWLINE)
                }
            }
        }
    }
}
