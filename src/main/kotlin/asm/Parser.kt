package asm

import cpu.Memory
import model.*

class Parser(src: String, private val mem: Memory) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()
    private var currentSection = Section.CODE
    private val dataSymbols = mutableMapOf<String, Int>() // symbol -> address
    private var dataPtr = 0x1000 // Data segment starts at 4KB, leaving space for system/code

    private fun eat(kind: Token.Kind): Token {
        if (look.kind != kind) error("Parse error at line ${look.line}: expected $kind, got ${look.kind}")
        val t = look
        look = lex.nextToken()
        return t
    }
    private fun tryEat(kind: Token.Kind): Token? = if (look.kind == kind) eat(kind) else null
    private fun isNewlineOrEOF() = look.kind == Token.Kind.NEWLINE || look.kind == Token.Kind.EOF

    private fun parseNumber(text: String): Int {
        val t = text.lowercase()
        return when {
            t.startsWith("0x") -> t.substring(2).toInt(16)
            t.endsWith("h") -> t.dropLast(1).toInt(16)
            else -> t.toInt()
        } and 0xFFFF
    }
    private fun parseReg(id: String): Reg? = when(id.uppercase()) {
        "AX"-> Reg.AX
        "BX"-> Reg.BX
        "CX"-> Reg.CX
        "DX"-> Reg.DX
        "SI"-> Reg.SI
        "DI"-> Reg.DI
        "BP"-> Reg.BP
        "SP"-> Reg.SP
        else -> null
    }

    private fun parseOperand(): Operand {
        return when (look.kind) {
            Token.Kind.ID -> {
                val idTok = eat(Token.Kind.ID)
                val reg = parseReg(idTok.text)
                if (reg != null) return Operand.RegOp(reg)
                val symAddr = dataSymbols[idTok.text]
                if (symAddr != null) return Operand.MemOp(null, symAddr) // `mov ax, var` -> `mov ax, [address_of_var]`
                // If it's not a register or a known data symbol, assume it's a code label.
                return Operand.LabelOp(idTok.text)
            }
            Token.Kind.NUMBER -> Operand.ImmOp(parseNumber(eat(Token.Kind.NUMBER).text))
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: Int? = null
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) base = r else {
                            val addr = dataSymbols[idTok.text] ?: error("Unknown symbol ${idTok.text}")
                            disp = addr
                        }
                        if (tryEat(Token.Kind.PLUS) != null) disp = parseNumber(eat(Token.Kind.NUMBER).text)
                    }
                    Token.Kind.NUMBER -> { disp = parseNumber(eat(Token.Kind.NUMBER).text) }
                    else -> error("Line ${look.line}: bad memory operand")
                }
                eat(Token.Kind.RBRACK)
                Operand.MemOp(base, disp)
            }
            else -> error("Line ${look.line}: unexpected token ${look.kind}")
        }
    }

    private fun parseOp(id: String): Op = when(id.uppercase()) {
        "MOV"-> Op.MOV
        "ADD"-> Op.ADD
        "SUB"-> Op.SUB
        "INC"-> Op.INC
        "DEC"-> Op.DEC
        "CMP"-> Op.CMP
        "JMP"-> Op.JMP
        "JE"-> Op.JE
        "JNE"-> Op.JNE
        "JG"-> Op.JG
        "JL"-> Op.JL
        "JGE"-> Op.JGE
        "JLE"-> Op.JLE
        "PUSH"-> Op.PUSH
        "POP"-> Op.POP
        "CALL"-> Op.CALL
        "RET"-> Op.RET
        "INT"-> Op.INT
        "NOP"-> Op.NOP
        else -> error("Line ${look.line}: unknown opcode $id")
    }

    data class ParsedProgram(
        val instructions: List<Instruction>,
        val labels: MutableMap<String, Int> // label -> instruction index
    )

    fun parseProgram(): ParsedProgram {
        val instructions = mutableListOf<Instruction>()
        val labels = mutableMapOf<String, Int>()
        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instructions, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    // Could be a label or an opcode
                    val idTok = eat(Token.Kind.ID)
                    val id = idTok.text

                    // Comments
                    if (tryEat(Token.Kind.COLON) != null) {
                        labels[id] = instructions.size // points to the next instruction index
                        // consume any trailing tokens until the newline
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    // Check section
                    if (id.startsWith(".")) {
                        when(id.lowercase()) {
                            ".data" -> currentSection = Section.DATA
                            ".code" -> currentSection = Section.CODE
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    // Opcode
                    if (currentSection == Section.CODE) {
                        val op = parseOp(id)
                        var dst: Operand? = null
                        var src: Operand? = null
                        if (!isNewlineOrEOF()) {
                            // dst
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) src = parseOperand()
                            // eat rest of line
                            while (!isNewlineOrEOF()) look = lex.nextToken()
                        }
                        tryEat(Token.Kind.NEWLINE)
                        instructions.add(Instruction(op, dst, src, idTok.line))
                        continue
                    }

                    // Data
                    if (currentSection == Section.DATA) {
                        val typeTok = eat(Token.Kind.ID)
                        if (typeTok.text.uppercase() != "DW") error("Only DW supported")
                        val valTok = if (look.kind == Token.Kind.NUMBER) eat(Token.Kind.NUMBER) else null
                        val value = if (valTok != null) parseNumber(valTok.text) else 0
                        dataSymbols[id] = dataPtr
                        mem.write16(dataPtr, value)
                        dataPtr += 2
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }
                }
                else -> {
                    // skip unexpected till newline
                    while (!isNewlineOrEOF()) look = lex.nextToken()
                    tryEat(Token.Kind.NEWLINE)
                }
            }
        }
    }
}