package asm

import model.*

class Parser(src: String) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()

    private fun eat(kind: Token.Kind): Token {
        if (look.kind != kind) error("Parse error at line ${look.line}: expected $kind, got ${look.kind}")
        val t = look
        look = lex.nextToken()
        return t
    }

    private fun tryEat(kind: Token.Kind): Token? {
        if (look.kind == kind) return eat(kind)
        return null
    }

    private fun isNewlineOrEOF() = look.kind == Token.Kind.NEWLINE || look.kind == Token.Kind.EOF

    private fun parseNumber(text: String, line: Int): Int {
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
                // If it's an identifier not a register, treat it as a symbolic label.
                return Operand.LabelOp(idTok.text)
            }
            Token.Kind.NUMBER -> {
                val nTok = eat(Token.Kind.NUMBER)
                Operand.ImmOp(parseNumber(nTok.text, nTok.line))
            }
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: Int? = null
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) base = r else error("Line ${idTok.line}: unknown base register ${idTok.text}")
                        if (tryEat(Token.Kind.PLUS) != null) {
                            disp = when(look.kind){
                                Token.Kind.NUMBER -> parseNumber(eat(Token.Kind.NUMBER).text, idTok.line)
                                else -> error("Line ${idTok.line}: expected displacement number after +")
                            }
                        }
                    }
                    Token.Kind.NUMBER -> {
                        disp = parseNumber(eat(Token.Kind.NUMBER).text, look.line)
                    }
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
        val instructions: MutableList<Instruction>,
        val labels: MutableMap<String, Int> // label -> instruction index
    )

    fun parseProgram(): ParsedProgram {
        val instrs = mutableListOf<Instruction>()
        val labels = mutableMapOf<String, Int>()

        fun atLineStart(): Boolean = true // (not strictly tracked; fine for MVP)

        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instrs, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    // Could be a label or an opcode
                    val idTok = eat(Token.Kind.ID)
                    if (tryEat(Token.Kind.COLON) != null) {
                        // label
                        val name = idTok.text
                        labels[name] = instrs.size // points to next instruction index
                        // consume any trailing tokens until newline
                        while (!isNewlineOrEOF()) { look = lex.nextToken() }
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    } else {
                        // opcode path
                        val op = parseOp(idTok.text)
                        var dst: Operand? = null
                        var src: Operand? = null
                        if (!isNewlineOrEOF()) {
                            // dst
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) {
                                src = parseOperand()
                            }
                            // eat rest of line
                            while (!isNewlineOrEOF()) look = lex.nextToken()
                        }
                        tryEat(Token.Kind.NEWLINE)
                        instrs.add(Instruction(op, dst, src, null, idTok.line))
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