package lexical

import model.Operation.Companion.allOperations
import java.io.File
import java.io.FileNotFoundException
import java.util.regex.Pattern
import kotlin.system.exitProcess

class Lexer(source: String) {
    companion object {
        // Regex patterns
        private val hexPattern = Pattern.compile("^(?:0x[0-9a-f]+|[0-9][0-9a-f]*h)", Pattern.CASE_INSENSITIVE)
        private val binPattern = Pattern.compile("^(?:0b[01]+|[01]+b)", Pattern.CASE_INSENSITIVE)
        private val octPattern = Pattern.compile("^(?:0[qo][0-7]+|[0-7]+[qo])", Pattern.CASE_INSENSITIVE)
        private val decPattern = Pattern.compile("^[-+]?[0-9]+(?:_[0-9]+)*d?", Pattern.CASE_INSENSITIVE)
        private val identifierPattern = Pattern.compile("^[A-Za-z_][A-Za-z0-9_]*")
        private val labelPattern = Pattern.compile("^[A-Za-z_][A-Za-z0-9_]*\\s*:")
        private val registerPattern = Pattern.compile("^(r[0-9]+|e?[abcd]x|[sd]i|[sb]p)", Pattern.CASE_INSENSITIVE)
        private val segmentPattern = Pattern.compile("^(?:section\\s+)?\\.(code|data|stack)", Pattern.CASE_INSENSITIVE)
        private val stringPattern = Pattern.compile("^\"([^\"\\\\]|\\\\.)*\"|^'([^'\\\\]|\\\\.)*'")
        private val dataDirectivePattern = Pattern.compile("^(byte|word|dword|db|dw|dd|dq|dt)", Pattern.CASE_INSENSITIVE)

        private val operations = allOperations.map { it.toString().lowercase() }
    }

    private val tokens = mutableListOf<Token>()

    init {
        val lines = source
            .replace("\r\n", "\n")
            .replace('\r', '\n')
            .split('\n')
            .map { it.substringBefore(';').trim() } // remove comments

        for ((i, line) in lines.withIndex()) {
            var rest = line
            while (rest.isNotEmpty()) {
                val token = matchToken(rest, i + 1)
                if (token != null) {
                    tokens += token
                    rest = rest.substring(token.text.length)
                        .trimStart(' ', '\t', '\r', '\n', ':')
                } else {
                    // Unknown char → consume 1 to avoid infinite loop
                    tokens += Token(Token.Kind.UNKNOWN, rest[0].toString(), i + 1)
                    rest = rest.drop(1)
                }
            }
        }
    }

    private fun matchToken(input: String, line: Int): Token? {
        fun match(p: Pattern, kind: Token.Kind): Token? {
            val m = p.matcher(input)
            return if (m.find()) Token(kind, m.group(), line) else null
        }

        when {
            input.startsWith(",") -> return Token(Token.Kind.COMMA, ",", line)
            input.startsWith("[") -> return Token(Token.Kind.LBRACKET, "[", line)
            input.startsWith("]") -> return Token(Token.Kind.RBRACKET, "]", line)
            input.startsWith("+") -> return Token(Token.Kind.PLUS, "+", line)
            input.startsWith("-") -> return Token(Token.Kind.MINUS, "-", line)
        }

        match(segmentPattern, Token.Kind.SEGMENT)?.let { return it }
        match(dataDirectivePattern, Token.Kind.DATA_DIR)?.let { return it }
        match(labelPattern, Token.Kind.LABEL)?.let {
            return Token(Token.Kind.LABEL, it.text.substringBefore(':').trim(), line)
        }
        match(registerPattern, Token.Kind.REGISTER)?.let { return it }
        match(hexPattern, Token.Kind.NUMBER_HEX)?.let { return it }
        match(binPattern, Token.Kind.NUMBER_BIN)?.let { return it }
        match(octPattern, Token.Kind.NUMBER_OCT)?.let { return it }
        match(decPattern, Token.Kind.NUMBER_DEC)?.let { return it }
        match(stringPattern, Token.Kind.STRING)?.let { return it }
        match(identifierPattern, Token.Kind.IDENTIFIER)?.let {
            if (operations.contains(it.text.lowercase()))
                return Token(Token.Kind.OPERATION, it.text, line)
            return it
        }

        return null
    }

    private val tokensIterator = tokens.iterator()

    /**
     * Returns the next token from the input source code.
     *
     * @return The next token, or throws an exception if there are no more tokens.
     * @throws NoSuchElementException if there are no more tokens.
     */
    fun nextToken(): Token = tokensIterator.next()

    /**
     * Checks if there are more tokens to be processed.
     *
     * @return `true` if there are more tokens, `false` otherwise.
     */
    fun hasToken(): Boolean = tokensIterator.hasNext()
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

    println("Tokens:")
    while (lexer.hasToken()) {
        println(lexer.nextToken())
    }
}