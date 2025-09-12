package lexical

import model.Operation.Companion.allOperations
import java.util.regex.Pattern
import kotlin.NoSuchElementException

/**
 * The Lexer class is responsible for tokenizing the input source code.
 * It takes a string of source code as input and produces a stream of tokens.
 *
 * The lexer works by iterating through the source code line by line,
 * and for each line, it tries to match various patterns to identify tokens.
 * Comments (lines starting with ';') are ignored.
 *
 * The supported token types include:
 * - Numbers (hexadecimal, binary, octal, decimal)
 * - Identifiers
 * - Labels
 * - Registers
 * - Segments (code, data, stack)
 * - Strings
 * - Data directives (byte, word, dword, etc.)
 * - Operations (instructions)
 * - Punctuation (comma, brackets, plus, minus)
 *
 * If an unrecognized character is encountered, it is treated as an UNKNOWN token.
 *
 * @property source The input source code string.
 */
abstract class Lexer(source: String) {
    companion object {
        // Regex patterns
        private val hexPattern = Pattern.compile("^(?:0x[0-9a-f]+|[0-9][0-9a-f]*h)", Pattern.CASE_INSENSITIVE)
        private val binPattern = Pattern.compile("^(?:0b[01]+|[01]+b)", Pattern.CASE_INSENSITIVE)
        private val octPattern = Pattern.compile("^(?:0[qo][0-7]+|[0-7]+[qo])", Pattern.CASE_INSENSITIVE)
        private val decPattern = Pattern.compile("^[-+]?[0-9]+(?:_[0-9]+)*d?", Pattern.CASE_INSENSITIVE)
        private val identifierPattern = Pattern.compile("^[A-Za-z_][A-Za-z0-9_]*")
        private val labelPattern = Pattern.compile("^[A-Za-z_][A-Za-z0-9_]*\\s*:")
        private val registerPattern = Pattern.compile("^(?i)(e?[abcd]x|[abcd][lh]|[sd]i|[sb]p|[cdefgs]s)\\b", Pattern.CASE_INSENSITIVE)
        private val segmentPattern = Pattern.compile("^(?:section\\s+)?\\.(code|data|stack)", Pattern.CASE_INSENSITIVE)
        private val stringPattern = Pattern.compile("^\"([^\"\\\\]|\\\\.)*\"|^'([^'\\\\]|\\\\.)*'")
        private val dataDirectivePattern = Pattern.compile("^(byte|word|dword|db|dw|dd|dq|dt)", Pattern.CASE_INSENSITIVE)

        private val operations = allOperations.map { it.toString().lowercase() }
    }

    private val tokens = mutableListOf<Token>()
    private val labels = mutableSetOf<String>()

    init {
        val lines = source
            .replace("\r\n", "\n")
            .replace('\r', '\n')
            .split('\n')
            .map { it.substringBefore(';').trim() } // remove comments

        for ((i, line) in lines.withIndex()) {
            var rest = line
            var currentLineNumber = i + 1
            while (rest.isNotEmpty()) {
                val token = matchToken(rest, currentLineNumber)
                if (token != null) {
                    tokens += token
                    if (token.kind == Token.Kind.LABEL) {
                        labels.add(token.text)
                    }
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
            input.startsWith("*") -> return Token(Token.Kind.MULT, "*", line)
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
            if (operations.contains(it.text.lowercase())) {
                return Token(Token.Kind.OPERATION, it.text, line)
            }
            if (labels.contains(it.text)) {
                return Token(Token.Kind.LABEL, it.text, line)
            }
            return it
        }

        return null
    }

    private var index = -1

    /**
     * Checks if there are more tokens to be processed.
     *
     * @return `true` if there are more tokens, `false` otherwise.
     */
    fun hasToken(): Boolean = index < tokens.size - 1

    /**
     * Returns the next token from the input source code.
     *
     * @return The next token, or throws an exception if there are no more tokens.
     * @throws NoSuchElementException if there are no more tokens.
     */
    fun nextToken(): Token {
        if (index >= tokens.size) throw NoSuchElementException()
        return tokens[++index]
    }

    /**
     * Peeks at the next token in the stream without consuming it.
     * This function allows you to look ahead at the next token that would be returned by [nextToken],
     * without advancing the iterator.
     *
     * @return The next token in the stream.
     * @throws NoSuchElementException if there are no more tokens.
     */
    fun peekToken(): Token {
        if (index >= tokens.size) throw NoSuchElementException()
        return tokens[index + 1]
    }

    /**
     * Returns the previous token from the input source code.
     * This function is useful when you need to look back at the previously processed token.
     *
     * @return The previous token.
     * @throws NoSuchElementException if there is no previous token (e.g., at the beginning of the token stream).
     */
    fun previousToken(): Token {
        if (index <= 0) throw NoSuchElementException()
        return tokens[--index]
    }

    /**
     * Checks if there is a previous token.
     *
     * This function is useful when iterating backwards or when needing to look behind the current token.
     *
     * @return `true` if there is a previous token, `false` otherwise.
     */
    fun hasPrevious(): Boolean = index > 0

    /**
     * Returns a list of all tokens generated from the input source code.
     *
     * @return A list of [Token] objects.
     */
    fun getTokens(): List<Token> = tokens
}
