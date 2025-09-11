package lexical

/**
 * Represents a token in the source code.
 *
 * A token is a sequence of characters that is treated as a single unit in the grammar of the programming language.
 * For example, keywords, identifiers, operators, literals, and punctuation marks are all tokens.
 *
 * @property kind The type of the token. See [Kind] for possible values.
 * @property text The actual text of the token as it appeared in the source code.
 * @property line The line number in the source code where this token begins.
 */
data class Token(val kind: Kind, val text: String, val line: Int) {
    /**
     * Enum representing the different kinds of tokens that can be recognized by the lexer.
     */
    enum class Kind {
        /** Hexadecimal number (e.g., 0x1A) */
        NUMBER_HEX,
        /** Binary number (e.g., 0b1010) */
        NUMBER_BIN,
        /** Octal number (e.g., 0o77) */
        NUMBER_OCT,
        /** Decimal number (e.g., 123) */
        NUMBER_DEC,
        /** String literal */
        STRING,
        /** Identifier (e.g., variable names, function names) */
        IDENTIFIER,
        /** Label identifier (e.g., myLabel:) */
        LABEL,
        /** Register identifier (e.g., eax, PC) */
        REGISTER,
        /** A data directory directive (e.g., word, dw, dd) */
        DATA_DIR,
        /** Comma separator ',' */
        COMMA,
        /** Left bracket '[' */
        LBRACKET,
        /** Right bracket ']' */
        RBRACKET,
        /** Plus operator '+' */
        PLUS,
        /** Minus operator '-' */
        MINUS,
        /** Segment directive (e.g., .code, .data) */
        SEGMENT,
        /** Unknown or unrecognized token */
        UNKNOWN
    }

    override fun toString(): String = "$line: $kind $text"
}