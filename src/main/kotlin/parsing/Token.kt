package parsing

import java.util.regex.Pattern

/**
 * Represents a sealed class hierarchy for token types in a language parser. Each token type inherits from this base class
 * and provides its own specific implementation and regular expression pattern for matching.
 *
 * @property text The string representation of the token.
 * @property line The line number where this token was found.
 */
sealed class Token(open val text: String, open val line: Int) {
    val length: Int = text.length
    val value: String = text
    val isNumber: Boolean
        get() = this is HexNumber || this is BinNumber || this is OctNumber || this is RealNumber || this is DecNumber

    override fun toString(): String = String.format("%-3d: %-16s %s", line, this::class.simpleName, text)

    interface Factory {
        val pattern: Pattern
        fun create(text: String, line: Int): Token
    }

    /**
     * Represents a comma token.
     * @param line The line number where this token was found.
     */
    data class Comma(override val line: Int) : Token(",", line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^,")
            override fun create(text: String, line: Int): Token = Comma(line)
        }
    }

    /**
     * Represents a left bracket token.
     * @param line The line number where this token was found.
     */
    data class LBracket(override val line: Int) : Token("[", line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^\\[")
            override fun create(text: String, line: Int): Token = LBracket(line)
        }
    }

    /**
     * Represents a right bracket token.
     * @param line The line number where this token was found.
     */
    data class RBracket(override val line: Int) : Token("]", line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^]")
            override fun create(text: String, line: Int): Token = RBracket(line)
        }
    }

    /**
     * Represents a multiplication or asterisk token.
     * @param line The line number where this token was found.
     */
    data class Multi(override val line: Int) : Token("*", line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^\\*")
            override fun create(text: String, line: Int): Token = Multi(line)
        }
    }

    /**
     * Represents a plus sign token.
     * @param line The line number where this token was found.
     */
    data class Plus(override val line: Int) : Token("+", line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^\\+")
            override fun create(text: String, line: Int): Token = Plus(line)
        }
    }

    /**
     * Represents a minus sign or hyphen token.
     * @param line The line number where this token was found.
     */
    data class Minus(override val line: Int) : Token("-", line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^-")
            override fun create(text: String, line: Int): Token = Minus(line)
        }
    }


    /**
     * Represents a segment directive token (e.g., .data, .code).
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Segment(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^\\.(?:data\\??|const|stack|code)\\b", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = Segment(text, line)
        }
    }

    /**
     * Represents a data directive token (e.g., byte, word, dd).
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class DataDir(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^(?:byte|word|dword|db|dw|dd|dq|dt)\\b", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = DataDir(text, line)
        }
    }

    /**
     * Represents a label token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Label(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^[a-z_@?$][a-z0-9_@?$]*\\s*:", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = Label(text, line)
        }
    }

    /**
     * Represents a register token (e.g., EAX, BL, SI).
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Register(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^(?:e?[abcd]x|[abcd][lh]|e?[sd]i|e?[sb]p|[cdefgs]s)\\b", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = Register(text, line)
        }
    }

    /**
     * Represents a string literal token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Text(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^\"(?:[^\"\\\\]|\\\\.)*\"|^'(?:[^'\\\\]|\\\\.)*'")
            override fun create(text: String, line: Int): Token = Text(text, line)
        }
    }

    /**
     * Represents an identifier token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Identifier(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^[a-z_@?$][a-z0-9_@?$]*", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = Identifier(text, line)
        }
    }

    /**
     * Represents a mnemonic token (e.g., MOV, ADD).
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Mnemonic(override val text: String, override val line: Int) : Token(text, line)



    /**
     * Represents a hexadecimal number token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class HexNumber(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^(?:0x[0-9a-f]+|\\d[0-9a-f]*h)\\b", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = HexNumber(text, line)
        }
    }

    /**
     * Represents a binary number token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class BinNumber(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^(?:0b[01]+|[01]+b)\\b", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = BinNumber(text, line)
        }
    }

    /**
     * Represents an octal number token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class OctNumber(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^(?:0[oq][0-7]+|[0-7]+[oq])\\b", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = OctNumber(text, line)
        }
    }

    /**
     * Represents a real (floating-point) number token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class RealNumber(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^(?:[-+]?(?:(?:\\d+\\.\\d*|\\.\\d+)(?:E[-+]?\\d+)?|\\d+E[-+]?\\d+)(?!\\w)|\\d[0-9a-f]*r\\b)", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = RealNumber(text, line)
        }
    }

    /**
     * Represents a decimal number token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class DecNumber(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^[-+]?\\d+d?\\b(?!\\.)", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int): Token = DecNumber(text, line)
        }
    }

    /**
     * Represents an expression token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class Expression(override val text: String, override val line: Int) : Token(text, line) {
        companion object : Factory {
            override val pattern: Pattern = Pattern.compile("^[a-z_@?$][a-z0-9_@?$]*\\s*\\+?\\s*[a-z_@?$][a-z0-9_@?$]*", Pattern.CASE_INSENSITIVE)
            override fun create(text: String, line: Int) = Expression(text, line)
        }
    }

    /**
     * Represents an unknown or unparseable token.
     * @param text The string representation of the token.
     * @param line The line number where this token was found.
     */
    data class UNKNOWN(override val text: String, override val line: Int) : Token(text, line)
}