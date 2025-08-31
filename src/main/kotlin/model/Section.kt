package model

/**
 * Represents the different sections of memory in the simulated computer.
 * Each section has a specific purpose and may have a defined size.
 *
 * @property DATA The data section, used for storing program data.
 * @property CODE The code section, used for storing program instructions.
 * @property STACK The stack section, used for managing function calls and local variables.
 */
enum class Section {
    /**
     * The data section of memory.
     * This section is typically used to store global and static variables.
     */
    DATA,

    /**
     * The code section of memory, also known as the text section.
     * This section stores the executable instructions of a program.
     */
    CODE,

    /**
     * The stack section of memory.
     * This section is used for storing temporary data such as function arguments, local variables, and return addresses.
     */
    STACK
}