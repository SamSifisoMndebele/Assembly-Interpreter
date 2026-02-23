package model

/**
 * Represents the different segments of memory in the simulated computer.
 * Each segment has a specific purpose and may have a defined size.
 *
 * @property DATA The data segment, used for storing program data.
 * @property CODE The code segment, used for storing program instructions.
 * @property STACK The stack segment, used for managing function calls and local variables.
 */
enum class Segment {
    /**
     * The data segment of memory.
     * This segment is typically used to store global and static variables.
     */
    DATA,

    /**
     * The code segment of memory, also known as the text segment.
     * This segment stores the executable instructions of a program.
     */
    CODE,

    /**
     * The stack segment of memory.
     * This segment is used for storing temporary data such as function arguments, local variables, and return addresses.
     */
    STACK
}