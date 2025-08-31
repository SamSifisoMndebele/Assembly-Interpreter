package model

/**
 * Represents the registers available in an x86-like architecture.
 *
 * This enum class categorizes registers by their size: 8-bit, 16-bit, and 32-bit.
 */
enum class Reg {
    /** Accumulator Low byte (8-bit) */
    AL,
    /** Accumulator High byte (8-bit) */
    AH,
    /** Base Low byte (8-bit) */
    BL,
    /** Base High byte (8-bit) */
    BH,
    /** Count Low byte (8-bit) */
    CL,
    /** Count High byte (8-bit) */
    CH,
    /** Data Low byte (8-bit) */
    DL,
    /** Data High byte (8-bit) */
    DH,

    /** Accumulator register (16-bit) */
    AX,
    /** Base register (16-bit) */
    BX,
    /** Count register (16-bit) */
    CX,
    /** Data register (16-bit) */
    DX,
    /** Source Index register (16-bit) */
    SI,
    /** Destination Index register (16-bit) */
    DI,
    /** Base Pointer register (16-bit) */
    BP,
    /** Stack Pointer register (16-bit) */
    SP,

    /** Extended Accumulator register (32-bit) */
    EAX,
    /** Extended Base register (32-bit) */
    EBX,
    /** Extended Count register (32-bit) */
    ECX,
    /** Extended Data register (32-bit) */
    EDX,
    /** Extended Source Index register (32-bit) */
    ESI,
    /** Extended Destination Index register (32-bit) */
    EDI,
    /** Extended Base Pointer register (32-bit) */
    EBP,
    /** Extended Stack Pointer register (32-bit) */
    ESP
}