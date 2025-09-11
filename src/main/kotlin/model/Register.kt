package model

/**
 * Represents the registers available in an x86-like architecture.
 *
 * This enum class categorizes registers by their size: 8-bit, 16-bit, and 32-bit,
 * and includes segment registers.
 */
enum class Register(val code: UByte) {
    /** Accumulator Low byte (8-bit) */
    AL(0x00u),
    /** Count Low byte (8-bit) */
    CL(0x01u),
    /** Data Low byte (8-bit) */
    DL(0x02u),
    /** Base Low byte (8-bit) */
    BL(0x03u),
    /** Accumulator High byte (8-bit) */
    AH(0x04u),
    /** Count High byte (8-bit) */
    CH(0x05u),
    /** Data High byte (8-bit) */
    DH(0x06u),
    /** Base High byte (8-bit) */
    BH(0x07u),

    /** Accumulator register (16-bit) */
    AX(0x00u),
    /** Count register (16-bit) */
    CX(0x01u),
    /** Data register (16-bit) */
    DX(0x02u),
    /** Base register (16-bit) */
    BX(0x03u),
    /** Stack Pointer register (16-bit) */
    SP(0x04u),
    /** Base Pointer register (16-bit) */
    BP(0x05u),
    /** Source Index register (16-bit) */
    SI(0x06u),
    /** Destination Index register (16-bit) */
    DI(0x07u),

    /** Extended Accumulator register (32-bit) */
    EAX(0x00u),
    /** Extended Count register (32-bit) */
    ECX(0x01u),
    /** Extended Data register (32-bit) */
    EDX(0x02u),
    /** Extended Base register (32-bit) */
    EBX(0x03u),
    /** Extended Stack Pointer register (32-bit) */
    ESP(0x04u),
    /** Extended Base Pointer register (32-bit) */
    EBP(0x05u),
    /** Extended Source Index register (32-bit) */
    ESI(0x06u),
    /** Extended Destination Index register (32-bit) */
    EDI(0x07u),

    // Segment registers (used in MOV Sreg instructions)
    /** Extra Segment register (16-bit) */
    ES(0x00u),
    /** Code Segment register (16-bit) */
    CS(0x01u),
    /** Stack Segment register (16-bit) */
    SS(0x02u),
    /** Data Segment register (16-bit) */
    DS(0x03u),
    /** F Segment register (16-bit) */
    FS(0x04u),
    /** G Segment register (16-bit) */
    GS(0x05u);
}