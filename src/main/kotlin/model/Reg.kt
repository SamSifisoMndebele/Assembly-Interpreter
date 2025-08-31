package model

/**
 * Represents the general-purpose registers available in x86 architecture.
 *
 * This enum class categorizes registers by their size: 8-bit, 16-bit, and 32-bit.
 *
 * **8-bit Registers:**
 *  - `AL`: Low byte of the AX register.
 *  - `AH`: High byte of the AX register.
 *  - `BL`: Low byte of the BX register.
 *  - `BH`: High byte of the BX register.
 *  - `CL`: Low byte of the CX register.
 *  - `CH`: High byte of the CX register.
 *  - `DL`: Low byte of the DX register.
 *  - `DH`: High byte of the DX register.
 *
 * **16-bit Registers:**
 *  - `AX`: Accumulator register (AL and AH combined).
 *  - `BX`: Base register (BL and BH combined).
 *  - `CX`: Count register (CL and CH combined).
 *  - `DX`: Data register (DL and DH combined).
 *  - `SI`: Source Index register.
 *  - `DI`: Destination Index register.
 *  - `BP`: Base Pointer register.
 *  - `SP`: Stack Pointer register.
 *
 * **32-bit Registers:**
 *  - `EAX`: Extended Accumulator register (includes AX).
 *  - `EBX`: Extended Base register (includes BX).
 *  - `ECX`: Extended Count register (includes CX).
 *  - `EDX`: Extended Data register (includes DX).
 *  - `ESI`: Extended Source Index register (includes SI).
 *  - `EDI`: Extended Destination Index register (includes DI).
 *  - `EBP`: Extended Base Pointer register (includes BP).
 *  - `ESP`: Extended Stack Pointer register (includes SP).
 */// General Purpose Registers
enum class Reg {
    // 8-bit registers
    AL, AH, BL, BH, CL, CH, DL, DH,
    // 16-bit registers
    AX, BX, CX, DX, SI, DI, BP, SP,
    // 32-bit registers
    EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP
}