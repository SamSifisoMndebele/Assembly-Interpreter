.data
x DWORD 1234ABCDh
y DWORD 7
sum DWORD ?

.code
    mov ax, x
    push ax
    ; Compute 5 + 7, print it, store to [1000], then simple loop
    MOV AX, 5
    ADD AX, 7
    MOV BX, AX
    pop ax

    ; countdown from 3 to 0
    MOV CX, 3
loop_start:
    DEC CX
    CMP CX, 0
    JG loop_start ; jump if greater than 0