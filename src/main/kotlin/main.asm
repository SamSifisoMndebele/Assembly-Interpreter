.data
x DWORD 1234ABCDh
y WORD 7,5,8,10
name BYTE "Sam", 0
char BYTE 'A'
sum DWORD ?
count WORD 5h

.code
    push eax
    push 25h
    mov eax, [x + ebx + esi*2 + 16]
    ; Compute 5 + 7
    MOV AX, 5
    ;ADD AX, 7
    ;pop eax
    ;mov dx, [y+2]

    ; countdown from 3 to 0
    ;mov CX, 3
;loop_start:
;    DEC CX
;    CMP CX, 0
;    JG loop_start ; jump if greater than 0