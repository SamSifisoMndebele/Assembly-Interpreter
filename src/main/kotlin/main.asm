.data
x DWORD 1234ABCDh
y WORD 0x7,5o,(8-2)*3,10
name BYTE "Sam", 0
char BYTE 'A'
sum DWORD ?
count WORD 5h

.code
start:
    push eax
    push 25h
    push x
    mov eax, [x + ebx + esi*2 + 16]
    MOV AX, 5
    mov y, 50
    ;ADD AX, [y+2]
    ;pop eax

    ; countdown from 3 to 0
    ;mov CX, 3
;loop_start:
;    DEC CX
;    CMP CX, 0
;    JG loop_start ; jump if greater than 0