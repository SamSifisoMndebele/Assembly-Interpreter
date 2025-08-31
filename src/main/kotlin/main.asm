
;.data
;x DW 5
;y DW 7
;sum DW ?

;.code
    ; Compute 5 + 7, print it, store to [1000], then simple loop
    MOV AX, 5
    ADD AX, 7
    MOV BX, 10
    MOV [1000], AX

    ; countdown from 3 to 0
    MOV CX, 3
loop_start:
    DEC CX
    CMP CX, 0
    JG loop_start ; jump if greater than 0