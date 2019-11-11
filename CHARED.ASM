W@CHARED SEGMENT BYTE 'CODE'
                ASSUME  CS:W@CHARED
                PUBLIC  CHARED
CHARED          PROC    FAR
;
;Set DS:SI to point to first argument
;
                PUSH    BP
                MOV     BP,SP
                LDS     SI,[BP+6]
;
;This initializes AH to 6H and DL to 0FFh.
;
                MOV     AH,006H
                MOV     DL,0FFH
;
;This call interrupt 21h to read the character, if one is available
;
                INT     21H
                MOV     [SI],AL
                POP     BP
                RET
CHARED          ENDP
W@CHARED ENDS
                END
