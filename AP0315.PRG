SAVE SCRE TO SCRE4
store .t. to wsub4
do while wsub4
   RESTORE SCREEN FROM SCRE4
   @ 04,37 clear to 07,60
   @ 04,37       to 07,60
   @ 05,38 PROMPT "SOBRES DE PAGO        "
   @ 06,38 PROMPT "CONCEPTO              "
   MENU TO WSUBSUB4
   DO CASE
   CASE WSUBSUB4 = 1
        HIDE MENU MENUAP
        DO AP0315A
        SHOW MENU MENUAP
   CASE WSUBSUB4 = 2
        HIDE MENU MENUAP
        DO AP0315B
        SHOW MENU MENUAP
   CASE WSUBSUB4 = 0
        EXIT
   ENDCASE
enddo
