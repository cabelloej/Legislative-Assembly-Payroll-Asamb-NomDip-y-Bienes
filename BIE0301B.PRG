select 2
SAVE SCRE TO WSCRE99
store .t. to viendo
do while viendo
   RESTORE SCRE FROM WSCRE99
   STORE SPACE(3)      TO XADMGRU
   STORE SPACE(3)      TO XADMSUB
   STORE SPACE(3)      TO XADMREN
   STORE 0             TO WTOTAL
   STORE 100           TO WLINEA
   STORE 0             TO WPAGINA

   @ 04,0 clear to 09,60
   @ 04,0 to 09,60
   @ 04,02 SAY "UBICACIONES ADMINISTRATIVAS"
   @ 05,01 say "GRUPO       :"
   @ 06,01 say "SUBGRUPO    :"
   @ 07,01 say "RENGLON     :"
   @ 08,01 say "SALIDA (M/I):"

   @ 05,14 GET XADMGRU
   READ
   IF READKEY()=12 .OR. READKEY()=268
      EXIT
   ENDIF
   IF XADMGRU <> SPACE(3)
      STORE XADMGRU+"000000" TO XCLAVEGRU
      SELECT 2
      FIND &XCLAVEGRU
      IF EOF()
         STORE "NO REG.COMO GRUPO ADM." TO XADMGRUDES
      ELSE
         STORE DESCRI                   TO XADMGRUDES
      ENDIF
      @ 05,30 SAY XADMGRUDES
      @ 06,14 GET XADMSUB
      READ
      IF READKEY()=12 .OR. READKEY()=268
         LOOP
      ENDIF
      IF XADMSUB <> SPACE(3)
         STORE XADMGRU+XADMSUB+"000"  TO XCLAVESUB
         SELECT 2
         FIND &XCLAVESUB
         IF EOF()
            STORE "NO REG.COMO SG. ADM." TO XADMSUBDES
         ELSE
            STORE DESCRI                  TO XADMSUBDES
         ENDIF
         @ 06,30 SAY XADMSUBDES
         @ 07,14 GET XADMREN
         READ
         IF READKEY()=12 .OR. READKEY()=268
            LOOP
         ENDIF
         IF XADMREN <> SPACE(3)
            STORE XADMGRU+XADMSUB+XADMREN TO XCLAVEREN
            SELECT 2
            FIND &XCLAVEREN
            IF EOF()
               STORE "NO REG.COMO RG. ADM." TO XADMRENDES
            ELSE
               STORE DESCRI                 TO XADMRENDES
            ENDIF
         ELSE
            STORE "TODOS LOS RENGLONES"     TO XADMRENDES
         ENDIF
         @ 07,30 SAY XADMRENDES
      ELSE
         STORE "TODOS LOS SUBGRUPOS" TO XADMSUBDES
         STORE "TODOS LOS RENGLONES" TO XADMRENDES
         @ 06,30 SAY XADMSUBDES
         @ 07,30 SAY XADMRENDES
      ENDIF
   ELSE
      STORE "TODOS LOS GRUPOS"    TO XADMGRUDES
      STORE "TODOS LOS SUBGRUPOS" TO XADMSUBDES
      STORE "TODOS LOS RENGLONES" TO XADMRENDES
      @ 05,30 SAY XADMGRUDES
      @ 06,30 SAY XADMSUBDES
      @ 07,30 SAY XADMRENDES
   ENDIF
   STORE "SELECCIONE LA SALIDA: (M)ONITOR, (I)MPRESORA" TO TEX
   STORE "MI" TO WCH
   DO PREGUNTA
   IF READKEY()=12 .OR. READKEY()=268
      LOOP
   ENDIF
   STORE WCH TO WSALIDA
   STORE 0   TO WPAGINA
   STORE 100 TO WLINEA
   IF WSALIDA = "I"
      STORE 55 TO WSALTO
      STORE "IMPRESORA" TO WSALIDES
   ELSE
      STORE 22 TO WSALTO
      STORE "MONITOR" TO WSALIDES
   ENDIF
   @ 08,14 SAY WSALIDA
   @ 08,30 SAY WSALIDES

   STORE "OPCIONES: (C)ONTINUAR, (S)ALIR" TO TEX
   STORE "CS" TO WCH
   DO PREGUNTA
   IF WCH = "S"
      exit
   ENDIF
   IF WSALIDA = "I"
      SET DEVI TO PRINT
   ELSE
      SET DEVI TO SCRE
   ENDIF

   SELECT 2
   GO TOP
   DO WHILE .NOT. EOF()
      *** FILTROS
      IF XADMGRU  <> SPACE(3) .AND. XADMGRU <> GRUPO
         SELECT 2
         SKIP
         LOOP
      ENDIF
      IF XADMSUB  <> SPACE(3) .AND. XADMSUB <> SUBGRUPO
         SELECT 2
         SKIP
         LOOP
      ENDIF
      IF XADMREN  <> SPACE(3) .AND. XADMREN <> RENGLON
         SELECT 2
         SKIP
         LOOP
      ENDIF
      *** FIN FILTROS
      STORE WLINEA+1 TO WLINEA
      IF WLINEA >=WSALTO
         STORE WPAGINA + 1 TO WPAGINA
         IF WSALIDA = "M"
            if WPAGINA <> 1
               STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO MES
               DO AVISO WITH MES
               IF READKEY()=12 .OR. READKEY()=268
                  EXIT
               ENDIF
            endif
            @ 0,0 clear
         ENDIF
         IF WSALIDA = "M"
            @ 0,0 SAY QQWW
         ELSE
            @ 0,0 SAY CHR(14)+QQWW
         ENDIF
         @ 0,60 SAY "PAGINA:"+STR(WPAGINA,4)
         @ 1,0 SAY "LISTADO DE UBICACIONES ADMINISTRATIVAS"
         @ 1,60 SAY "FECHA :"+DTOC(DATE())
         @ 2,00 SAY "GRUPO   :"+XADMGRUDES
         @ 3,00 SAY "SUBGRUPO:"+XADMSUBDES
         @ 4,00 SAY "RENGLON :"+XADMRENDES
         @ 5,00 SAY "GRUPO  "
         @ 5,08 SAY "SUBGRU."
         @ 5,16 SAY "RENGLON"
         @ 5,25 SAY "DESCRIPCION"
         @ 6,00 SAY "-------"
         @ 6,08 SAY "-------"
         @ 6,16 SAY "-------"
         @ 6,25 SAY "------------------------------"
         STORE 7 TO WLINEA
      ENDIF
      IF GRUPO <> "000"
         @ WLINEA , 00 SAY GRUPO
      ENDIF
      IF SUBGRUPO <> "000"
         @ WLINEA , 08 SAY SUBGRUPO
      ENDIF
      IF RENGLON <> "000"
         @ WLINEA , 16 SAY RENGLON
      ENDIF
      @ WLINEA , 25 SAY DESCRI
      SELECT 2
      SKIP
   ENDDO
   IF WSALIDA = "M"
      STORE "OPRIMA <ENTER> PARA FINALIZAR" TO MES
      DO AVISO WITH MES
   ELSE
      EJECT
      SET DEVI TO SCRE
   ENDIF
ENDDO
return

