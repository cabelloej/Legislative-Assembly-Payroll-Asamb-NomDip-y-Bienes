select 1
SAVE SCRE TO WSCRE99
store .t. to viendo
do while viendo
   RESTORE SCRE FROM WSCRE99
   STORE SPACE(3)      TO XTIPGRU
   STORE SPACE(3)      TO XTIPSUB
   STORE SPACE(3)      TO XTIPREN
   STORE 0             TO WTOTAL
   STORE 100           TO WLINEA
   STORE 0             TO WPAGINA

   @ 04,0 clear to 09,60
   @ 04,0 to 09,60
   @ 04,02 SAY "TIPOS DE BIENES"
   @ 05,01 say "GRUPO       :"
   @ 06,01 say "SUBGRUPO    :"
   @ 07,01 say "RENGLON     :"
   @ 08,01 say "SALIDA (M/I):"

   @ 05,14 GET XTIPGRU
   READ
   IF READKEY()=12 .OR. READKEY()=268
      EXIT
   ENDIF
   IF XTIPGRU <> SPACE(3)
      STORE XTIPGRU+"000000" TO XCLAVEGRU
      SELECT 1
      FIND &XCLAVEGRU
      IF EOF()
         STORE "NO REG.COMO GR.DE TIPO" TO XTIPGRUDES
      ELSE
         STORE DESCRI                    TO XTIPGRUDES
      ENDIF
      @ 05,30 SAY XTIPGRUDES
      @ 06,14 GET XTIPSUB
      READ
      IF READKEY()=12 .OR. READKEY()=268
         LOOP
      ENDIF
      IF XTIPSUB <> SPACE(3)
         STORE XTIPGRU+XTIPSUB+"000"  TO XCLAVESUB
         SELECT 1
         FIND &XCLAVESUB
         IF EOF()
            STORE "NO REG.COMO SG.DE TIPO" TO XTIPSUBDES
         ELSE
            STORE DESCRI                   TO XTIPSUBDES
         ENDIF
         @ 06,30 SAY XTIPSUBDES
         @ 07,14 GET XTIPREN
         READ
         IF READKEY()=12 .OR. READKEY()=268
            LOOP
         ENDIF
         IF XTIPREN <> SPACE(3)
            STORE XTIPGRU+XTIPSUB+XTIPREN TO XCLAVEREN
            SELECT 1
            FIND &XCLAVEREN
            IF EOF()
               STORE "NO REG.COMO RG.DE TIPO" TO XTIPRENDES
            ELSE
               STORE DESCRI                   TO XTIPRENDES
            ENDIF
         ELSE
            STORE "TODOS LOS RENGLONES"       TO XTIPRENDES
         ENDIF
         @ 07,30 SAY XTIPRENDES
      ELSE
         STORE "TODOS LOS SUBGRUPOS" TO XTIPSUBDES
         STORE "TODOS LOS RENGLONES" TO XTIPRENDES
         @ 06,30 SAY XTIPSUBDES
         @ 07,30 SAY XTIPRENDES
      ENDIF
   ELSE
      STORE "TODOS LOS GRUPOS"    TO XTIPGRUDES
      STORE "TODOS LOS SUBGRUPOS" TO XTIPSUBDES
      STORE "TODOS LOS RENGLONES" TO XTIPRENDES
      @ 05,30 SAY XTIPGRUDES
      @ 06,30 SAY XTIPSUBDES
      @ 07,30 SAY XTIPRENDES
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

   SELECT 1
   GO TOP
   DO WHILE .NOT. EOF()
      *** FILTROS
      IF XTIPGRU  <> SPACE(3) .AND. XTIPGRU <> GRUPO
         SELECT 1
         SKIP
         LOOP
      ENDIF
      IF XTIPSUB  <> SPACE(3) .AND. XTIPSUB <> SUBGRUPO
         SELECT 1
         SKIP
         LOOP
      ENDIF
      IF XTIPREN  <> SPACE(3) .AND. XTIPREN <> RENGLON
         SELECT 1
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
         @ 1,0 SAY "LISTADO DE TIPOS DE BIENES"
         @ 1,60 SAY "FECHA :"+DTOC(DATE())
         @ 2,00 SAY "GRUPO   :"+XTIPGRUDES
         @ 3,00 SAY "SUBGRUPO:"+XTIPSUBDES
         @ 4,00 SAY "RENGLON :"+XTIPRENDES
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
      SELECT 1
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