SELECT 4
SAVE SCRE TO WSCRE99
store .t. to viendo
do while viendo
   RESTORE SCRE FROM WSCRE99
   STORE SPACE(3)      TO XINGGRU
   STORE SPACE(6)      TO XINGSUB
   STORE 0             TO WTOTAL
   STORE 100           TO WLINEA
   STORE 0             TO WPAGINA

   @ 04,0 clear to 08,60
   @ 04,0 to 08,60
   @ 04,02 SAY "TIPOS DE INGRESOS"
   @ 05,01 say "GRUPO       :"
   @ 06,01 say "SUBGRUPO    :"
   @ 07,01 say "SALIDA (M/I):"

   @ 05,14 GET XINGGRU
   READ
   IF READKEY()=12 .OR. READKEY()=268
      EXIT
   ENDIF
   IF XINGGRU <> SPACE(3)
      STORE XINGGRU+"000000" TO XCLAVEGRU
      SELECT 4
      FIND &XCLAVEGRU
      IF EOF()
         STORE "NO REG.COMO TIPO DE ING." TO XINGGRUDES
      ELSE
         STORE DESCRI                     TO XINGGRUDES
      ENDIF
      @ 05,30 SAY XINGGRUDES
      @ 06,14 GET XINGSUB
      READ
      IF READKEY()=12 .OR. READKEY()=268
         LOOP
      ENDIF
      IF XINGSUB <> SPACE(6)
         STORE XINGGRU+XINGSUB  TO XCLAVESUB
         SELECT 4
         FIND &XCLAVESUB
         IF EOF()
            STORE "NO REG.COMO SUBTIPO DE ING." TO XINGSUBDES
         ELSE
            STORE DESCRI                        TO XINGSUBDES
         ENDIF
         @ 06,30 SAY XINGSUBDES
      ELSE
         STORE "TODOS LOS SUBGRUPOS" TO XINGSUBDES
         @ 06,30 SAY XINGSUBDES
      ENDIF
   ELSE
      STORE "TODOS LOS GRUPOS"    TO XINGGRUDES
      STORE "TODOS LOS SUBGRUPOS" TO XINGSUBDES
      @ 05,30 SAY XINGGRUDES
      @ 06,30 SAY XINGSUBDES
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
   @ 07,14 SAY WSALIDA
   @ 07,30 SAY WSALIDES

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

   SELECT 4
   GO TOP
   DO WHILE .NOT. EOF()
      *** FILTROS
      IF XINGGRU  <> SPACE(3) .AND. XINGGRU <> GRUPO
         SELECT 4
         SKIP
         LOOP
      ENDIF
      IF XINGSUB  <> SPACE(6) .AND. XINGSUB <> SUBGRUPO
         SELECT 4
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
         @ 1,0 SAY "LISTADO DE TIPOS DE INGRESOS"
         @ 1,60 SAY "FECHA :"+DTOC(DATE())
         @ 2,00 SAY "GRUPO   :"+XINGGRUDES
         @ 3,00 SAY "SUBGRUPO:"+XINGSUBDES
         @ 5,00 SAY "GRUPO  "
         @ 5,08 SAY "SUBGRU."
         @ 5,25 SAY "DESCRIPCION"
         @ 6,00 SAY "-------"
         @ 6,08 SAY "-------"
         @ 6,25 SAY "------------------------------"
         STORE 7 TO WLINEA
      ENDIF
      IF GRUPO <> "000"
         @ WLINEA , 00 SAY GRUPO
      ENDIF
      IF SUBGRUPO <> "000000"
         @ WLINEA , 08 SAY SUBGRUPO
      ENDIF
      @ WLINEA , 25 SAY DESCRI
      SELECT 4
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