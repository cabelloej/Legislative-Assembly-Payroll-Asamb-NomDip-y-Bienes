SELECT 3
SAVE SCRE TO WSCRE99
store .t. to viendo
do while viendo
   RESTORE SCRE FROM WSCRE99
   STORE SPACE(3)      TO XGEOGRU
   STORE SPACE(3)      TO XGEOSUB
   STORE SPACE(3)      TO XGEOREN
   STORE 0             TO WTOTAL
   STORE 100           TO WLINEA
   STORE 0             TO WPAGINA

   @ 04,0 clear to 09,60
   @ 04,0 to 09,60
   @ 04,02 SAY "UBICACIONES GEOGRAFICAS"
   @ 05,01 say "GRUPO       :"
   @ 06,01 say "SUBGRUPO    :"
   @ 07,01 say "RENGLON     :"
   @ 08,01 say "SALIDA (M/I):"

   @ 05,14 GET XGEOGRU
   READ
   IF READKEY()=12 .OR. READKEY()=268
      EXIT
   ENDIF
   IF XGEOGRU <> SPACE(3)
      STORE XGEOGRU+"000000" TO XCLAVEGRU
      SELECT 3
      FIND &XCLAVEGRU
      IF EOF()
         STORE "NO REG.COMO GRUPO GEO." TO XGEOGRUDES
      ELSE
         STORE DESCRI                   TO XGEOGRUDES
      ENDIF
      @ 05,30 SAY XGEOGRUDES
      @ 06,14 GET XGEOSUB
      READ
      IF READKEY()=12 .OR. READKEY()=268
         LOOP
      ENDIF
      IF XGEOSUB <> SPACE(3)
         STORE XGEOGRU+XGEOSUB+"000"  TO XCLAVESUB
         SELECT 3
         FIND &XCLAVESUB
         IF EOF()
            STORE "NO REG.COMO SG. GEO." TO XGEOSUBDES
         ELSE
            STORE DESCRI                 TO XGEOSUBDES
         ENDIF
         @ 06,30 SAY XGEOSUBDES
         @ 07,14 GET XGEOREN
         READ
         IF READKEY()=12 .OR. READKEY()=268
            LOOP
         ENDIF
         IF XGEOREN <> SPACE(3)
            STORE XGEOGRU+XGEOSUB+XGEOREN TO XCLAVEREN
            SELECT 3
            FIND &XCLAVEREN
            IF EOF()
               STORE "NO REG.COMO RG. GEO." TO XGEORENDES
            ELSE
               STORE DESCRI                 TO XGEORENDES
            ENDIF
         ELSE
            STORE "TODOS LOS RENGLONES"     TO XGEORENDES
         ENDIF
         @ 07,30 SAY XGEORENDES
      ELSE
         STORE "TODOS LOS SUBGRUPOS" TO XGEOSUBDES
         STORE "TODOS LOS RENGLONES" TO XGEORENDES
         @ 06,30 SAY XGEOSUBDES
         @ 07,30 SAY XGEORENDES
      ENDIF
   ELSE
      STORE "TODOS LOS GRUPOS"    TO XGEOGRUDES
      STORE "TODOS LOS SUBGRUPOS" TO XGEOSUBDES
      STORE "TODOS LOS RENGLONES" TO XGEORENDES
      @ 05,30 SAY XGEOGRUDES
      @ 06,30 SAY XGEOSUBDES
      @ 07,30 SAY XGEORENDES
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

   SELECT 3
   GO TOP
   DO WHILE .NOT. EOF()
      *** FILTROS
      IF XGEOGRU  <> SPACE(3) .AND. XGEOGRU <> GRUPO
         SELECT 3
         SKIP
         LOOP
      ENDIF
      IF XGEOSUB  <> SPACE(3) .AND. XGEOSUB <> SUBGRUPO
         SELECT 3
         SKIP
         LOOP
      ENDIF
      IF XGEOREN  <> SPACE(3) .AND. XGEOREN <> RENGLON
         SELECT 3
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
         @ 1,0 SAY "LISTADO DE UBICACIONES GEOGRAFICAS"
         @ 1,60 SAY "FECHA :"+DTOC(DATE())
         @ 2,00 SAY "GRUPO   :"+XGEOGRUDES
         @ 3,00 SAY "SUBGRUPO:"+XGEOSUBDES
         @ 4,00 SAY "RENGLON :"+XGEORENDES
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
      SELECT 3
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