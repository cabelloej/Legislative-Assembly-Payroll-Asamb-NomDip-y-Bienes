SAVE SCRE TO WSCRE99
store .t. to viendo
do while viendo
   STORE .F. TO WFLAGEXIT
   RESTORE SCRE FROM WSCRE99
   STORE SPACE(3)      TO XINGGRU
   STORE SPACE(6)      TO XINGSUB
   STORE SPACE(3)      TO XINGTIPREF
   STORE SPACE(12)     TO XINGNUMREF
   STORE SPACE(12)     TO XCODIGO
   STORE CTOD("01-01-"+   STR( YEAR(DATE()) )) TO XDESDE
   STORE CTOD("31-12-"+   STR( YEAR(DATE()) )) TO XHASTA
   STORE 0             TO WTOTAL
   STORE 100           TO WLINEA
   STORE 0             TO WPAGINA

   @ 04,0 clear to 12,70
   @ 04,0 to 12,70
   @ 04,02 SAY "INGRESOS"
   @ 05,01 say "GR.DEL INGR.:"
   @ 06,01 say "SG.DEL INGR.:"
   @ 07,01 say "TIPO DE REF.:"
   @ 08,01 say "No.  DE REF.:"
   @ 09,01 say "FECHA ING.>=:"
   @ 10,01 say "FECHA ING.<=:"
   @ 11,01 say "SALIDA (M/I):"

   @ 05,14 GET XINGGRU
   READ
   IF READKEY()=12 .OR. READKEY()=268
      SELECT 7
      SET ORDER TO 1
      EXIT
   ENDIF
   IF XINGGRU <> SPACE(3)
      STORE XINGGRU+"000000" TO XCLAVEGRU
      SELECT 4
      FIND &XCLAVEGRU
      IF EOF()
         STORE "NO REG. COMO GR.DE INGRESO" TO XINGGRUDES
      ELSE
         STORE DESCRI                       TO XINGGRUDES
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
            STORE "NO REG. COMO SG.DE INGRESO" TO XINGSUBDES
         ELSE
            STORE DESCRI                       TO XINGSUBDES
         ENDIF
      ELSE
         STORE "TODOS LOS SUBGRUPOS" TO XINGSUBDES
      ENDIF
      @ 06,30 SAY XINGSUBDES
   ELSE
      STORE "TODOS LOS GRUPOS"    TO XINGGRUDES
      STORE "TODOS LOS SUBGRUPOS" TO XINGSUBDES
      @ 05,30 SAY XINGGRUDES
      @ 06,30 SAY XINGSUBDES
   ENDIF

   @ 07,14 GET XINGTIPREF
   READ
   IF READKEY()=12 .OR. READKEY()=268
      LOOP
   ENDIF
   IF XINGTIPREF <> SPACE(3)
      SELECT 6
      FIND &XINGTIPREF
      IF EOF()
         STORE "NO REG.COMO TIPO DE REF." TO XTIPREFDES
      ELSE
         STORE DESCRI                     TO XTIPREFDES
      ENDIF
      @ 07,30 SAY XTIPREFDES
      @ 08,14 GET XINGNUMREF
      READ
      IF READKEY()=12 .OR. READKEY()=268
         LOOP
      ENDIF
      IF XINGNUMREF <> SPACE(12)
         STORE XINGNUMREF              TO XNUMREFDES
      ELSE
         STORE "TODAS LAS "+XTIPREFDES TO XNUMREFDES
      ENDIF
      @ 08,30 SAY XNUMREFDES
   ELSE
      STORE "TODOS LOS TIPOS "    TO XTIPREFDES
      STORE "TODOS LOS NUMEROS"   TO XNUMREFDES
      @ 07,30 SAY XTIPREFDES
      @ 08,30 SAY XNUMREFDES
   ENDIF

   @ 09,14 GET XDESDE
   @ 10,14 GET XHASTA
   READ
   IF READKEY()=12 .OR. READKEY()=268
      LOOP
   ENDIF
   IF XDESDE > XHASTA
      STORE "ERROR EN FECHAS, VERIFIQUE." TO MES
      DO AVISO WITH MES
      LOOP
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
   @ 11,14 SAY WSALIDES

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

   SELECT 7
   SET ORDER TO 6
   GO TOP
   DO WHILE .NOT. EOF()
      *** FILTROS
      IF XINGGRU <> SPACE(3) .AND. XINGGRU <> INGGRU
         SELECT 7
         SKIP
         LOOP
      ENDIF
      IF XINGSUB <> SPACE(6) .AND. XINGSUB <> INGSUB
         SELECT 7
         SKIP
         LOOP
      ENDIF
      IF XINGTIPREF <> SPACE(3) .AND. XINGTIPREF <> INGTIPREF
         SELECT 7
         SKIP
         LOOP
      ENDIF
      IF XINGNUMREF <> SPACE(12) .AND. XINGNUMREF <> INGNUMREF
         SELECT 7
         SKIP
         LOOP
      ENDIF
      IF INGFECHA < XDESDE
         SELECT 7
         SKIP
         LOOP
      ENDIF
      IF INGFECHA > XHASTA
         EXIT
      ENDIF
      *** FIN FILTROS

      STORE WLINEA+1 TO WLINEA
      IF WLINEA >=WSALTO
         DO HD0302
         IF WFLAGEXIT
            EXIT
         ENDIF
      ENDIF
      SELECT 7
      @ WLINEA ,01  SAY INGFECHA
      @ WLINEA ,10  SAY INGGRU
      @ WLINEA ,14  SAY INGSUB
      @ WLINEA ,21  SAY INGTIPREF
      @ WLINEA ,25  SAY INGNUMREF
      @ WLINEA ,38  SAY CODIGO
      @ WLINEA ,51  SAY UNIDAD
      @ WLINEA ,64  SAY VALOR PICTURE "####,###,###.##"
      STORE WTOTAL + VALOR TO WTOTAL
      IF DESCRI1 <> SPACE(40)
         STORE WLINEA+1 TO WLINEA
         IF WLINEA >=WSALTO
            DO HD0302
            IF WFLAGEXIT
               EXIT
            ENDIF
         ENDIF
         @ WLINEA,38 SAY DESCRI1
      ENDIF
      IF DESCRI2 <> SPACE(40)
         STORE WLINEA+1 TO WLINEA
         IF WLINEA >=WSALTO
            DO HD0302
            IF WFLAGEXIT
               EXIT
            ENDIF
         ENDIF
         @ WLINEA,38 SAY DESCRI2
      ENDIF
      IF DESCRI3 <> SPACE(40)
         STORE WLINEA+1 TO WLINEA
         IF WLINEA >=WSALTO
            DO HD0302
            IF WFLAGEXIT
               EXIT
            ENDIF
         ENDIF
         @ WLINEA,38 SAY DESCRI3
      ENDIF
      IF DESCRI4 <> SPACE(40)
         STORE WLINEA+1 TO WLINEA
         IF WLINEA >=WSALTO
            DO HD0302
            IF WFLAGEXIT
               EXIT
            ENDIF
         ENDIF
         @ WLINEA,38 SAY DESCRI4
      ENDIF
      STORE WLINEA+1 TO WLINEA
      @ WLINEA,00 SAY REPLICATE("-",80)
      SELECT 7
      SKIP
   ENDDO
   STORE WLINEA + 1 TO WLINEA
   IF WLINEA >=WSALTO
      DO HD0302
      IF WFLAGEXIT
         EXIT
      ENDIF
   ENDIF
   @ WLINEA ,38  SAY "TOTAL VALOR REPORTADO:"
   @ WLINEA ,64  SAY WTOTAL PICTURE "####,###,###.##"
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