STORE .T. TO WCARGANDO
DO WHILE WCARGANDO
   @ WLIN01+0,WCOL01+0 CLEAR to WLIN01+4,WCOL01+55
   @ WLIN01+0,WCOL01+0       to WLIN01+4,WCOL01+55 DOUBLE
   @ WLIN01+0,WCOL01+10 SAY " TIPOS DE BIENES "
   @ WLIN01+1,WCOL01+1  SAY "GRUPO       :"
   @ WLIN01+2,WCOL01+1  SAY "SUBGRUPO    :"
   @ WLIN01+3,WCOL01+1  SAY "RENGLON     :"
   @ WLIN01+1,WCOL01+15  GET WTIPGRU
   STORE "INGRESE EL CODIGO SOLICITADO ( *ELIMINA*=ELIMINAR CODIGO, <ESC>=SALIR)" TO MES
   DO MENSAJE WITH MES
   READ
   IF WTIPGRU=SPACE(3) .OR. WTIPGRU="000" .OR.READKEY()=12.OR.READKEY()=268
      EXIT
   ENDIF
   STORE WTIPGRU+"000"+"000" TO WCLAVE
   SELECT 1
   FIND &WCLAVE
   IF EOF()
      DO FILLOC
      APPEND BLANK
      UNLOCK
      DO RECLOC
      REPLACE GRUPO     WITH WTIPGRU
      REPLACE SUBGRUPO  WITH "000"
      REPLACE RENGLON   WITH "000"
      FLUSH
      @ WLIN01+1,WCOL01+20 GET DESCRI
      READ
      unlock
      IF READKEY()=12.OR.READKEY()=268
         LOOP
      ENDIF
   ELSE
      do recloc
      @ WLIN01+1,WCOL01+20 GET DESCRI
      READ
      unlock
      IF READKEY()=12.OR.READKEY()=268
         LOOP
      ENDIF
      IF DESCRI = "*ELIMINA*"
         STORE "ESTA SEGURO QUE DESEA ELIMINAR ? (S/N)" TO TEX
         STORE "NS" TO WCH
         DO PREGUNTA
         IF WCH = "N"
            LOOP
         ELSE
            DO FILLOC
            DELETE
            UNLOCK
            LOOP
         ENDIF
      ENDIF
      FLUSH
   ENDIF
   @ WLIN01+2,WCOL01+15 GET WTIPSUB
   READ
   IF WTIPSUB=SPACE(3) .OR. WTIPSUB="000" .OR.READKEY()=12.OR.READKEY()=268
      LOOP
   ENDIF
   STORE WTIPGRU+WTIPSUB+"000" TO WCLAVE
   SELECT 1
   FIND &WCLAVE
   IF EOF()
      DO FILLOC
      APPEND BLANK
      UNLOCK
      DO RECLOC
      REPLACE GRUPO    WITH WTIPGRU
      REPLACE SUBGRUPO WITH WTIPSUB
      REPLACE RENGLON  WITH "000"
      FLUSH
      @ WLIN01+2,WCOL01+20 GET DESCRI
      READ
      unlock
      IF READKEY()=12.OR.READKEY()=268
         LOOP
      ENDIF
   ELSE
      do recloc
      @ WLIN01+2,WCOL01+20 GET DESCRI
      READ
      unlock
      IF READKEY()=12.OR.READKEY()=268
         LOOP
      ENDIF
      IF DESCRI = "*ELIMINA*"
         STORE "ESTA SEGURO QUE DESEA ELIMINAR ? (S/N)" TO TEX
         STORE "NS" TO WCH
         DO PREGUNTA
         IF WCH = "N"
            LOOP
         ELSE
            DO FILLOC
            DELETE
            UNLOCK
            LOOP
         ENDIF
      ENDIF
      FLUSH
   ENDIF
   @ WLIN01+3,WCOL01+15 GET WTIPREN
   READ
   IF WTIPREN=SPACE(3) .OR. WTIPREN="000" .OR.READKEY()=12.OR.READKEY()=268
      LOOP
   ENDIF
   STORE WTIPGRU+WTIPSUB+WTIPREN TO WCLAVE
   SELECT 1
   FIND &WCLAVE
   IF EOF()
      DO FILLOC
      APPEND BLANK
      UNLOCK
      DO RECLOC
      REPLACE GRUPO    WITH WTIPGRU
      REPLACE SUBGRUPO WITH WTIPSUB
      REPLACE RENGLON  WITH WTIPREN
      FLUSH
      @ WLIN01+3,WCOL01+20 GET DESCRI
      READ
      unlock
      IF READKEY()=12.OR.READKEY()=268
         LOOP
      ENDIF
   ELSE
      do recloc
      @ WLIN01+3,WCOL01+20 GET DESCRI
      READ
      unlock
      IF READKEY()=12.OR.READKEY()=268
         LOOP
      ENDIF
      IF DESCRI = "*ELIMINA*"
         STORE "ESTA SEGURO QUE DESEA ELIMINAR ? (S/N)" TO TEX
         STORE "NS" TO WCH
         DO PREGUNTA
         IF WCH = "N"
            LOOP
         ELSE
            DO FILLOC
            DELETE
            UNLOCK
            LOOP
         ENDIF
      ENDIF
      FLUSH
   ENDIF
ENDDO
RETURN
