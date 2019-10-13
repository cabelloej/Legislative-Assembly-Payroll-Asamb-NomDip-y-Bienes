STORE .T. TO WCARGABIEN
STORE WCODIGO          TO WLASTCODIGO
STORE SPACE(40)        TO WDESCRI1
STORE SPACE(40)        TO WDESCRI2
STORE SPACE(40)        TO WDESCRI3
STORE SPACE(40)        TO WDESCRI4
STORE SPACE(06)        TO WUNIDAD
STORE 0                TO WVALOR
STORE "B"              TO WESTADO
DO WHILE WCARGABIEN
   @ 00,00 CLEAR
   @ 00,00 SAY  QQWW
   @ 01,00 SAY "INVENTARIO DE BIENES (FICHA DEL BIEN)"
   @ 02,00 SAY "+----------------------------------------+-------------------------------------+"
   @ 03,00 SAY "|     CODIGO Y DESCRIPCION DEL BIEN      |             TIPO DE BIEN            |"
   @ 04,00 SAY "|                                        |GR:                                  |"
   @ 05,00 SAY "|                                        |SG:                                  |"
   @ 06,00 SAY "|                                        |RG:                                  |"
   @ 07,00 SAY "|                                        |-------------------------------------|"
   @ 08,00 SAY "|                                        |       UBICACION ADMINISTRATIVA      |"
   @ 09,00 SAY "|ESTADO:                                 |GR:                                  |"
   @ 10,00 SAY "|----------------------------------------|SG:                                  |"
   @ 11,00 SAY "|         DATOS DEL INGRESO              |RG:                                  |"
   @ 12,00 SAY "|GR:                                     |-------------------------------------|"
   @ 13,00 SAY "|SG:                                     |     UBICACION GEOGRAFICA ACTUAL     |"
   @ 14,00 SAY "|RF:                    Nro.:            |GR:                                  |"
   @ 15,00 SAY "|FC          Un:        Val.:            |SG:                                  |"
   @ 16,00 SAY "|----------------------------------------|RG:                                  |"
   @ 17,00 SAY "|         DATOS DEL EGRESO               +-------------------------------------|"
   @ 18,00 SAY "|GR:                                     |  REUBICACION GEOGRAFICA AL EGRESO   |"
   @ 19,00 SAY "|SG:                                     |GR:                                  |"
   @ 20,00 SAY "|RF:                    Nro.:            |SG:                                  |"
   @ 21,00 SAY "|FC:                                     |RG:                                  |"
   @ 22,00 SAY "+----------------------------------------+-------------------------------------+"
   STORE "INGRESE EL CODIGO DEL BIEN" TO MES
   DO MENSAJE WITH MES
   @ 04,01 GET WCODIGO
   READ
   IF WCODIGO = SPACE(12) .OR.READKEY()=12.OR.READKEY()=268
      EXIT
   ENDIF
   SELECT 7
   FIND &WCODIGO
   IF EOF()
      IF WPRG = "INGRESO"
         STORE .T. TO WINGRESA
         STORE .T. TO WMODIFICA
         STORE "BIEN NO REGISTRADO, INGRESAR ? (S/N)" TO TEX
         STORE "SN" TO WCH
         DO PREGUNTA
         IF WCH = "N"
            LOOP
         ENDIF
      ELSE
         STORE "BIEN NO REGISTRADO, VERIFIQUE" TO MES
         DO AVISO WITH MES
         LOOP
      ENDIF
   ELSE
      STORE .F. TO WINGRESA
      STORE .F. TO WMODIFICA
      STORE DESCRI1          TO WDESCRI1
      STORE DESCRI2          TO WDESCRI2
      STORE DESCRI3          TO WDESCRI3
      STORE DESCRI4          TO WDESCRI4
      STORE TIPGRU           TO WTIPGRU
      STORE TIPSUB           TO WTIPSUB
      STORE TIPREN           TO WTIPREN
      STORE ADMGRU           TO WADMGRU
      STORE ADMSUB           TO WADMSUB
      STORE ADMREN           TO WADMREN
      STORE GEOGRU           TO WGEOGRU
      STORE GEOSUB           TO WGEOSUB
      STORE GEOREN           TO WGEOREN
      STORE INGGRU           TO WINGGRU
      STORE INGSUB           TO WINGSUB
      STORE INGTIPREF        TO WINGTIPREF
      STORE INGNUMREF        TO WINGNUMREF
      STORE INGFECHA         TO WINGFECHA
      STORE EGRGRU           TO WEGRGRU
      STORE EGRSUB           TO WEGRSUB
      STORE EGRTIPREF        TO WEGRTIPREF
      STORE EGRNUMREF        TO WEGRNUMREF
      STORE EGRFECHA         TO WEGRFECHA
      STORE EGRGEOGRU        TO WEGRGEOGRU
      STORE EGRGEOSUB        TO WEGRGEOSUB
      STORE EGRGEOREN        TO WEGRGEOREN
      STORE UNIDAD           TO WUNIDAD
      STORE VALOR            TO WVALOR
      STORE ESTADO           TO WESTADO
   ENDIF
   STORE WTIPGRU+"000000"        TO WCLAVEGRU
   STORE WTIPGRU+WTIPSUB+"000"   TO WCLAVESUB
   STORE WTIPGRU+WTIPSUB+WTIPREN TO WCLAVEREN
   IF WTIPGRU <> SPACE(3)
      SELECT 1
      FIND &WCLAVEGRU
      IF EOF()
         STORE "NO REGISTRADO" TO WTIPGRUDES
      ELSE
         STORE DESCRI          TO WTIPGRUDES
      ENDIF
      FIND &WCLAVESUB
      IF EOF()
         STORE "NO REGISTRADO" TO WTIPSUBDES
      ELSE
         STORE DESCRI          TO WTIPSUBDES
      ENDIF
      FIND &WCLAVEREN
      IF EOF()
         STORE "NO REGISTRADO" TO WTIPRENDES
      ELSE
         STORE DESCRI          TO WTIPRENDES
      ENDIF
   ELSE
      STORE SPACE(20)          TO WTIPGRUDES
      STORE SPACE(20)          TO WTIPSUBDES
      STORE SPACE(20)          TO WTIPRENDES
   ENDIF
   STORE WADMGRU+"000000"        TO WCLAVEGRU
   STORE WADMGRU+WADMSUB+"000"   TO WCLAVESUB
   STORE WADMGRU+WADMSUB+WADMREN TO WCLAVEREN
   IF WADMGRU <> SPACE(3)
      SELECT 2
      FIND &WCLAVEGRU
      IF EOF()
         STORE "NO REGISTRADO" TO WADMGRUDES
      ELSE
         STORE DESCRI          TO WADMGRUDES
      ENDIF
      FIND &WCLAVESUB
      IF EOF()
         STORE "NO REGISTRADO" TO WADMSUBDES
      ELSE
         STORE DESCRI          TO WADMSUBDES
      ENDIF
      FIND &WCLAVEREN
      IF EOF()
         STORE "NO REGISTRADO" TO WADMRENDES
      ELSE
         STORE DESCRI          TO WADMRENDES
      ENDIF
   ELSE
      STORE SPACE(20)          TO WADMGRUDES
      STORE SPACE(20)          TO WADMSUBDES
      STORE SPACE(20)          TO WADMRENDES
   ENDIF
   STORE WGEOGRU+"000000"        TO WCLAVEGRU
   STORE WGEOGRU+WGEOSUB+"000"   TO WCLAVESUB
   STORE WGEOGRU+WGEOSUB+WGEOREN TO WCLAVEREN
   IF WGEOGRU <> SPACE(3)
      SELECT 3
      FIND &WCLAVEGRU
      IF EOF()
         STORE "NO REGISTRADO" TO WGEOGRUDES
      ELSE
         STORE DESCRI          TO WGEOGRUDES
      ENDIF
      FIND &WCLAVESUB
      IF EOF()
         STORE "NO REGISTRADO" TO WGEOSUBDES
      ELSE
         STORE DESCRI          TO WGEOSUBDES
      ENDIF
      FIND &WCLAVEREN
      IF EOF()
         STORE "NO REGISTRADO" TO WGEORENDES
      ELSE
         STORE DESCRI          TO WGEORENDES
      ENDIF
   ELSE
      STORE SPACE(20)          TO WGEOGRUDES
      STORE SPACE(20)          TO WGEOSUBDES
      STORE SPACE(20)          TO WGEORENDES
   ENDIF
   STORE WEGRGEOGRU+"000000"              TO WCLAVEGRU
   STORE WEGRGEOGRU+WEGRGEOSUB+"000"      TO WCLAVESUB
   STORE WEGRGEOGRU+WEGRGEOSUB+WEGRGEOREN TO WCLAVEREN
   IF WEGRGEOGRU <> SPACE(3)
      SELECT 3
      FIND &WCLAVEGRU
      IF EOF()
         STORE "NO REGISTRADO" TO WEGEOGRUD
      ELSE
         STORE DESCRI          TO WEGEOGRUD
      ENDIF
      FIND &WCLAVESUB
      IF EOF()
         STORE "NO REGISTRADO" TO WEGEOSUBD
      ELSE
         STORE DESCRI          TO WEGEOSUBD
      ENDIF
      FIND &WCLAVEREN
      IF EOF()
         STORE "NO REGISTRADO" TO WEGEOREND
      ELSE
         STORE DESCRI          TO WEGEOREND
      ENDIF
   ELSE
      STORE SPACE(30)          TO WEGEOGRUD
      STORE SPACE(30)          TO WEGEOSUBD
      STORE SPACE(30)          TO WEGEOREND
   ENDIF
   STORE WINGGRU+"000000" TO WCLAVEGRU
   STORE WINGGRU+WINGSUB  TO WCLAVESUB
   IF WINGGRU <> SPACE(3)
      SELECT 4
      FIND &WCLAVEGRU
      IF EOF()
         STORE "NO REGISTRADO" TO WINGGRUDES
      ELSE
         STORE DESCRI          TO WINGGRUDES
      ENDIF
      FIND &WCLAVESUB
      IF EOF()
         STORE "NO REGISTRADO" TO WINGSUBDES
      ELSE
         STORE DESCRI          TO WINGSUBDES
      ENDIF
   ELSE
      STORE SPACE(20)          TO WINGGRUDES
      STORE SPACE(20)          TO WINGSUBDES
   ENDIF
   STORE WEGRGRU+"000000" TO WCLAVEGRU
   STORE WEGRGRU+WEGRSUB  TO WCLAVESUB
   IF WEGRGRU <> SPACE(3)
      SELECT 5
      FIND &WCLAVEGRU
      IF EOF()
         STORE "NO REGISTRADO" TO WEGRGRUDES
      ELSE
         STORE DESCRI          TO WEGRGRUDES
      ENDIF
      FIND &WCLAVESUB
      IF EOF()
         STORE "NO REGISTRADO" TO WEGRSUBDES
      ELSE
         STORE DESCRI          TO WEGRSUBDES
      ENDIF
   ELSE
      STORE SPACE(30)          TO WEGRGRUDES
      STORE SPACE(30)          TO WEGRSUBDES
   ENDIF
   IF WINGTIPREF <> SPACE(3)
      SELECT 6
      FIND &WINGTIPREF
      IF EOF()
         STORE "NO REGISTRADO" TO WINGREFDES
      ELSE
         STORE DESCRI          TO WINGREFDES
      ENDIF
   ELSE
      STORE SPACE(20)           TO WINGREFDES
   ENDIF
   IF WEGRTIPREF <> SPACE(3)
      FIND &WEGRTIPREF
      IF EOF()
         STORE "NO REGISTRADO" TO WEGRREFDES
      ELSE
         STORE DESCRI          TO WEGRREFDES
      ENDIF
   ELSE
      STORE SPACE(30)          TO WEGRREFDES
   ENDIF
   @ 05,01 SAY WDESCRI1
   @ 06,01 SAY WDESCRI2
   @ 07,01 SAY WDESCRI3
   @ 08,01 SAY WDESCRI4
   @ 09,10 SAY WESTADO

   @ 12,04 SAY WINGGRU+" "+WINGGRUDES
   @ 13,04 SAY WINGSUB+" "+WINGSUBDES
   @ 14,04 SAY WINGREFDES
   @ 14,29 SAY WINGNUMREF
   @ 15,04 SAY WINGFECHA
   @ 15,16 SAY WUNIDAD
   @ 15,29 SAY WVALOR PICTURE "#########.##"

   @ 18,04 SAY WEGRGRU+" "+WEGRGRUDES
   @ 19,04 SAY WEGRSUB+" "+WEGRSUBDES
   @ 20,04 SAY WEGRREFDES
   @ 20,29 SAY WEGRNUMREF
   @ 21,04 SAY WEGRFECHA
   @ 04,45 SAY WTIPGRU+" "+WTIPGRUDES
   @ 05,45 SAY WTIPSUB+" "+WTIPSUBDES
   @ 06,45 SAY WTIPREN+" "+WTIPRENDES
   @ 09,45 SAY WADMGRU+" "+WADMGRUDES
   @ 10,45 SAY WADMSUB+" "+WADMSUBDES
   @ 11,45 SAY WADMREN+" "+WADMRENDES
   @ 14,45 SAY WGEOGRU+" "+WGEOGRUDES
   @ 15,45 SAY WGEOSUB+" "+WGEOSUBDES
   @ 16,45 SAY WGEOREN+" "+WGEORENDES

   @ 19,45 SAY WEGRGEOGRU+" "+WEGEOGRUD
   @ 20,45 SAY WEGRGEOSUB+" "+WEGEOSUBD
   @ 21,45 SAY WEGRGEOREN+" "+WEGEOREND
   IF .NOT. WMODIFICA
      STORE "OPCIONES: (M)ODIFICAR, (E)LIMINAR, (S)ALIR" TO TEX
      STORE "SME" TO WCH
      DO PREGUNTA
      STORE WCH TO WRESP
      IF WRESP= "S"
         LOOP
      ELSE
         IF WRESP= "M"
            STORE .T. TO WMODIFICA
         ELSE
            IF WPRG = "INGRESO"
               STORE "ESTA SEGURO QUE DESEA ELIMINAR ? (S/N)" TO TEX
               STORE "NS" TO WCH
               DO PREGUNTA
               IF WCH = "N"
                  LOOP
               ELSE
                   *** CHEQUEAR QUE EL TIPO NO ESTE SIN CONEXIONES
                   SELECT 7
                   DELETE
                   LOOP
               ENDIF
            ELSE
               STORE "SOLO PUEDE ELIMINAR POR INGRESO DE BIENES" TO MES
               DO AVISO WITH MES
               LOOP
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF WMODIFICA .AND. (WCODIGO = WLASTCODIGO .OR. WPRG = "FICHA")
      *** DESCRIPCION
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE LA DESCRIPCION DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 05,01 GET WDESCRI1
         @ 06,01 GET WDESCRI2
         @ 07,01 GET WDESCRI3
         @ 08,01 GET WDESCRI4
         @ 09,10 GET WESTADO
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WDESCRI1 <> SPACE(30)
            EXIT
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF

      *** GRUPO DEL TIPO DE BIEN
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL TIPO (GRUPO) DEL INGRESO" TO MES
         DO MENSAJE WITH MES
         @ 04,45 GET WTIPGRU
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WTIPGRU <> SPACE(3)
            STORE WTIPGRU+"000000" TO WCLAVE
            SELECT 1
            FIND &WCLAVE
            IF EOF()
               STORE "TIPO DE BIEN (GRUPO) NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 05 TO WLIN01
                  STORE 00 TO WCOL01
                  DO BIE0101
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 04,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** SUBGRUPO DEL TIPO DE BIEN
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL TIPO DE BIEN (SUBGRUPO)" TO MES
         DO MENSAJE WITH MES
         @ 05,45 GET WTIPSUB
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WTIPSUB <> SPACE(3)
            STORE WTIPGRU+WTIPSUB+"000" TO WCLAVE
            SELECT 1
            FIND &WCLAVE
            IF EOF()
               STORE "TIPO DE BIEN (SUBGRUPO) NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 06 TO WLIN01
                  STORE 00 TO WCOL01
                  DO BIE0101
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 05,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** RENGLON DEL TIPO DE BIEN
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL TIPO DE BIEN (RENGLON)" TO MES
         DO MENSAJE WITH MES
         @ 06,45 GET WTIPREN
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WTIPREN <> SPACE(3)
            STORE WTIPGRU+WTIPSUB+WTIPREN TO WCLAVE
            SELECT 1
            FIND &WCLAVE
            IF EOF()
               STORE "TIPO DE BIEN (RENGLON) NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 07 TO WLIN01
                  STORE 00 TO WCOL01
                  DO BIE0101
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 06,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF

      *** GRUPO ADMINISTRATIVO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL GRUPO ADMINISTRATIVO DEL INGRESO" TO MES
         DO MENSAJE WITH MES
         @ 09,45 GET WADMGRU
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WADMGRU <> SPACE(3)
            STORE WADMGRU+"000000" TO WCLAVE
            SELECT 2
            FIND &WCLAVE
            IF EOF()
               STORE "GRUPO ADMINISTRATIVO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 10 TO WLIN02
                  STORE 00 TO WCOL02
                  DO BIE0102
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 09,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** SUBGRUPO ADMINISTRATIVO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL SUBGRUPO ADMINISTRATIVO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 10,45 GET WADMSUB
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WADMSUB <> SPACE(3)
            STORE WADMGRU+WADMSUB+"000" TO WCLAVE
            SELECT 2
            FIND &WCLAVE
            IF EOF()
               STORE "SUBGRUPO ADMINISTRATIVO DEL BIEN NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 11 TO WLIN02
                  STORE 00 TO WCOL02
                  DO BIE0102
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 10,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** RENGLON ADMINISTRATIVO DEL BIEN
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL RENGLON ADMINISTRATIVO" TO MES
         DO MENSAJE WITH MES
         @ 11,45 GET WADMREN
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WADMREN <> SPACE(3)
            STORE WADMGRU+WADMSUB+WADMREN TO WCLAVE
            SELECT 2
            FIND &WCLAVE
            IF EOF()
               STORE "RENGLON ADMINISTRATIVO DEL BIEN NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN02
                  STORE 00 TO WCOL02
                  DO BIE0102
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 11,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF

      *** GRUPO GEOGRAFICO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL GRUPO GEOGRAFICO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 14,45 GET WGEOGRU
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WGEOGRU <> SPACE(3)
            STORE WGEOGRU+"000000" TO WCLAVE
            SELECT 3
            FIND &WCLAVE
            IF EOF()
               STORE "GRUPO GEOGRAFICO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 15 TO WLIN03
                  STORE 00 TO WCOL03
                  DO BIE0103
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 14,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** SUBGRUPO GEOGRAFICO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL SUBGRUPO GEOGRAFICO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 15,45 GET WGEOSUB
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WGEOSUB <> SPACE(3)
            STORE WGEOGRU+WGEOSUB+"000" TO WCLAVE
            SELECT 3
            FIND &WCLAVE
            IF EOF()
               STORE "SUBGRUPO GEOGRAFICO DEL BIEN NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 16 TO WLIN03
                  STORE 00 TO WCOL03
                  DO BIE0103
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 15,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** RENGLON GEOGRAFICO DEL BIEN
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL RENGLON GEOGRAFICO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 16,45 GET WGEOREN
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WGEOREN <> SPACE(3)
            STORE WGEOGRU+WGEOSUB+WGEOREN TO WCLAVE
            SELECT 3
            FIND &WCLAVE
            IF EOF()
               STORE "RENGLON GEOGRAFICO DEL BIEN NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 17 TO WLIN03
                  STORE 00 TO WCOL03
                  DO BIE0103
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 16,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
   ENDIF
   IF WMODIFICA .AND. WPRG = "INGRESO"
      *** GRUPO DEL INGRESO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL TIPO (GRUPO) DEL INGRESO" TO MES
         DO MENSAJE WITH MES
         @ 12,04 GET WINGGRU
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WINGGRU <> SPACE(3)
            STORE WINGGRU+"000000" TO WCLAVEING
            SELECT 4
            FIND &WCLAVEING
            IF EOF()
               STORE "TIPO DE INGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 13 TO WLIN04
                  STORE 00 TO WCOL04
                  DO BIE0104
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 12,08 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** SUBGRUPO DEL INGRESO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL DETALLE (SUBGRUPO) DEL INGRESO" TO MES
         DO MENSAJE WITH MES
         @ 13,04 GET WINGSUB
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WINGSUB <> SPACE(6)
            STORE WINGGRU+WINGSUB TO WCLAVEING
            SELECT 4
            FIND &WCLAVEING
            IF EOF()
               STORE "DETALLE (SUBGRUPO) DEL INGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 14 TO WLIN04
                  STORE 00 TO WCOL04
                  DO BIE0104
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 13,11 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF

      *** TIPO DE REFERENCIA
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE TIPO DE REFERENCIA DEL INGRESO" TO MES
         DO MENSAJE WITH MES
         @ 14,04 GET WINGTIPREF
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WINGTIPREF <> SPACE(3)
            SELECT 6
            FIND &WINGTIPREF
            IF EOF()
               STORE "TIPO DE REFERENCIA DEL INGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 15 TO WLIN06
                  STORE 00 TO WCOL06
                  DO BIE0106
                  RESTORE SCRE FROM S0107
                 LOOP
               ENDIF
            ELSE
               @ 14,04 SAY DESCRI
               @ 14,29 GET WINGNUMREF
               READ
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
          LOOP
      ENDIF

      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         @ 15,04 GET WINGFECHA
         READ
         IF WINGFECHA <> CTOD("  -  -  ")
            EXIT
         ENDIF
      ENDDO
      @ 15,16 GET WUNIDAD
      READ
      @ 15,29 GET WVALOR PICTURE "#########.##"
      READ
   ENDIF
   IF WMODIFICA .AND. WPRG = "EGRESO"
      *** GRUPO DEL EGRESO
      IF WEGRGRU <> SPACE(3)
         STORE .T. TO WASKREVERSA
      ELSE
         STORE .F. TO WASKREVERSA
      ENDIF
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL GRUPO DEL EGRESO (*R* PARA REVERSAR EGRESO)" TO MES
         DO MENSAJE WITH MES
         @ 18,04 GET WEGRGRU
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WEGRGRU <> SPACE(3) .AND. WEGRGRU <> "*R*"
            STORE WEGRGRU+"000000" TO WCLAVEEGR
            SELECT 5
            FIND &WCLAVEEGR
            IF EOF()
               STORE "TIPO DE EGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN05
                  STORE 00 TO WCOL05
                  DO BIE0105
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 18,08 SAY DESCRI
               EXIT
            ENDIF
         ELSE
            IF WASKREVERSA .AND. WEGRGRU="*R*"
               STORE "DESEA REVERSAR EL EGRESO ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SELECT 7
                  DO RECLOC
                  REPLACE  EGRGRU      WITH SPACE(3)
                  REPLACE  EGRSUB      WITH SPACE(6)
                  REPLACE  EGRTIPREF   WITH SPACE(3)
                  REPLACE  EGRNUMREF   WITH SPACE(12)
                  REPLACE  EGRFECHA    WITH CTOD("  -  -  ")
                  REPLACE  EGRGEOGRU   WITH SPACE(3)
                  REPLACE  EGRGEOSUB   WITH SPACE(3)
                  REPLACE  EGRGEOREN   WITH SPACE(3)
                  UNLOCK
                  STORE .F. TO WCARGANDO
                  EXIT
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** SUBGRUPO DEL EGRESO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL DETALLE (SUBGRUPO) DEL EGRESO" TO MES
         DO MENSAJE WITH MES
         @ 19,04 GET WEGRSUB
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WEGRSUB <> SPACE(6)
            STORE WEGRGRU+WEGRSUB TO WCLAVEEGR
            SELECT 5
            FIND &WCLAVEEGR
            IF EOF()
               STORE "DETALLE (SUBGRUPO) DEL EGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN05
                  STORE 00 TO WCOL05
                  DO BIE0105
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 19,11 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF

      *** TIPO DE REFERENCIA
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE TIPO DE REFERENCIA DEL EGRESO" TO MES
         DO MENSAJE WITH MES
         @ 20,04 GET WEGRTIPREF
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WEGRTIPREF <> SPACE(3)
            SELECT 6
            FIND &WEGRTIPREF
            IF EOF()
               STORE "TIPO DE REFERENCIA DEL EGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN06
                  STORE 00 TO WCOL06
                  DO BIE0106
                  RESTORE SCRE FROM S0107
                 LOOP
               ENDIF
            ELSE
               @ 20,04 SAY DESCRI
               @ 20,29 GET WEGRNUMREF
               READ
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
          LOOP
      ENDIF
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         @ 21,04 GET WEGRFECHA
         READ
         IF WINGFECHA <> CTOD("  -  -  ")
            EXIT
         ENDIF
      ENDDO
      *** GRUPO GEOGRAFICO DEL EGRESO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL GRUPO GEOGRAFICO DEL EGRESO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 19,45 GET WEGRGEOGRU
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WEGRGEOGRU <> SPACE(3)
            STORE WEGRGEOGRU+"000000" TO WCLAVE
            SELECT 3
            FIND &WCLAVE
            IF EOF()
               STORE "GRUPO GEOGRAFICO DEL EGRESO NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN03
                  STORE 00 TO WCOL03
                  DO BIE0103
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 19,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** SUBGRUPO GEOGRAFICO DEL EGRESO
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL SUBGRUPO GEOGRAFICO DEL EGRESO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 20,45 GET WEGRGEOSUB
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WEGRGEOSUB <> SPACE(3)
            STORE WEGRGEOGRU+WEGRGEOSUB+"000" TO WCLAVE
            SELECT 3
            FIND &WCLAVE
            IF EOF()
               STORE "SUBGRUPO GEOGRAFICO DEL EGRESO DEL BIEN NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN03
                  STORE 00 TO WCOL03
                  DO BIE0103
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 20,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
      *** RENGLON GEOGRAFICO DEL EGRESO DEL BIEN
      STORE .T. TO WCARGANDO
      DO WHILE WCARGANDO
         STORE "INGRESE EL RENGLON GEOGRAFICO DEL EGRESO DEL BIEN" TO MES
         DO MENSAJE WITH MES
         @ 21,45 GET WEGRGEOREN
         READ
         IF READKEY()=12.OR.READKEY()=268
            STORE .F. TO WCARGANDO
            EXIT
         ENDIF
         IF WEGRGEOREN <> SPACE(3)
            STORE WEGRGEOGRU+WEGRGEOSUB+WEGRGEOREN TO WCLAVE
            SELECT 3
            FIND &WCLAVE
            IF EOF()
               STORE "RENGLON GEOGRAFICO DEL EGRESO DEL BIEN NO REGISTRADO, DESEA INGRESAR ? (S/N)" TO TEX
               STORE "SN" TO WCH
               DO PREGUNTA
               IF WCH = "S"
                  SAVE SCRE TO S0107
                  STORE 12 TO WLIN03
                  STORE 00 TO WCOL03
                  DO BIE0103
                  RESTORE SCRE FROM S0107
                  LOOP
               ENDIF
            ELSE
               @ 21,49 SAY DESCRI
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF .NOT. WCARGANDO
         LOOP
      ENDIF
   ENDIF
   STORE "CONFORME ? (S/N)" TO TEX
   STORE "SN" TO WCH
   DO PREGUNTA
   IF WCH = "N"
      LOOP
   ENDIF
   SELECT 7
   IF WINGRESA
      DO FILLOC
      APPEND BLANK
      UNLOCK
      DO RECLOC
      REPLACE CODIGO  WITH WCODIGO
   ENDIF
   DO RECLOC
   REPLACE DESCRI1       WITH WDESCRI1
   REPLACE DESCRI2       WITH WDESCRI2
   REPLACE DESCRI3       WITH WDESCRI3
   REPLACE DESCRI4       WITH WDESCRI4
   REPLACE ESTADO        WITH WESTADO
   REPLACE TIPGRU        WITH WTIPGRU
   REPLACE TIPSUB        WITH WTIPSUB
   REPLACE TIPREN        WITH WTIPREN
   REPLACE ADMGRU        WITH WADMGRU
   REPLACE ADMSUB        WITH WADMSUB
   REPLACE ADMREN        WITH WADMREN
   REPLACE GEOGRU        WITH WGEOGRU
   REPLACE GEOSUB        WITH WGEOSUB
   REPLACE GEOREN        WITH WGEOREN
   REPLACE INGGRU        WITH WINGGRU
   REPLACE INGSUB        WITH WINGSUB
   REPLACE INGTIPREF     WITH WINGTIPREF
   REPLACE INGNUMREF     WITH WINGNUMREF
   REPLACE INGFECHA      WITH WINGFECHA
   REPLACE EGRGRU        WITH WEGRGRU
   REPLACE EGRSUB        WITH WEGRSUB
   REPLACE EGRTIPREF     WITH WEGRTIPREF
   REPLACE EGRNUMREF     WITH WEGRNUMREF
   REPLACE EGRFECHA      WITH WEGRFECHA
   REPLACE EGRGEOGRU     WITH WEGRGEOGRU
   REPLACE EGRGEOSUB     WITH WEGRGEOSUB
   REPLACE EGRGEOREN     WITH WEGRGEOREN
   REPLACE UNIDAD        WITH WUNIDAD
   REPLACE VALOR         WITH WVALOR
   FLUSH
   UNLOCK
ENDDO
RETURN

