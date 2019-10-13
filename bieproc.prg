****************
PROCEDURE CHKACC
****************
PARAMETERS WUSERCODE,WPROGRAMA,WACCESO,WFILTRO
SELECT SYSUSERD
STORE WUSERCODE+WPROGRAMA TO WCLAVEACC
SEEK WCLAVEACC
IF FOUND()
   STORE ACCESO TO WACCESO
   STORE FILTRO TO WFILTRO
ENDIF
RETURN


PROCEDURE INDICES
store "DESEA REORGANIZAR LOS INDICES (S/N)" TO TEX
STORE "SN" TO WCH
DO PREGUNTA
IF WCH = "N"
   RETURN
ENDIF
@ 0,0 CLEAR
@ 08,20 SAY "REORGANIZANDO INDICES, FAVOR ESPERAR"
CLOSE DATA
CLOSE INDEX

SELECT 1
USE BIETIP EXCL
DO FILLOC
INDEX ON GRUPO+SUBGRUPO+RENGLON TO BIETIP
UNLOCK ALL
***
SELECT 2
USE BIEADM EXCL
DO FILLOC
INDEX ON GRUPO+SUBGRUPO+RENGLON TO BIEADM
UNLOCK ALL
***
SELECT 3
USE BIEGEO EXCL
DO FILLOC
INDEX ON GRUPO+SUBGRUPO+RENGLON TO BIEGEO
UNLOCK ALL
***
SELECT 4 
USE BIEING EXCL
DO FILLOC
INDEX ON GRUPO+SUBGRUPO TO BIEING
UNLOCK ALL
***
SELECT 5
USE BIEEGR EXCL
DO FILLOC
INDEX ON GRUPO+SUBGRUPO TO BIEEGR
UNLOCK ALL
***
SELECT 6
USE BIEREF EXCL
DO FILLOC
INDEX ON CODIGO TO BIEREF
UNLOCK ALL
***
SELECT 7
USE BIEBIEN EXCL
DO FILLOC
INDEX ON CODIGO                      TO BIEBIEN1
INDEX ON TIPGRU+TIPSUB+TIPREN+CODIGO TO BIEBIEN2
INDEX ON ADMGRU+ADMSUB+ADMREN+CODIGO TO BIEBIEN3
INDEX ON GEOGRU+GEOSUB+GEOREN+CODIGO TO BIEBIEN4
INDEX ON INGTIPREF+INGNUMREF+CODIGO  TO BIEBIEN5
INDEX ON EGRTIPREF+EGRNUMREF+CODIGO  TO BIEBIEN6
INDEX ON DTOS(INGFECHA)+CODIGO       TO BIEBIEN7
INDEX ON DTOS(EGRFECHA)+CODIGO       TO BIEBIEN8
UNLOCK ALL

STORE "OPERACION EFECTUADA SATISFACTORIAMENTE, OPRIMA <ENTER> PARA CONTINUAR." TO MES
DO AVISO WITH MES
CLOSE DATA
CLOSE INDEX
RETURN TO MASTER
************************
PROCEDURE RECLOC
SAVE SCRE TO SCRE1
STORE .T. TO WRECLOC
STORE .T. TO WFLAGMES
DO WHILE WRECLOC
   IF RLOCK()
      EXIT
   ELSE
      IF WFLAGMES
         @ 12,05 CLEAR TO 18,75
         @ 12,05 TO 18,75
         @ 14,15 SAY "REGISTRO OCUPADO POR OTRO USUARIO, REINTENTANDO ..."
         @ 16,15 SAY "          OPRIMA [ESC] PARA ABANDONAR              "
         STORE .F. TO WFLAGMES
      ENDIF
      WVALUE = INKEY()
      IF WVALUE = 27
         store .t. to wjumping
         close data
         close index
         RETURN TO MASTER
      ENDIF
   ENDIF
ENDDO
RESTORE SCRE FROM SCRE1
RETURN
*************************
PROCEDURE FILLOC
SAVE SCRE TO SCRE1
STORE .T. TO WFILLOC
STORE .T. TO WFLAGMES
DO WHILE WFILLOC
   IF FLOCK()
      EXIT
   ELSE
      IF WFLAGMES
         @ 12,05 CLEAR TO 18,75
         @ 12,05 TO 18,75
         @ 14,15 SAY "ARCHIVO OCUPADO POR OTRO USUARIO, REINTENTANDO ..."
         @ 16,15 SAY "          OPRIMA [ESC] PARA ABANDONAR             "
         STORE .F. TO WFLAGMES
      ENDIF
      WVALUE = INKEY()
      IF WVALUE = 27
         store .t. to wjumping
         close data
         close index
         RETURN TO MASTER
      ENDIF
   ENDIF
ENDDO
RESTORE SCRE FROM SCRE1
RETURN
*************************
PROCEDURE PREGUNTA
STORE .T. TO WPREG
DO WHILE WPREG
   @ 23,0
   STORE SUBSTR(WCH,1,1) TO WCHOICE
   @ 23,40- (LEN(TEX)/2) SAY TEX GET WCHOICE
   READ
   STORE UPPER(WCHOICE) TO WCHOICE
   IF AT(WCHOICE,WCH) > 0
      STORE .F. TO WPREG
      EXIT
   ENDIF
ENDDO
@ 23,0
STORE WCHOICE TO WCH
RETURN
*************************
PROCEDURE MENSAJE
PARAMETERS MES
@ 23,0
@ 23,40-(LEN(MES)/2) SAY MES
RETURN
*************************
PROCEDURE AVISO
PARAMETERS MES
STORE " " TO X
?? CHR(7)
@ 23,0
@ 23,40-(LEN(MES)/2) SAY MES GET X
READ
@ 23,1
RETURN
*************************
PROCEDURE CHKPRINT
PARAMETERS SALTAR
store .t. to wprinting
do while wprinting
   store "Prepare la impresora y oprima <ENTTER> para continuar o (R)echazar" to Qmes
   @ 23,1
   @ 23,40-(len(Qmes)/2) say Qmes
   store " " to wstat
   @ 23,78 get wstat
   read
   if upper(wstat) = "R"
      store 1 to saltar
      store .f. to wprinting
   else
      store 0 to saltar
      if sys(13) = "READY"
         store .f. to wprinting
      endif
   endif
   @ 23,1
enddo
RETURN
*************************
procedure informa
store "I" to qw1
store "U" to qw2
store "G" to qw3
store "E" to qw4
store "T" to qw5
store "A" to qw6
store "O" to qw7
store "Z" to qw8
store "N" to qw9
store "A" to qw10
store " " to qw11
store "." to qw12
store "O" to qw13
store "D" to qw14
store "E" to qw15
store " " to qw16
store "L" to qw17
store "E" to qw18
store "D" to qw19
store " " to qw20
store "A" to qw21
store "V" to qw22
store "I" to qw23
store "T" to qw24
store "A" to qw25
store "L" to qw26
store "S" to qw27
store "I" to qw28
store "G" to qw29
store "E" to qw30
store "L" to qw31
store " " to qw32
store "A" to qw33
store "E" to qw34
store "L" to qw35
store "B" to qw36
store "M" to qw37
store "A" to qw38
store "S" to qw39
store "A" to qw40
STORE QW40+QW39+QW38+QW37+QW36+QW35+QW34+QW33+QW32+QW31+QW30+QW29+QW28 TO QQWW
STORE QQWW+QW27+QW26+QW25+QW24+QW23+QW22+QW21+QW20+QW19+QW18+QW17+QW16 TO QQWW
STORE QQWW+QW15+QW14+QW13+QW12+QW11+QW10+QW9+QW8+QW7+QW6+QW5+QW4+QW3+QW2+QW1 TO QQWW
RETURN
*************************
PROCEDURE STRNUMBER
STORE LEN(RTRIM(LTRIM(STR(WNUMBERN,7)))) TO WNUMLEN
STORE REPLICATE("0",7-WNUMLEN)+LTRIM(STR(WNUMBERN,7)) TO WNUMBERC
RETURN
*************************
PROCEDURE HD0302
STORE WPAGINA + 1 TO WPAGINA
IF WSALIDA = "M"
   if WPAGINA <> 1
      STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO MES
      DO AVISO WITH MES
      IF READKEY()=12 .OR. READKEY()=268
         STORE .T. TO WFLAGEXIT
      ENDIF
    endif
    @ 0,0 clear
ENDIF
IF WSALIDA = "M"
   @ 0,0 SAY QQWW
ELSE
   @ 0,0 SAY CHR(14)+QQWW
ENDIF
@ 00,60 SAY "FECHA :"+DTOC(DATE())
@ 01,60 SAY "PAGINA:"+STR(WPAGINA,4)
@ 01,00 SAY "LISTADO DE INGRESOS"
@ 02,00 SAY "GRUPO DE INGRESO:"+RTRIM(XINGGRUDES)
@ 02,40 SAY "SUBGRUPO:"+RTRIM(XINGSUBDES)
@ 03,00 SAY "TIPO DE REFEREN.:"+XTIPREFDES
@ 03,40 SAY "NUMERO  :"+XNUMREFDES
@ 04,00 SAY "INGRESOS DESDE :"+DTOC(XDESDE)+" HASTA :"+DTOC(XHASTA)
@ 05,00 SAY "+------------------------------------+------------+------+---------------------+"
@ 06,00 SAY "|          DATOS DEL INGRESO         |CODIGO      |UNIDAD|                VALOR|"
@ 07,00 SAY "+--------+---+------+---+------------|------------+------+---------------------|"
@ 08,00 SAY "|FECHA   |GR |SG    |REF|No.DE REF.  |DESCRIPCION DEL BIEN                     |"
@ 09,00 SAY "+--------+---+------+---+------------+-----------------------------------------+"
STORE 10 TO WLINEA
RETURN
*******************************
PROCEDURE HD0303
STORE WPAGINA + 1 TO WPAGINA
IF WSALIDA = "M"
   if WPAGINA <> 1
      STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO MES
      DO AVISO WITH MES
      IF READKEY()=12 .OR. READKEY()=268
         STORE .T. TO WFLAGEXIT
      ENDIF
    endif
    @ 0,0 clear
ENDIF
IF WSALIDA = "M"
   @ 0,0 SAY QQWW
ELSE
   @ 0,0 SAY CHR(14)+QQWW
ENDIF
@ 00,60 SAY "FECHA :"+DTOC(DATE())
@ 01,00 SAY "LISTADO DE EGRESOS"
@ 01,60 SAY "PAGINA:"+STR(WPAGINA,4)
@ 02,00 SAY "GRUPO DE EGRESO :"+RTRIM(XEGRGRUDES)
@ 02,40 SAY "SUBGRUPO:"+RTRIM(XEGRSUBDES)
@ 03,00 SAY "TIPO DE REFEREN.:"+XTIPREFDES
@ 03,40 SAY "NUMERO  :"+XNUMREFDES
@ 04,00 SAY "EGRESOS DESDE :"+DTOC(XDESDE)+" HASTA :"+DTOC(XHASTA)
@ 05,00 SAY "+------------------------------------+------------+------+---------------------+"
@ 06,00 SAY "|          DATOS DEL EGRESO          |CODIGO      |UNIDAD|                VALOR|"
@ 07,00 SAY "+--------+---+------+---+------------|------------+------+---------------------|"
@ 08,00 SAY "|FECHA   |GR |SG    |REF|No.DE REF.  |DESCRIPCION DEL BIEN                     |"
@ 09,00 SAY "+--------+---+------+---+------------+-----------------------------------------+"
STORE 10 TO WLINEA
RETURN
*******************************
PROCEDURE HD0304
STORE WPAGINA + 1 TO WPAGINA
IF WSALIDA = "M"
   if WPAGINA <> 1
      STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO MES
      DO AVISO WITH MES
      IF READKEY()=12 .OR. READKEY()=268
         STORE .T. TO WFLAGEXIT
      ENDIF
    endif
    @ 0,0 clear
ENDIF
IF WSALIDA = "M"
   @ 0,0 SAY QQWW
ELSE
   @ 0,0 SAY CHR(14)+QQWW
ENDIF
@ 00,60 SAY "FECHA :"+DTOC(DATE())
@ 01,00 SAY "LISTADO DE BIENES POR TIPO"
@ 01,60 SAY "PAGINA:"+STR(WPAGINA,4)
@ 02,00 SAY "GRUPO   :"+RTRIM(XTIPGRUDES)
@ 03,00 SAY "SUBGRUPO:"+RTRIM(XTIPSUBDES)
@ 04,00 SAY "RENGLON :"+RTRIM(XTIPRENDES)
@ 05,00 SAY "+---+---+---+------------+----------------------------------------+------------+"
@ 06,00 SAY "|GR |SG |RG |CODIGO      |DESCRIPCION DEL BIEN                    |       VALOR|"
@ 07,00 SAY "+---+---+---+------------+----------------------------------------+------------+"
STORE 08 TO WLINEA
RETURN
*******************************
PROCEDURE HD0305
STORE WPAGINA + 1 TO WPAGINA
IF WSALIDA = "M"
   if WPAGINA <> 1
      STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO MES
      DO AVISO WITH MES
      IF READKEY()=12 .OR. READKEY()=268
         STORE .T. TO WFLAGEXIT
      ENDIF
    endif
    @ 0,0 clear
ENDIF
IF WSALIDA = "M"
   @ 0,0 SAY QQWW
ELSE
   @ 0,0 SAY CHR(14)+QQWW
ENDIF
@ 00,60 SAY "FECHA :"+DTOC(DATE())
@ 01,00 SAY "LISTADO DE BIENES UBICACION ADMINISTRATIVA"
@ 01,60 SAY "PAGINA:"+STR(WPAGINA,4)
@ 02,00 SAY "GRUPO   :"+RTRIM(XADMGRUDES)
@ 03,00 SAY "SUBGRUPO:"+RTRIM(XADMSUBDES)
@ 04,00 SAY "RENGLON :"+RTRIM(XADMRENDES)
@ 05,00 SAY "+---+---+---+------------+----------------------------------------+------------+"
@ 06,00 SAY "|GR |SG |RG |CODIGO      |DESCRIPCION DEL BIEN                    |       VALOR|"
@ 07,00 SAY "+---+---+---+------------+----------------------------------------+------------+"
STORE 08 TO WLINEA
RETURN
*******************************
PROCEDURE HD0306
STORE WPAGINA + 1 TO WPAGINA
IF WSALIDA = "M"
   if WPAGINA <> 1
      STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO MES
      DO AVISO WITH MES
      IF READKEY()=12 .OR. READKEY()=268
         STORE .T. TO WFLAGEXIT
      ENDIF
    endif
    @ 0,0 clear
ENDIF
IF WSALIDA = "M"
   @ 0,0 SAY QQWW
ELSE
   @ 0,0 SAY CHR(14)+QQWW
ENDIF
@ 00,60 SAY "FECHA :"+DTOC(DATE())
@ 01,00 SAY "LISTADO DE BIENES UBICACION GEOGRAFICA"
@ 01,60 SAY "PAGINA:"+STR(WPAGINA,4)
@ 02,00 SAY "GRUPO   :"+RTRIM(XGEOGRUDES)
@ 03,00 SAY "SUBGRUPO:"+RTRIM(XGEOSUBDES)
@ 04,00 SAY "RENGLON :"+RTRIM(XGEORENDES)
@ 05,00 SAY "+---+---+---+------------+----------------------------------------+------------+"
@ 06,00 SAY "|GR |SG |RG |CODIGO      |DESCRIPCION DEL BIEN                    |       VALOR|"
@ 07,00 SAY "+---+---+---+------------+----------------------------------------+------------+"
STORE 08 TO WLINEA
RETURN
*******************************
