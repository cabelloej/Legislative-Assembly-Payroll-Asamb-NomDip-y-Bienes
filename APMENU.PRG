SELECT 20
USE SYSUSERD INDEX SYSUSERD
*** INI CONTROL DE ACCESO
STORE "APMENU " TO WPROGRAMA
STORE SPACE(1)  TO WACCESO
STORE SPACE(1)  TO WFILTRO
DO CHKACC WITH WUSERCODE,WPROGRAMA,WACCESO,WFILTRO
IF WACCESO=" "
   RETURN
ENDIF
*** FIN CONTROL DE ACCESO
USE


store "Nomina diputados" to wmodulo
@ 1,45 say space(34)
@ 01,80-len(wmodulo) say wmodulo
SET PROCEDURE TO APPROC
STORE SPACE(12) TO WCEDULA
STORE SPACE(4)  TO WCONCEPTO
STORE SPACE(6)  TO WCARGO
STORE SPACE(2)  TO WGRUPO
STORE SPACE(2)  TO WNOMINA
STORE SPACE(1) TO T7
STORE .T. TO WJUMPING
***************************************
*define menu menuap bar at line 3 shadow
define menu menuap bar at line 3 
*      define pad padap00 of menuap prompt "\<Sistema" skip
       define pad padap01 of menuap prompt "\<Instala" skip
       define pad padap02 of menuap prompt "\<Modulo " skip
       define pad padap03 of menuap prompt "\<Tabla  "
       define pad padap04 of menuap prompt "\<Accion "
       define pad padap05 of menuap prompt "\<Reporte"
       define pad padap06 of menuap prompt "\<Proceso"
       define pad padap07 of menuap prompt "ma\<Ntto."
       define pad padap08 of menuap prompt "\<Salir  "
       on pad padap03 of menuap activate popup subap03
       on pad padap04 of menuap activate popup subap04
       on pad padap05 of menuap activate popup subap05
       on pad padap06 of menuap activate popup subap06
       on pad padap07 of menuap activate popup subap07
       on sele pad padap08 of menuap return
       DEFINE POPUP subap03 FROM 4,30 shadow
              DEFINE BAR 01 OF subap03 PROMPT "       NOMINAS            " SKIP
              DEFINE BAR 02 OF subap03 PROMPT "\<Grupos de nominas       "
              DEFINE BAR 03 OF subap03 PROMPT "\<Nominas                 "
              DEFINE BAR 04 OF subap03 PROMPT "\<Ubicacion adm. personal "
              DEFINE BAR 05 OF subap03 PROMPT "\<Cargos                  "
              DEFINE BAR 06 OF subap03 PROMPT "c\<Entros de pago         "
              DEFINE BAR 07 OF subap03 PROMPT "c\<Ontratos colectivos    "
              DEFINE BAR 08 OF subap03 PROMPT "\<Sindicatos              "
              DEFINE BAR 09 OF subap03 PROMPT "\<Bancos                  "
              DEFINE BAR 10 OF subap03 PROMPT "conce\<Ptos nominales     "
              DEFINE BAR 11 OF subap03 PROMPT "       PERSONAL           "  SKIP
              DEFINE BAR 12 OF subap03 PROMPT "datos pe\<Rsonales        "
              DEFINE BAR 13 OF subap03 PROMPT "Datos \<Laborales         "
              DEFINE BAR 14 OF subap03 PROMPT "Concep\<Tos               "
              DEFINE BAR 15 OF subap03 PROMPT "\<Variacion de Conceptos  "
              DEFINE BAR 16 OF subap03 PROMPT "        VARIOS            "  SKIP
              DEFINE BAR 17 OF subap03 PROMPT "le\<Y del trabajo         "
              ON SELECTION POPUP subap03 DO subap03 WITH BAR()
       DEFINE POPUP subap04 FROM 4,40 shadow
              DEFINE BAR 01 OF subap04 PROMPT "\<Apertura de periodo       "
              DEFINE BAR 02 OF subap04 PROMPT "variacion \<Personal        "
              DEFINE BAR 03 OF subap04 PROMPT "variacion \<General         "
              DEFINE BAR 04 OF subap04 PROMPT "imprimir nomina \<Ordinaria "
              DEFINE BAR 05 OF subap04 PROMPT "imprimir nomina \<Conceptual"
              DEFINE BAR 06 OF subap04 PROMPT "imprimir nomina \<Vertical  "
              DEFINE BAR 07 OF subap04 PROMPT "imprimir nomina \<Bancaria  "
              DEFINE BAR 08 OF subap04 PROMPT "imprimir nomina \<Resumen   "
              DEFINE BAR 09 OF subap04 PROMPT "imprimir nomina var\<Iacion "
              DEFINE BAR 10 OF subap04 PROMPT "imprimir nomina \<Horizontal"
              DEFINE BAR 11 OF subap04 PROMPT "ci\<Erre de periodo         "
              ON SELECTION POPUP subap04 DO subap04 WITH BAR()
       DEFINE POPUP subap05 FROM 4,50 shadow
              DEFINE BAR 01 OF subap05 PROMPT "          TABLAS            " SKIP
              DEFINE BAR 02 OF subap05 PROMPT "\<Grupos de nominas           "
              DEFINE BAR 03 OF subap05 PROMPT "\<Nominas                     "
              DEFINE BAR 04 OF subap05 PROMPT "\<Ubicacion adm. personal     "
              DEFINE BAR 05 OF subap05 PROMPT "\<Cargos                      "
              DEFINE BAR 06 OF subap05 PROMPT "centros de \<Pago             "
              DEFINE BAR 07 OF subap05 PROMPT "c\<Ontratos colectivos        "
              DEFINE BAR 08 OF subap05 PROMPT "\<Sindicatos                  "
              DEFINE BAR 09 OF subap05 PROMPT "\<Bancos                      "
              DEFINE BAR 10 OF subap05 PROMPT "conc\<Eptos nominales         "
              DEFINE BAR 11 OF subap05 PROMPT "personal x \<Ficha            "
              DEFINE BAR 12 OF subap05 PROMPT "personal x grupo \<Y nomina   "
              DEFINE BAR 13 OF subap05 PROMPT "personal x ubic\<Acion adm.   "
              DEFINE BAR 14 OF subap05 PROMPT "        HISTORICOS            " SKIP
              DEFINE BAR 15 OF subap05 PROMPT "\<Individual                  "
              DEFINE BAR 16 OF subap05 PROMPT "nomina\<L                     "
              DEFINE BAR 17 OF subap05 PROMPT "concep\<Tual                  "
              DEFINE BAR 18 OF subap05 PROMPT "ahorro \<Habitacional         "
              ON SELECTION POPUP subap05 DO subap05 WITH BAR()
       DEFINE POPUP subap06 FROM 4,60 shadow
              DEFINE BAR 01 OF subap06 PROMPT "cambio de \<Cedula            "
              DEFINE BAR 02 OF subap06 PROMPT "cambio de codigo concep\<To   "
              DEFINE BAR 03 OF subap06 PROMPT "calculo de \<Vacaciones       "
              DEFINE BAR 04 OF subap06 PROMPT "calculo de \<Utilidades       "
              DEFINE BAR 05 OF subap06 PROMPT "calculo de \<Liquidacion      "
              ON SELECTION POPUP subap06 DO subap06 WITH BAR()
       DEFINE POPUP subap07 FROM 4,70 shadow
              DEFINE BAR 01 OF subap07 PROMPT "\<Reorganizar indices       "
              DEFINE BAR 02 OF subap07 PROMPT "\<Compactar tablas de datos "
              ON SELECTION POPUP subap07 DO subap07 WITH BAR()
*** ACTIVACION DEL MENU DE personal
do while .t.
   ACTIVATE MENU menuap 
enddo
*RELEASE menuap
CLOSE DATA
CLOSE INDEX
SET PROC TO SYSPROC
RETURN
*********************************
*** RUTINAS
***
PROCEDURE subap03
PARAMETERS SELBAR
DO CASE
   CASE SELBAR = 2
        save scre to wscre01
        do ap0102
        restore scre from wscre01
   CASE SELBAR = 3
        save scre to wscre01
        do ap0103
        restore scre from wscre01
   CASE SELBAR = 4
        save scre to wscre01
        do ap0104
        restore scre from wscre01
   CASE SELBAR = 5
        save scre to wscre01
        do ap0105
        restore scre from wscre01
   CASE SELBAR = 6
        save scre to wscre01
        do ap0106
        restore scre from wscre01
   CASE SELBAR = 7
        save scre to wscre01
        do ap0107
        restore scre from wscre01
   CASE SELBAR = 8
        save scre to wscre01
        do ap0108
        restore scre from wscre01
   CASE SELBAR = 9
        save scre to wscre01
        do ap0109
        restore scre from wscre01
   CASE SELBAR = 10
        save scre to wscre01
        do ap0110
        restore scre from wscre01
   CASE SELBAR = 12
        save scre to wscre01
        do ap0112
        restore scre from wscre01
   CASE SELBAR = 13
        save scre to wscre01
        do ap0113
        restore scre from wscre01
   CASE SELBAR = 14
        save scre to wscre01
        do ap0114
        restore scre from wscre01
   CASE SELBAR = 15
        save scre to wscre01
        do ap0115
        restore scre from wscre01
   CASE SELBAR = 17
        save scre to wscre01
        do ap0117
        restore scre from wscre01
ENDCASE
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
RETURN
*** 
PROCEDURE subap04
PARAMETERS SELBAR
DO CASE
   CASE SELBAR = 1
        HIDE MENU menuap
        save scre to wscre01
        STORE .F. TO WCONCI
        SET CENT OFF
        DO ap0201
        SET CENT ON
        restore scre from wscre01
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 2
        HIDE MENU menuap
        save scre to wscre01
        DO ap0202
        restore scre from wscre01
        SHOW MENU menuap
   CASE SELBAR = 3
*       HIDE MENU menuap
        save scre to wscre01
        DO ap0203
        restore scre from wscre01
 *      SHOW MENU menuap
   CASE SELBAR = 4
        HIDE MENU menuap
        save scre to wscre01
        DO ap0204
        restore scre from wscre01
        SHOW MENU menuap
   CASE SELBAR = 5
        HIDE MENU menuap
        save scre to wscre01
        DO ap0205
        restore scre from wscre01
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 6
        HIDE MENU menuap
        save scre to wscre01
        DO ap0206
        restore scre from wscre01
        SHOW MENU menuap
   CASE SELBAR = 7
        HIDE MENU menuap
        save scre to wscre01
        DO ap0207
        restore scre from wscre01
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 8
        HIDE MENU menuap
        save scre to wscre01
        DO ap0208
        restore scre from wscre01
        SHOW MENU menuap
   CASE SELBAR = 9
        HIDE MENU menuap
        save scre to wscre01
        DO ap0209
        restore scre from wscre01
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 10
        HIDE MENU menuap
        save scre to wscre01
        DO ap0210
        restore scre from wscre01
        SHOW MENU menuap
   CASE SELBAR = 11
        HIDE MENU menuap
        save scre to wscre01
        DO ap0211
        restore scre from wscre01
        SHOW MENU menuap
ENDCASE
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
RETURN
***
PROCEDURE subap05
PARAMETERS SELBAR
DO CASE
   CASE SELBAR = 2
        save scre to wscre01
        HIDE MENU menuap
        DO ap0302
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 3
        save scre to wscre01
        HIDE MENU menuap
        DO ap0303
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 4
        save scre to wscre01
        HIDE MENU menuap
        DO ap0304
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 5
        save scre to wscre01
        HIDE MENU menuap
        DO ap0305
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 6
        save scre to wscre01
        HIDE MENU menuap
        DO ap0306
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 7
        save scre to wscre01
        HIDE MENU menuap
        DO ap0307
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 8
        save scre to wscre01
        HIDE MENU menuap
        DO ap0308
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 9
        save scre to wscre01
        HIDE MENU menuap
        DO ap0309
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 10
        save scre to wscre01
        HIDE MENU menuap
        DO ap0310
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 11
        save scre to wscre01
        HIDE MENU menuap
        DO ap0311
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 12
        save scre to wscre01
        HIDE MENU menuap
        DO ap0312
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 13
        save scre to wscre01
       *HIDE MENU menuap
        DO ap0313
       *SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 15
        save scre to wscre01
       *HIDE MENU menuap
        DO ap0315
       *SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 16
        save scre to wscre01
        *HIDE MENU menuap
        DO ap0316
        *SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 17
        save scre to wscre01
        HIDE MENU menuap
        DO ap0317
        SHOW MENU menuap
        restore scre from wscre01
   CASE SELBAR = 18
        save scre to wscre01
        HIDE MENU menuap
        DO ap0318
        SHOW MENU menuap
        restore scre from wscre01
ENDCASE
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
RETURN
***
PROCEDURE subap06
PARAMETERS SELBAR
DO CASE
   CASE SELBAR = 1
        save scre to wscre01
        DO ap0401
        restore scre from wscre01
   CASE SELBAR = 2
        save scre to wscre01
        DO ap0402
        restore scre from wscre01
   CASE SELBAR = 3
        save scre to wscre01
        DO ap0403
        restore scre from wscre01
   CASE SELBAR = 4
        save scre to wscre01
        DO ap0404
        restore scre from wscre01
   CASE SELBAR = 5
        save scre to wscre01
        DO ap0405
        restore scre from wscre01
ENDCASE
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
RETURN
***
PROCEDURE subap07
PARAMETERS SELBAR
DO CASE
   CASE SELBAR = 1
        save scre to wscre01
        DO INDICES
        restore scre from wscre01
   CASE SELBAR = 2
        save scre to wscre01
        DO COMPACTA
        restore scre from wscre01
ENDCASE
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
RETURN
***

