*** INICIO CONTROL DE ACCESO
SELECT 20
USE SYSUSERD INDEX SYSUSERD
STORE "BIEMENU " TO WPROGRAMA
STORE SPACE(1)  TO WACCESO
STORE SPACE(1)  TO WFILTRO
DO CHKACC WITH WUSERCODE,WPROGRAMA,WACCESO,WFILTRO
IF WACCESO=" "
   RETURN
ENDIF
USE
*** FIN CONTROL DE ACCESO

SET PROC TO BIEPROC
STORE "Bienes" to wmodulo
@ 01,80-len(wmodulo) say wmodulo

STORE SPACE(01)  TO WPRG
STORE SPACE(03)  TO WTIPGRU
STORE SPACE(03)  TO WTIPSUB
STORE SPACE(03)  TO WTIPREN
STORE SPACE(12)  TO WCODIGO
STORE SPACE(12)  TO WLASTCODIGO
STORE SPACE(03)  TO WGEOGRU
STORE SPACE(03)  TO WGEOSUB
STORE SPACE(03)  TO WGEOREN
STORE SPACE(03)  TO WADMGRU
STORE SPACE(03)  TO WADMSUB
STORE SPACE(03)  TO WADMREN
STORE SPACE(03)  TO WTIPREF
STORE SPACE(03)  TO WINGGRU
STORE SPACE(06)  TO WINGSUB
STORE SPACE(03)  TO WINGTIPREF
STORE SPACE(12)  TO WINGNUMREF
STORE CTOD("  -  -  ") TO WINGFECHA
STORE SPACE(03)  TO WEGRGRU
STORE SPACE(06)  TO WEGRSUB
STORE SPACE(03)  TO WEGRTIPREF
STORE SPACE(12)  TO WEGRNUMREF
STORE CTOD("  -  -  ") TO WEGRFECHA
STORE SPACE(03)  TO WEGRGEOGRU
STORE SPACE(03)  TO WEGRGEOSUB
STORE SPACE(03)  TO WEGRGEOREN

SELECT 1
USE BIETIP INDEX BIETIP
SELECT 2
USE BIEADM INDEX BIEADM
SELECT 3
USE BIEGEO INDEX BIEGEO
SELECT 4
USE BIEING INDEX BIEING
SELECT 5
USE BIEEGR INDEX BIEEGR
SELECT 6
USE BIEREF INDEX BIEREF
SELECT 7
USE BIEBIEN INDEX BIEBIEN1,BIEBIEN2,BIEBIEN3,BIEBIEN4,BIEBIEN5,BIEBIEN6,BIEBIEN7,BIEBIEN8

STORE SPACE(1) TO T7
STORE .T. TO WJUMPING
***************************************
define menu menubie bar at line 3
       define pad padbie00 of menubie prompt "\<Sistema" skip
       define pad padbie01 of menubie prompt "\<Instala" skip
       define pad padbie02 of menubie prompt "\<Modulo " skip
       define pad padbie03 of menubie prompt "\<Tabla  "
       define pad padbie04 of menubie prompt "\<Accion "
       define pad padbie05 of menubie prompt "\<Reporte"
       define pad padbie06 of menubie prompt "\<Proceso"
       define pad padbie07 of menubie prompt "ma\<Ntto."
       define pad padbie08 of menubie prompt "\<Salir  "
       on pad padbie03 of menubie activate popup subbie03
       on pad padbie04 of menubie activate popup subbie04
       on pad padbie05 of menubie activate popup subbie05
       on pad padbie06 of menubie activate popup subbie06
       on pad padbie07 of menubie activate popup subbie07
       on sele pad padbie08 of menubie RETURN
       DEFINE POPUP subbie03 FROM 4,30 shadow
              DEFINE BAR 01 OF subbie03 PROMPT "\<Tipos de bienes           "
              DEFINE BAR 02 OF subbie03 PROMPT "ubicacion \<Administrativa  "
              DEFINE BAR 03 OF subbie03 PROMPT "ubicacion \<Geografica      "
              DEFINE BAR 04 OF subbie03 PROMPT "tipos de \<Ingresos         "
              DEFINE BAR 05 OF subbie03 PROMPT "tipos de \<Egresos          "
              DEFINE BAR 06 OF subbie03 PROMPT "tipos de \<Referencias      "
              DEFINE BAR 07 OF subbie03 PROMPT "\<Ficha de bienes           "
              ON SELECTION POPUP subbie03 DO subbie03 WITH BAR()
       DEFINE POPUP subbie04 FROM 4,40 shadow
              DEFINE BAR 01 OF subbie04 PROMPT "\<Incorporacion             "
              DEFINE BAR 02 OF subbie04 PROMPT "\<Desincorporacion          "
              ON SELECTION POPUP subbie04 DO subbie04 WITH BAR()
       DEFINE POPUP subbie05 FROM 4,50 shadow
              DEFINE BAR 01 OF subbie05 PROMPT "\<Tablas                      "
              DEFINE BAR 02 OF subbie05 PROMPT "\<Incorporaciones             "
              DEFINE BAR 03 OF subbie05 PROMPT "\<Desincorporaciones          "
              DEFINE BAR 04 OF subbie05 PROMPT "bienes x \<Tipo               "
              DEFINE BAR 05 OF subbie05 PROMPT "bienes x ubic. \<Admiistrativa"
              DEFINE BAR 06 OF subbie05 PROMPT "bienes x ubic. \<Geografica   "
              ON SELECTION POPUP subbie05 DO subbie05 WITH BAR()
       DEFINE POPUP subbie06 FROM 4,60 shadow
              ON SELECTION POPUP subbie06 DO subbie06 WITH BAR()
       DEFINE POPUP subbie07 FROM 4,70 shadow
              DEFINE BAR 01 OF subbie07 PROMPT "\<Reorganizar indices       "
              DEFINE BAR 02 OF subbie07 PROMPT "\<Compactar tablas de datos "
              ON SELECTION POPUP subbie07 DO subbie07 WITH BAR()
*** ACTIVACION DEL MENU
do while .t.
   ACTIVATE MENU menubie 
enddo
RELEASE menubie
RETURN

******************
PROCEDURE subbie03
******************
PARAMETERS SELBAR
save scre to wscre01
hide menu menubie
push key clear
@ 4,0 CLEAR

DO CASE
   CASE SELBAR = 1
        store 04 to wlin01
        store 00 to wcol01
        do bie0101
   CASE SELBAR = 2
        store 04 to wlin02
        store 00 to wcol02
        do bie0102
   CASE SELBAR = 3
        store 04 to wlin03
        store 00 to wcol03
        do bie0103
   CASE SELBAR = 4
        store 04 to wlin04
        store 00 to wcol04
        do bie0104
   CASE SELBAR = 5
        store 04 to wlin05
        store 00 to wcol05
        do bie0105
   CASE SELBAR = 6
        store 04 to wlin06
        store 00 to wcol06
        do bie0106
   CASE SELBAR = 7
        SAVE SCRE TO WSCREBIE
        HIDE MENU MENUBIE
        store 04 to wlin07
        store 00 to wcol07
        store "FICHA" to wprg
        do BIEBIEN
        SHOW MENU MENUBIE
        RESTORE SCRE FROM WSCREBIE
ENDCASE
pop key
show menu menubie
restore scre from wscre01
RETURN

******************
PROCEDURE subbie04
******************
PARAMETERS SELBAR
push key clear
DO CASE
   CASE SELBAR = 1
        save scre to wscrebie
        hide menu menubie
        STORE "INGRESO" TO WPRG
        DO BIEBIEN
        SHOW menu menubie
        RESTORE scre FROM wscrebie
   CASE SELBAR = 2
        save scre to wscrebie
        hide menu menubie
        STORE "EGRESO" TO WPRG
        STORE SPACE(12) TO WLASTCODIGO
        DO BIEBIEN
        SHOW menu menubie
        RESTORE scre FROM wscrebie
ENDCASE
pop key
show menu menubie
restore scre from wscrebie
RETURN

******************
PROCEDURE subbie05
******************
PARAMETERS SELBAR
save scre to wscre03
hide menu menubie
push key clear
DO CASE
   CASE SELBAR = 1
        DO bie0301
   CASE SELBAR = 2
        DO bie0302
   CASE SELBAR = 3
        DO bie0303
   CASE SELBAR = 4
        DO bie0304
   CASE SELBAR = 5
        DO bie0305
   CASE SELBAR = 6
        DO bie0306
ENDCASE
RESTORE scre FROM WScre03
SHOW menu menubie
push key clear
RETURN

******************
PROCEDURE subbie06
******************
PARAMETERS SELBAR
save scre to wscre01
hide menu menubie
push key clear
DO CASE
   CASE SELBAR = 1
        DO conrenu
   CASE SELBAR = 2
        DO conreco
ENDCASE
save scre to wscre01
hide menu menubie
push key clear
RETURN

******************
PROCEDURE subbie07
******************
PARAMETERS SELBAR
save scre to wscre01
hide menu menubie
push key clear
DO CASE
   CASE SELBAR = 1
        DO INDICES
   CASE SELBAR = 2
        DO COMPACTA
ENDCASE
save scre to wscre01
hide menu menubie
push key clear
RETURN
***



