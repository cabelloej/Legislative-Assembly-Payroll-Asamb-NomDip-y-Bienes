SAVE SCRE TO SCRE3
store .t. to wsub3
do while wsub3
   RESTORE SCREEN FROM SCRE3
   @ 04,19 clear to 09,42
   @ 04,19 to 09,42
   @ 05,20 PROMPT "Incluir concepto      "
   @ 06,20 PROMPT "Modificar cantidad    "
   @ 07,20 PROMPT "Modificar factor      "
   @ 08,20 PROMPT "Eliminar concepto     "
   MENU TO WSUB0203
   DO CASE
   CASE WSUB0203 = 1
        DO AP0115A
   CASE WSUB0203 = 2
        DO AP0115B
   CASE WSUB0203 = 3
        DO AP0115C
   CASE WSUB0203 = 4
        DO AP0115d
   CASE WSUB0203 = 0
        EXIT
   ENDCASE
enddo
***************************
PROCEDURE AP0115A
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
store 0        to wcantidad
@ 04,0 say "INCLUSION GENERAL DE CONCEPTOS EN CONCEPTOS NOMINALES"
@ 05,0 clear to 13,60
@ 05,0 to 13,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
@ 10,01 say "CANTIDAD          :"
@ 11,01 say "FACTOR            :"
@ 12,01 SAY "MINIMO Y MAXIMO   :"
store .t. to wgettip
do while wgettip
   store "INDIQUE PERSONAL (O=OBRERO, E=EMPLEADO, P=PENSIONADO, J=JUBILADO)" TO MES
   do mensaje with mes
   @ 06,20 get wtipper picture "!"
   read
   if readkey()=12.or.readkey()=268.or.wtipper=space(2)
      close data
      close index
      return
   endif
   if wtipper="O".or.wtipper="E".or.wtipper="P".or.wtipper="J"
      exit
   endif
enddo
store .t. to wgetgru
do while wgetgru
   store "INDIQUE EL GRUPO DE NOMINA" TO MES
   do mensaje with mes
   @ 07,20 get wgrupo  
   read
   if readkey()=12.or.readkey()=268
      close data
      close index
      return
   endif
   if wgrupo <> space(2)
      select 1
      find &wgrupo
      if eof()
         store "GRUPO NO REGISTRADO, VERIFIQUE" to mes
         do aviso with mes
         loop
      else
         store descri to wdesgrupo
         @ 07,26 say wdesgrupo
         @ 08,20 get wnomina
         read
         if readkey()=12.or.readkey()=268
            loop
         endif
         if wnomina <> space(2)
            store wgrupo+wnomina to wclave
            select 2
            find &wclave
            if eof()
               store "NOMINA NO REGISTRADA PARA ESTE GRUPO, VERIFIQUE" to mes
               do aviso with mes
               loop
            else
               store descri to wdesnomi
               @ 08,26 say wdesnomi
               exit
            endif
         else
            store "TODAS LAS NOMINAS DEL GRUPO" to wdesnomi
            @ 08,26 say wdesnomi
            exit
         endif
      endif 
   else
      store "TODOS LOS GRUPOS"  to wdesgrupo
      store "TODAS LAS NOMINAS DE LOS GRUPOS" to wdesnomi  
      @ 07,26 say wdesgrupo
      @ 08,26 say wdesnomi
      exit
   endif
enddo
store .t. to wgetcon
do while wgetcon
   store "INDIQUE EL CODIGO DE CONCEPTO" TO MES
   do mensaje with mes
   @ 09,20 get wqconcepto           
   read
   if readkey()=12.or.readkey()=268.or.wqconcepto=space(4)
      close data
      close index
      return      
   endif
   select 3
   find &wqconcepto
   if eof()
      store "CONCEPTO NO REGISTRADO, VERIFIQUE" to mes
      do aviso with mes
      loop
   else
      @ 09,26 say descri
      store factor to wfactor
      store minimo to wminimo
      store maximo to wmaximo
      exit
   endif
enddo
store .t. to wgetcan
do while wgetcan
   store "INDIQUE EL NUMERO DE UNIDADES A INGRESAR POR PERSONA" to mes
   do mensaje with mes
   @ 10,20 get wcantidad picture "#######.##" 
   read
   if wcantidad > 0
      exit
   endif
enddo
store .t. to wgetfac
do while wgetfac
   store "INDIQUE EL FACTOR DE CALCULO DEL CONCEPTO" to mes
   do mensaje with mes
   @ 11,20 get wfactor picture "#######.##" 
   read
   if wfactor > 0
      exit
   endif
enddo
store .t. to wgetmm
do while wgetmm
   store "INDIQUE EL MINIMO Y MAXIMO A CALCULAR DEL CONCEPTO" to mes
   do mensaje with mes
   @ 12,20 get wminimo picture "#######.##" 
   @ 12,35 get wmaximo picture "#######.##" 
   read
   if wmaximo <= wminimo
      store "ERROR EN DEFINICION DE MINIMO Y MAXIMO" TO MES
      do aviso with mes
   else
      exit
   endif
enddo
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 7
go top
do while .not. eof()
   if tipo <> wtipper
      select 7
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 7
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 7
      skip
      loop
   endif
   store cedula             to xcedula
   store xcedula+wqconcepto to wclavecon
   select 4
   find &wclavecon
   if eof() 
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula   with xcedula
      replace concepto with wqconcepto
      replace cantidad with wcantidad
      replace factor   with wfactor
      replace minimo   with wminimo
      replace maximo   with wmaximo
      unlock
      *** RUTINA DE RECALCULO DE CONCEPTOS PERSONALES
      STORE 0 TO WTOTBON
      STORE 0 TO WTOTNBON
      STORE 0 TO WTOTASI
      STORE 0 TO WTOTDEC
      STORE 0 TO WTOTLIQ
      *STORE WCEDULA TO XCEDULA
      DO CONCECAL
   endif
   select 7
   skip
enddo
close data
close index
return
******************************
PROCEDURE AP0115B
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
store 0        to wcantidad
store 0        to wporcentaje
@ 04,0 say "MODIFICACION GENERAL DE CANTIDAD POR CONCEPTO EN CONCEPTOS NOMINALES"
@ 05,0 clear to 12,60
@ 05,0 to 12,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
@ 10,01 say "CANTIDAD          :"
@ 11,01 say "PORCENTAJE DE VAR.:"
store .t. to wgettip
do while wgettip
   store "INDIQUE PERSONAL (O=OBRERO, E=EMPLEADO, P=PENSIONADO, J=JUBILADO)" TO MES
   do mensaje with mes
   @ 06,20 get wtipper picture "!"
   read
   if readkey()=12.or.readkey()=268.or.wtipper=space(2)
      close data
      close index
      return
   endif
   if wtipper="O".or.wtipper="E".or.wtipper="P".or.wtipper="J"
      exit
   endif
enddo
store .t. to wgetgru
do while wgetgru
   store "INDIQUE EL GRUPO DE NOMINA" TO MES
   do mensaje with mes
   @ 07,20 get wgrupo  
   read
   if readkey()=12.or.readkey()=268
      close data
      close index
      return
   endif
   if wgrupo <> space(2)
      select 1
      find &wgrupo
      if eof()
         store "GRUPO NO REGISTRADO, VERIFIQUE" to mes
         do aviso with mes
         loop
      else
         store descri to wdesgrupo
         @ 07,26 say wdesgrupo
         @ 08,20 get wnomina
         read
         if readkey()=12.or.readkey()=268
            loop
         endif
         if wnomina <> space(2)
            store wgrupo+wnomina to wclave
            select 2
            find &wclave
            if eof()
               store "NOMINA NO REGISTRADA PARA ESTE GRUPO, VERIFIQUE" to mes
               do aviso with mes
               loop
            else
               store descri to wdesnomi
               @ 08,26 say wdesnomi
               exit
            endif
         else
            store "TODAS LAS NOMINAS DEL GRUPO" to wdesnomi
            @ 08,26 say wdesnomi
            exit
         endif
      endif 
   else
      store "TODOS LOS GRUPOS"  to wdesgrupo
      store "TODAS LAS NOMINAS DE LOS GRUPOS" to wdesnomi  
      @ 07,26 say wdesgrupo
      @ 08,26 say wdesnomi
      exit
   endif
enddo
store .t. to wgetcon
do while wgetcon
   store "INDIQUE EL CODIGO DE CONCEPTO, <ENTER>=TODOS LOS CONCEPTOS" TO MES
   do mensaje with mes
   @ 09,20 get wqconcepto           
   read
   if readkey()=12.or.readkey()=268
      close data
      close index
      return      
   endif
   if wqconcepto <> space(4)
      select 3
      find &wqconcepto
      if eof()
         store "CONCEPTO NO REGISTRADO, VERIFIQUE" to mes
         do aviso with mes
         loop
      else
         store descri to wconcepdes
         @ 09,26 say wconcepdes
         exit
      endif
   else
      store "TODOS LOS CONCEPTOS" to wconcepdes
      @  09,26 say wconcepdes
      exit
   endif
enddo
store "INDIQUE EL TIPO DE VARIACION: (D)IRECTA, (P)ORCENTUAL" TO TEX
store "DP" to wch
do pregunta
store wch to wtipvar
if wtipvar = "D"
   store .t. to wgetcan
   do while wgetcan
      store "INDIQUE EL NUMERO DE UNIDADES A INGRESAR POR PERSONA" to mes
      do mensaje with mes
      @ 10,20 get wcantidad picture "#######.##" 
      read
      if wcantidad > 0
         exit
      endif
   enddo
else
   store .t. to wgetpor
   do while wgetpor
      store "INDIQUE EL % DE VARIACION (ANTEPONGA SIGNO NEG. PARA REBAJAR)" to mes
      do mensaje with mes
      @ 11,20 get wporcentaje picture "####.##" 
      read
      if wporcentaje <> 0
         exit
      endif
   enddo
endif
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 7
go top
do while .not. eof()
   if tipo <> wtipper
      select 7
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 7
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 7
      skip
      loop
   endif
   store cedula  to xcedula
   select 4
   find &xcedula
   do while .not. eof() .and. cedula = xcedula
      if wqconcepto<>space(4).and.wqconcepto<>concepto
         select 4
         skip
         loop
      else
         do recloc
         if wtipvar = "D"
            replace cantidad with wcantidad 
         else
            replace cantidad with (cantidad*wporcentaje)/100
         endif
         *** RUTINA DE RECALCULO DE CONCEPTOS PERSONALES
         STORE 0 TO WTOTBON
         STORE 0 TO WTOTNBON
         STORE 0 TO WTOTASI
         STORE 0 TO WTOTDEC
         STORE 0 TO WTOTLIQ
         STORE WCEDULA TO XCEDULA
         DO CONCECAL
         unlock 
         if wqconcepto <> space(4)
            exit
         endif
      endif
      select 4
      skip
   enddo
   select 7
   skip
enddo
close data
close index
return
*********************
PROCEDURE AP0115C
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
store 0        to wfactor  
@ 04,0 say "MODIFICACION GENERAL DE FACTORES DE CONCEPTOS EN CONCEPTOS NOMINALES"
@ 05,0 clear to 12,60
@ 05,0 to 12,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
@ 10,01 say "FACTOR DE CALCULO :"
store .t. to wgettip
do while wgettip
   store "INDIQUE PERSONAL (O=OBRERO, E=EMPLEADO, P=PENSIONADO, J=JUBILADO)" TO MES
   do mensaje with mes
   @ 06,20 get wtipper picture "!"
   read
   if readkey()=12.or.readkey()=268.or.wtipper=space(2)
      close data
      close index
      return
   endif
   if wtipper="O".or.wtipper="E".or.wtipper="P".or.wtipper="J"
      exit
   endif
enddo
store .t. to wgetgru
do while wgetgru
   store "INDIQUE EL GRUPO DE NOMINA" TO MES
   do mensaje with mes
   @ 07,20 get wgrupo  
   read
   if readkey()=12.or.readkey()=268
      close data
      close index
      return
   endif
   if wgrupo <> space(2)
      select 1
      find &wgrupo
      if eof()
         store "GRUPO NO REGISTRADO, VERIFIQUE" to mes
         do aviso with mes
         loop
      else
         store descri to wdesgrupo
         @ 07,26 say wdesgrupo
         @ 08,20 get wnomina
         read
         if readkey()=12.or.readkey()=268
            loop
         endif
         if wnomina <> space(2)
            store wgrupo+wnomina to wclave
            select 2
            find &wclave
            if eof()
               store "NOMINA NO REGISTRADA PARA ESTE GRUPO, VERIFIQUE" to mes
               do aviso with mes
               loop
            else
               store descri to wdesnomi
               @ 08,26 say wdesnomi
               exit
            endif
         else
            store "TODAS LAS NOMINAS DEL GRUPO" to wdesnomi
            @ 08,26 say wdesnomi
            exit
         endif
      endif 
   else
      store "TODOS LOS GRUPOS"  to wdesgrupo
      store "TODAS LAS NOMINAS DE LOS GRUPOS" to wdesnomi  
      @ 07,26 say wdesgrupo
      @ 08,26 say wdesnomi
      exit
   endif
enddo
store .t. to wgetcon
do while wgetcon
   store "INDIQUE EL CODIGO DE CONCEPTO" TO MES
   do mensaje with mes
   @ 09,20 get wqconcepto           
   read
   if wqconcepto=space(4).or.readkey()=12.or.readkey()=268
      close data
      close index
      return      
   endif
   select 3
   find &wqconcepto
   if eof()
      store "CONCEPTO NO REGISTRADO, VERIFIQUE" to mes
      do aviso with mes
      loop
   else
      store descri to wconcepdes
      @ 09,26 say wconcepdes
      exit
   endif
enddo
store .t. to wgetfac
do while wgetfac
   store "INDIQUE EL FACTOR DE CALCULO DEL CONCEPTO" to mes
   do mensaje with mes
   @ 10,20 get wfactor picture "########.###" 
   read
   if wfactor > 0
      exit
   endif
enddo
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 7
go top
do while .not. eof()
   if tipo <> wtipper
      select 7
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 7
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 7
      skip
      loop
   endif
   store cedula  to xcedula
   select 4
   find &xcedula
   do while .not. eof() .and. cedula = xcedula
      if wqconcepto<>concepto
         select 4
         skip
         loop
      else
         do recloc
         replace factor  with wfactor 
         *** RUTINA DE RECALCULO DE CONCEPTOS PERSONALES
         STORE 0 TO WTOTBON
         STORE 0 TO WTOTNBON
         STORE 0 TO WTOTASI
         STORE 0 TO WTOTDEC
         STORE 0 TO WTOTLIQ
         *STORE CEDULA TO XCEDULA
         DO CONCECAL
         unlock 
         exit
      endif
      select 4
      skip
   enddo
   select 7
   skip
enddo
close data
close index
return
*************************
PROCEDURE AP0115D
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
@ 04,0 say "ELIMINACION GENERAL DE CONCEPTOS EN CONCEPTOS NOMINALES"
@ 05,0 clear to 10,60
@ 05,0 to 10,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
store .t. to wgettip
do while wgettip
   store "INDIQUE PERSONAL (O=OBRERO, E=EMPLEADO,P=PENSIONADO, J=JUBILADO)" TO MES
   do mensaje with mes
   @ 06,20 get wtipper picture "!"
   read
   if readkey()=12.or.readkey()=268.or.wtipper=space(2)
      close data
      close index
      return
   endif
   if wtipper="O".or.wtipper="E".or.wtipper="P".or.wtipper="J"
      exit
   endif
enddo
store .t. to wgetgru
do while wgetgru
   store "INDIQUE EL GRUPO DE NOMINA" TO MES
   do mensaje with mes
   @ 07,20 get wgrupo  
   read
   if readkey()=12.or.readkey()=268
      close data
      close index
      return
   endif
   if wgrupo <> space(2)
      select 1
      find &wgrupo
      if eof()
         store "GRUPO NO REGISTRADO, VERIFIQUE" to mes
         do aviso with mes
         loop
      else
         store descri to wdesgrupo
         @ 07,26 say wdesgrupo
         @ 08,20 get wnomina
         read
         if readkey()=12.or.readkey()=268
            loop
         endif
         if wnomina <> space(2)
            store wgrupo+wnomina to wclave
            select 2
            find &wclave
            if eof()
               store "NOMINA NO REGISTRADA PARA ESTE GRUPO, VERIFIQUE" to mes
               do aviso with mes
               loop
            else
               store descri to wdesnomi
               @ 08,26 say wdesnomi
               exit
            endif
         else
            store "TODAS LAS NOMINAS DEL GRUPO" to wdesnomi
            @ 08,26 say wdesnomi
            exit
         endif
      endif 
   else
      store "TODOS LOS GRUPOS"  to wdesgrupo
      store "TODAS LAS NOMINAS DE LOS GRUPOS" to wdesnomi  
      @ 07,26 say wdesgrupo
      @ 08,26 say wdesnomi
      exit
   endif
enddo
store .t. to wgetcon
do while wgetcon
   store "INDIQUE EL CODIGO DE CONCEPTO" TO MES
   do mensaje with mes
   @ 09,20 get wqconcepto           
   read
   if readkey()=12.or.readkey()=268.or.wqconcepto=space(4)
      close data
      close index
      return      
   endif
   select 3
   find &wqconcepto
   if eof()
      store "CONCEPTO NO REGISTRADO, VERIFIQUE" to mes
      do aviso with mes
      loop
   else
      @ 09,26 say descri
      exit
   endif
enddo
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 7
go top
do while .not. eof()
   if tipo <> wtipper
      select 7
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 7
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 7
      skip
      loop
   endif
   store cedula  to xcedula
   store xcedula+wqconcepto to wclavecon
   select 4
   find &wclavecon
   if .not. eof()
      do recloc
      DELETE 
      unlock 
   endif
   select 7
   skip
enddo
close data
close index
return
