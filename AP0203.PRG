SAVE SCRE TO SCRE3
store .t. to wsub3
do while wsub3
   RESTORE SCREEN FROM SCRE3
   @ 05,29 clear to 10,52
   @ 06,29 to 10,52
   @ 05,30 SAY    "  VARIACION GENERAL   "
   @ 07,30 PROMPT "Incluir concepto      "
   @ 08,30 PROMPT "Modificar concepto    "
   @ 09,30 PROMPT "Eliminar concepto     "
   MENU TO WSUB0203
   DO CASE
   CASE WSUB0203 = 1
        DO AP0203A
   CASE WSUB0203 = 2
        DO AP0203B
   CASE WSUB0203 = 3
        DO AP0203C
   CASE WSUB0203 = 0
        EXIT
   ENDCASE
enddo
**********************
PROC AP0203A
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 5
use appaggen index appagge1,appagge2
select 6
use appagcon index appagco1,appagco2
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
store 0        to wcantidad
@ 04,0 say "INCLUSION GENERAL DE CONCEPTOS"
@ 05,0 clear to 12,60
@ 05,0 to 12,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
@ 10,01 say "CANTIDAD          :"
@ 11,01 say "RECALCULAR SOBRES?:"
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
   if wtipper="O".or.wtipper="E".or. wtipper="J".or.wtipper="P"
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
store "INDIQUE SI DESEA RECALCULAR TODOS LOS SOBRES AFECTADOS (S/N)" to tex
store "NS" to wch
do pregunta
store wch to wrecalsob
@ 11,20 say wrecalsob
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 5
go top
do while .not. eof()
   if tipo <> wtipper
      select 5
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 5
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 5
      skip
      loop
   endif
   store cedula  to xcedula
   select 7
   find &xcedula
   if eof()
      select 5
      skip
      loop
   else
      store "PROCESANDO CEDULA "+cedula+" "+rtrim(apellidos)+" "+rtrim(nombres) to mes
      do mensaje with mes
   endif
   select 5
   store fecha1  to wfecha1
   store fecha2  to wfecha2
   store recno() to wstart
   store .f.     to wflagcon
   select 6
   find &xcedula
   do while .not. eof() .and. cedula = xcedula
      if concepto = wqconcepto
         store .t. to wflagcon
         exit
      endif
      select 6
      skip
   enddo
   if .not. wflagcon
      do filloc
      append blank
      unlock 
      do recloc
      replace cedula   with xcedula
      replace concepto with wqconcepto
      replace fecha1   with wfecha1
      replace fecha2   with wfecha2
      unlock
   endif
   do recloc
   replace cantidad with wcantidad 
   unlock 
   if wrecalsob = "S"
      *** RUTINA DE RECALCULO DEL SOBRE
      STORE 0 TO WTOTBON
      STORE 0 TO WTOTNBON
      STORE 0 TO WTOTASI
      STORE 0 TO WTOTDEC
      STORE 0 TO WTOTLIQ
      DO SOBRECAL
      unlock
   endif
   select 5
   go wstart
   skip
enddo
close data
close index
return
***********************
PROC AP0203B
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 5
use appaggen index appagge1,appagge2
select 6
use appagcon index appagco1,appagco2
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
store 0        to wcantidad
store 0        to wporcentaje
@ 04,0 say "MODIFICACION GENERAL DE CONCEPTOS"
@ 05,0 clear to 13,60
@ 05,0 to 13,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
@ 10,01 say "CANTIDAD          :"
@ 11,01 say "PORCENTAJE DE VAR.:"
@ 12,01 say "RECALCULAR SOBRES?:" 
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
   if wtipper="O".or.wtipper="E".or.wtipper="J".or.wtipper="P"
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
store "INDIQUE SI DESEA RECALCULAR TODOS LOS SOBRES AFECTADOS (S/N)" to tex
store "NS" to wch
do pregunta
store wch to wrecalsob
@ 12,20 say wrecalsob
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 5
go top
do while .not. eof()
   if tipo <> wtipper
      select 5
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 5
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 5
      skip
      loop
   endif
   store cedula  to xcedula
   select 7
   find &xcedula
   if eof()
      select 5
      skip
      loop
   else
      store "PROCESANDO CEDULA "+cedula+" "+rtrim(apellidos)+" "+rtrim(nombres) to mes
      do mensaje with mes
   endif
   select 5
   store recno() to wstart
   store .f.     to wflagcon
   select 6
   find &xcedula
   do while .not. eof() .and. cedula = xcedula
      if wqconcepto<>space(4).and.wqconcepto<>concepto
         select 6
         skip
         loop
      else
         do recloc
         if wtipvar = "D"
            replace monto    with (monto/cantidad)*wcantidad
            replace cantidad with wcantidad 
         else
            store (cantidad*wporcentaje)/100 to wcantidad
            replace monto    with (monto/cantidad)*wcantidad
            replace cantidad with wcantidad
         endif
         unlock 
         if wqconcepto <> space(4)
            exit
         endif
      endif
      select 6
      skip
   enddo
   if wrecalsob = "S"
      *** RUTINA DE RECALCULO DEL SOBRE
      STORE 0 TO WTOTBON
      STORE 0 TO WTOTNBON
      STORE 0 TO WTOTASI
      STORE 0 TO WTOTDEC
      STORE 0 TO WTOTLIQ
      DO SOBRECAL
      unlock
   endif
   select 5
   go wstart
   skip
enddo
close data
close index
return
*****************
PROC AP0203C
select 1
use apgrupos index apgrupos
select 2
use apnomina index apnomina
select 3
use apcon index apcon
select 4
use apconper index apconper
select 5
use appaggen index appagge1,appagge2
select 6
use appagcon index appagco1,appagco2
select 7 
use apperson index apperso1
store space(1) to wtipper
store space(2) to wgrupo
store space(2) to wnomina
store space(4) to wqconcepto
@ 04,0 say "ELIMINACION GENERAL DE CONCEPTOS"
@ 05,0 clear to 11,60
@ 05,0 to 11,60
@ 06,01 say "TIPO DE PERSONAL  :"
@ 07,01 SAY "GRUPO DE NOMINAS  :"
@ 08,01 SAY "CODIGO DE NOMINA  :"
@ 09,01 SAY "CODIGO DE CONCEPTO:"
@ 10,01 say "RECALCULAR SOBRES?:"
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
   if wtipper="O".or.wtipper="E".or.wtipper="J".or.wtipper="P"
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
store "INDIQUE SI DESEA RECALCULAR TODOS LOS SOBRES AFECTADOS (S/N)" to tex
store "NS" to wch
do pregunta
store wch to wrecalsob
@ 10,20 say wrecalsob
store "OPCIONES: (C)ONTINUAR, (S)ALIR" to tex
store "SC" to wch
do pregunta
if wch = "S"
   close data
   close index
   return
endif
select 5
go top
do while .not. eof()
   if tipo <> wtipper
      select 5
      skip
      loop
   endif
   if wgrupo <> space(2).and.wgrupo<>grupo
      select 5
      skip
      loop
   endif
   if wnomina<>space(2).and.wnomina<>nomina 
      select 5
      skip
      loop
   endif
   store cedula  to xcedula
   select 7
   find &xcedula
   if eof()
      select 5
      skip
      loop
   else
      store "PROCESANDO CEDULA "+cedula+" "+rtrim(apellidos)+" "+rtrim(nombres) to mes
      do mensaje with mes
   endif
   select 5
   store recno() to wstart
   store .f.     to wflagcon
   select 6
   find &xcedula
   do while .not. eof() .and. cedula = xcedula
      if concepto = wqconcepto
         do recloc
         DELETE 
         unlock 
         if wrecalsob = "S"
            *** RUTINA DE RECALCULO DEL SOBRE
            STORE 0 TO WTOTBON
            STORE 0 TO WTOTNBON
            STORE 0 TO WTOTASI
            STORE 0 TO WTOTDEC
            STORE 0 TO WTOTLIQ
            DO SOBRECAL
            unlock
         endif
         exit
      endif
      select 6
      skip
   enddo
   select 5
   go wstart
   skip
enddo
close data
close index
return
***************
