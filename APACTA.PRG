STORE .F. TO FLAGDEL                                         
SET COLOR TO W+/BG
@ 4,0 CLEAR                                                  
@ 6,0 TO 19,79                                               
SET COLOR TO W+/B
@ 4,40 - (LEN(WHEADER)/2) SAY WHEADER
SET COLOR TO B/W+
@ 5,01 SAY "CODIGO"                                          
@ 5,32 SAY "DESCRIPCION"                                     
STORE  "INCLUIR    " TO XMENU1                               
STORE  "MODIFICAR  " TO XMENU2                               
STORE  "ELIMINAR   " TO XMENU3                               
STORE  "SELECCIONAR" TO XMENU4                              
STORE  "BUSCAR     " TO XMENU5                              
STORE 4 TO WMENU                                            
STORE .T. TO XFLAG                                          
STORE .F. TO WREGS                                          
GO TOP                                                      
DO WHILE .NOT. EOF()                                        
   STORE .T. TO WREGS                                       
   EXIT                                                     
ENDDO                                                       
IF .NOT. WREGS                                              
   DO APACTAI
ENDIF                                       
GO TOP                                      
STORE 12 TO WRANGO                          
STORE RECNO() TO WINICIO                    
STORE 0 TO WREVERSE                         
STORE 0 TO WEDILIN                          
STORE .T. TO FLAGJUMP                       
STORE .T. TO WEDITAR                        
DO WHILE WEDITAR                            
   IF FLAGJUMP                              
      STORE 0       TO WCONT                
      STORE 07      TO WLINE                
      GO WINICIO                            
      SET COLOR TO W+/BG
      DO WHILE .NOT. EOF() .AND. WCONT < WRANGO                   
         STORE WLINE+WCONT TO WSCRELIN                            
         STORE WLINE+WCONT TO WLASTLIN                            
         STORE RECNO() TO WLASTREC                                
         @ WSCRELIN ,01  SAY CODIGO                               
         @ WSCRELIN ,30  SAY DESCRI                               
         STORE WCONT + 1 TO WCONT                                 
         SKIP                                                     
      ENDDO                                                       
      STORE .F. TO FLAGJUMP                                       
   ENDIF                                                          
   *** RELLENO DE ESP. VACION (SI NO HAY SUF. REGISTROS)          
   STORE WCONT TO WFILCON                                         
   STORE WLASTLIN TO WFILLIN                                      
   SET COLOR TO W+/BG
   DO WHILE WFILCON < WRANGO                                      
      STORE WFILLIN + 1 TO WFILLIN                                
      @ WFILLIN, 1 SAY SPACE(78)
      STORE WFILCON + 1 TO WFILCON                                
   ENDDO                                                          
   ***                                                            
   IF WREVERSE = 0                                                
      STORE WLASTREC TO WREVERSE                                  
      STORE WLASTLIN TO WEDILIN                                   
      STORE WINICIO  TO WCLEARREC                                 
      STORE WLINE    TO WCLEARLIN                                 
   ENDIF                                                          
   ** PARA BORRAR EL REGISTRO ANTERIOR                            
   SET COLOR TO W+/BG
   GO WCLEARREC                                                   
   STORE WCLEARLIN TO WSCRELIN                                    
   @ WSCRELIN ,01  SAY CODIGO                                     
   @ WSCRELIN ,30  SAY DESCRI                                     
   ** PARA MARCAR EL REGISTRO ACTUAL                              
   SET COLOR TO B+/W
   GO WREVERSE                                                    
   STORE WEDILIN TO WSCRELIN                                      
   @ WSCRELIN ,01  SAY CODIGO                                     
   @ WSCRELIN ,30  SAY DESCRI                                     
   STORE WREVERSE TO WCLEARREC                                    
   STORE WSCRELIN TO WCLEARLIN                                    
   *** PARA INDICAR LA ROTACION DEL MENU (SI LA HUBO)             
   IF XFLAG                                                       
      SET COLOR TO W+/B
      @ 20,05 SAY  XMENU1                                         
      @ 20,20 SAY  XMENU2                                         
      @ 20,35 SAY  XMENU3                                         
      @ 20,50 SAY  XMENU4                                         
      @ 20,65 SAY  XMENU5                                         
      SET COLOR TO B+/W
      STORE "XMENU"+STR(WMENU,1) TO WMACRO                        
      @ 20,5+((WMENU-1)*15)  SAY &WMACRO                          
      STORE .F. TO XFLAG                                          
   ENDIF                                                          
   *** CAPTURA DE INTERACTIVIDAD                                  
   CLEAR TYPEAHEAD                                                
   STORE INKEY(0) TO WMUEVE                                       
   *** PAGE DOWN                                                  
   IF WMUEVE = 3                                                  
      GO WINICIO                                                  
      SKIP +12                                                    
      IF .NOT. EOF()                                              
         STORE RECNO() TO WINICIO                                 
         STORE .T.     TO FLAGJUMP                                
         STORE WINICIO TO WREVERSE                                
         STORE WLINE   TO WEDILIN                                 
         STORE WINICIO TO WCLEARREC                               
         STORE WLINE   TO WCLEARLIN                               
      ENDIF                                                       
   ENDIF                                                          
   *** PAGE UP                                                    
   IF WMUEVE =  18                                                
      GO WINICIO                                                  
      SKIP -12                                                    
      STORE RECNO() TO WINICIO                                    
      STORE .T. TO FLAGJUMP                                       
      STORE 0 TO WREVERSE                                         
   ENDIF                                                          
   *** HOME                                                       
   IF WMUEVE = 1                                                  
      GO TOP                                                      
      IF .NOT. EOF()                                              
         STORE RECNO() TO WINICIO                                 
         STORE .T. TO FLAGJUMP                                    
         STORE WINICIO TO WREVERSE                                
         STORE WLINE   TO WEDILIN                                 
         STORE WINICIO TO WCLEARREC                               
         STORE WLINE   TO WCLEARLIN                               
      ENDIF                                                       
   ENDIF                                                          
   *** END                                                        
   IF WMUEVE = 6                                                  
      GO BOTT                                                     
      SKIP -11                                                    
      STORE RECNO() TO WINICIO                                    
      STORE 0 TO WREVERSE                                         
      STORE .T. TO FLAGJUMP                                       
   ENDIF                                                          
   *** DOWN ARROW                                                 
   IF WMUEVE = 24                                                 
      SKIP + 1                                                    
      IF WEDILIN < (WLINE+(WCONT-1))                              
         STORE WREVERSE    TO WCLEARREC                        
         STORE RECNO()     TO WREVERSE                         
         STORE WEDILIN     TO WCLEARLIN                        
         STORE WEDILIN + 1 TO WEDILIN                          
      ELSE                                                     
         IF .NOT. EOF()                                        
            GO WINICIO                                         
            SKIP +1                                            
            STORE RECNO() TO WINICIO                           
            STORE .T.     TO FLAGJUMP                          
            STORE 0       TO WREVERSE                          
         *  STORE WINICIO TO WREVERSE                          
         *  STORE WLINE   TO WEDILIN                           
         *  STORE WINICIO TO WCLEARREC                         
         *  STORE WLINE   TO WCLEARLIN                         
         ELSE                                                  
            GO WINICIO                                         
            STORE RECNO() TO WINICIO                           
            STORE .T.     TO FLAGJUMP                          
            STORE 0       TO WREVERSE                          
         *  STORE WINICIO TO WREVERSE                          
         *  STORE WLINE   TO WEDILIN                           
         *  STORE WINICIO TO WCLEARREC                         
         *  STORE WLINE   TO WCLEARLIN                         
         ENDIF                                                 
      ENDIF                                                    
   ENDIF                                                       
   *** UP ARROW                                                
   IF WMUEVE = 5                                               
      SKIP -1                                                  
      IF WEDILIN > WLINE                                       
         STORE WREVERSE    TO WCLEARREC                        
         STORE RECNO()     TO WREVERSE                         
         STORE WEDILIN     TO WCLEARLIN                        
         STORE WEDILIN - 1 TO WEDILIN                          
      ELSE                                                     
         GO WINICIO                                            
         SKIP -1                                               
         STORE RECNO() TO WINICIO                              
         STORE .T.     TO FLAGJUMP                             
         STORE WINICIO TO WREVERSE                             
         STORE WLINE   TO WEDILIN                              
         STORE WINICIO TO WCLEARREC                            
         STORE WLINE   TO WCLEARLIN                            
      ENDIF                                                    
   ENDIF                                                       
   *** RIGHT ARROW                                             
   IF WMUEVE = 4                                               
      STORE .T. TO XFLAG                                       
      STORE WMENU+1 TO WMENU                                   
      IF WMENU > 5                                             
         STORE 1 TO WMENU                                      
      ENDIF                                                    
   ENDIF                                                       
   *** LEFT ARROW                                              
   IF WMUEVE = 19                                              
      STORE .T. TO XFLAG                                       
      STORE WMENU-1 TO WMENU                                   
      IF WMENU < 1                                             
         STORE 5 TO WMENU                                      
      ENDIF                                                    
   ENDIF                                                       
   *** ESCAPE                                                  
   IF WMUEVE = 27                                              
      STORE CODIGO TO WSEL 
      SET COLOR TO
      RETURN                                                   
   ENDIF                                                       
   *** ENTER (INICIO DE OPERACIONES DE ACTUALIZACION DE DATOS  
   IF WMUEVE = 13                                              
      DO CASE                                                  
         CASE WMENU = 1                                        
              DO APACTAI
         CASE WMENU = 2                
              DO RECLOC 
              DO APACTAM
              UNLOCK 
         CASE WMENU = 3 
              DO RECLOC
              DO APACTAE
              UNLOCK 
         CASE WMENU = 4                        
              STORE CODIGO TO WSEL
              SET COLOR TO
              RETURN                            
         CASE WMENU = 5                         
              SAVE SCRE TO S9
              store "METODO DE BUSQUEDA: (C)ODIGO, (D)ESCRIPCION, (S)ALIR" TO TEX
              STORE "SCD" TO WCH
              DO PREGUNTA
              STORE WCH TO WSCD
              IF WSCD = "S"
                 RESTORE SCRE FROM S9
              ELSE
                 IF WSCD = "C"                        
                    STORE SPACE(LEN(CODIGO)) TO WCODIGO 
                    @ WSCRELIN ,01  GET WCODIGO         
                    READ                                
                    IF WCODIGO <> SPACE(LEN(CODIGO))
                       FIND &WCODIGO
                       IF .NOT. EOF()
                          STORE RECNO() TO WINICIO        
                          STORE .T. TO FLAGJUMP           
                          STORE RECNO() TO WREVERSE       
                          STORE RECNO() TO WCLEARREC      
                          STORE WLINE   TO WEDILIN        
                          STORE WLINE   TO WCLEARLIN
                       ELSE                         
                          RESTORE SCRE FROM S9              
                       ENDIF                                  
                    ELSE                                      
                       RESTORE SCRE FROM S9                   
                    ENDIF
                 ELSE
                    STORE "INDIQUE DESCRIPCION:" TO MES
                    DO MENSAJE WITH MES
                    STORE SPACE(15) TO WSEARCH
                    @ 23,50 GET WSEARCH
                    READ
                    STORE RTRIM(LTRIM(WSEARCH)) TO WSEARCH
                    STORE LEN(WSEARCH) TO WLENSER
                    STORE RECNO() TO WSTARTING
                    GO TOP
                    STORE 100 TO WLINEEMI
                    STORE 0 TO PAGE
                    DO WHILE .NOT. EOF()
                       IF WSEARCH <> SPACE(WLENSER) .AND. AT(WSEARCH,DESCRI) = 0
                          SKIP
                          LOOP
                       ENDIF
                       STORE WLINEEMI + 1 TO WLINEEMI
                       IF WLINEEMI > 21
                          STORE PAGE + 1 TO PAGE
                          IF PAGE > 1
                             STORE "SELECCIONE: (C)ONTINUAR, (S)ALIR" TO TEX
                             STORE "CS" TO WCH
                             DO PREGUNTA
                             IF WCH = "S"
                                EXIT
                             ENDIF
                          ENDIF
                          SET COLOR TO W+/BG
                          @ 4,0 CLEAR
                          @ 5,0 SAY "GOBERNACION DEL ESTADO ANZOATEGUI"
                          @ 6,0 SAY "SISTEMA DE INFORMACION"
                          @ 7,0 SAY "CODIGO"
                          @ 7,20 SAY "DESCRIPCION"
                          @ 8,0 SAY "------------------"
                          @ 8,20 SAY "------------------------------"
                          STORE 9 TO WLINEEMI
                       ENDIF
                       @ WLINEEMI,0 SAY CODIGO
                       @ WLINEEMI,20 SAY DESCRI
                       SKIP
                    ENDDO
                    store "OPRIMA <ENTER> PARA SALIR" TO MES
                    DO AVISO WITH MES
                    GO WSTARTING
                    RESTORE SCRE FROM S9  
                 ENDIF
              ENDIF                                   
      ENDCASE                                         
   ENDIF                                              
ENDDO                                                 
RETURN                                                
                                                      
