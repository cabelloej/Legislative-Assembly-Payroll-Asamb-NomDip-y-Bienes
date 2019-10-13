              STORE SPACE(LEN(CODIGO)) TO WCODIGO                                                                                                                                                                                                             
              STORE SPACE(LEN(DESCRI)) TO WDESCRI                                                                                                                                                                                                             
              STORE 100 TO WSCRELIN                                                                                                                                                                                                                           
              STORE .T. TO WACT                                                                                                                                                                                                                               
              DO WHILE WACT                                                                                                                                                                                                                                   
                 STORE WSCRELIN + 1 TO WSCRELIN                                                                                                                                                                                                               
                 IF WSCRELIN > 18                                                                                                                                                                                                                             
                    SET COLOR TO W+/BG
                    @ 7,1 CLEAR TO 18,78
                    STORE 7 TO WSCRELIN                                                                                                                                                                                                                       
                 ENDIF                                                                                                                                                                                                                                        
                 @ WSCRELIN ,01  GET WCODIGO                                                                                                                                                                                                                  
                 READ                                                                                                                                                                                                                                         
                 IF WCODIGO = SPACE(LEN(CODIGO)) .OR.READKEY()=12.OR.READKEY()=268                                                                                                                                                                            
                    STORE .F. TO WACT                                                                                                                                                                                                                         
                    GO TOP                                                                                                                                                                                                                                    
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE 0 TO WREVERSE                                                                                                                                                                                                                       
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 find &wcodigo
                 if eof()
                    ***
                 else
                    @ WSCRELIN ,30  SAY DESCRI
                    STORE "CODIGO YA EXISTE. <ENTER>" TO MES
                    DO AVISO WITH MES
                    STORE WSCRELIN - 1  TO WSCRELIN
                    LOOP
                 ENDIF
                 @ WSCRELIN ,30  GET WDESCRI
                 read
                 STORE "CONFORME ? (S/N)" TO TEX                                                                                                                                                                                                              
                 STORE "SN" TO WCH                                                                                                                                                                                                                            
                 DO PREGUNTA                                                                                                                                                                                                                                  
                 IF WCH = "N"                                                                                                                                                                                                                                 
                    STORE WSCRELIN - 1  TO WSCRELIN                                                                                                                                                                                                           
                    LOOP                                                                                                                                                                                                                                      
                 ENDIF                                                                                                                                                                                                                                        
                 DO FILLOC
                 APPEND BLANK
                 UNLOCK
                 do recloc                                                                                                                                                                                                                                    
                 REPLACE CODIGO WITH WCODIGO                                                                                                                                                                                                                  
                 REPLACE DESCRI WITH WDESCRI                                                                                                                                                                                                                  
                 REPLACE USERNAME   WITH SYS(30)
                 REPLACE USERDATE   WITH DATE()
                 FLUSH                                                                                                                                                                                                                                        
                 unlock 
              ENDDO                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                              
