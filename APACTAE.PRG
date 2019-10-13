              if select() = 7
                 store codigo to wbenefi
                 select 3
                 set order to 2
                 find &wbenefi
                 if .not. eof()
                    store "ERROR, BENEFICIARIO CON TRANSACCIONES." TO MES
                    do aviso with mes
                    set order to 1
                    select 7
                    return
                 else
                    set order to 1
                    select 7
                 endif
              endif
              STORE "ESTA SEGURO QUE DESEA ELIMINAR (S/N)" TO TEX                                                                                                                                                                                             
              STORE "NS" TO WCH                                                                                                                                                                                                                               
              DO PREGUNTA                                                                                                                                                                                                                                     
              IF WCH = "S"                                                                                                                                                                                                                                    
                 REPLACE USERNAME   WITH SYS(30)
                 REPLACE USERDATE   WITH DATE()
                 DELETE                                                                                                                                                                                                                                       
                 FLUSH
                 STORE .T. TO FLAGDEL                                                                                                                                                                                                                         
                 SKIP                                                                                                                                                                                                                                         
                 IF .NOT. EOF()                                                                                                                                                                                                                               
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE RECNO() TO WREVERSE                                                                                                                                                                                                                 
                    STORE RECNO() TO WCLEARREC                                                                                                                                                                                                                
                    STORE WLINE   TO WEDILIN                                                                                                                                                                                                                  
                    STORE WLINE   TO WCLEARLIN                                                                                                                                                                                                                
                 ELSE                                                                                                                                                                                                                                         
                    GO TOP                                                                                                                                                                                                                                    
                    STORE RECNO() TO WINICIO                                                                                                                                                                                                                  
                    STORE .T. TO FLAGJUMP                                                                                                                                                                                                                     
                    STORE 0 TO WREVERSE                                                                                                                                                                                                                       
                 ENDIF                                                                                                                                                                                                                                        
              ENDIF                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                              
