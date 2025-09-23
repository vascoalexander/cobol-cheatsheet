      *****************************************************************
      * Program name:    MYPROG                               
      * Original author: MYNAME                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 01/01/08 MYNAME  Created for COBOL class         
      *                                                               
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MYPROG.
       AUTHOR. MYNAME. 
       INSTALLATION. COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN. 01/01/08. 
       DATE-COMPILED. 01/01/08. 
       SECURITY. NON-CONFIDENTIAL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       01 UserName       PIC X(15).
       01 dateFormat     PIC 99/99/99.
       01 currencyFormat PIC $$$,$$9.99.
       
       PROCEDURE DIVISION.
           MOVE "Vasco" TO UserName.
           MOVE "010725" TO dateFormat. 
           MOVE "9999" TO currencyFormat. 
           DISPLAY "Hello " UserName dateFormat currencyFormat 
           GOBACK.
       