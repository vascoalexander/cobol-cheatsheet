       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CONDITIONALS.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 PERSON-AGE   PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "Enter the Person's age: ".
              ACCEPT PERSON-AGE.
              IF PERSON-AGE >= 16 and PERSON-AGE < 70 THEN
                 DISPLAY "You are allowed to drive"
              ELSE
                 DISPLAY "You are not allowed to drive"
              END-IF.