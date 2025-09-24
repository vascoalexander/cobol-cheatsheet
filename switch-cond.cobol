       IDENTIFICATION DIVISION. 
       PROGRAM-ID. SWITCH-COND.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 EMPLOYMENT-NUMBER  PIC 9 VALUE ZERO.
       01 EMPLOYMENT-STATUS  PIC X(20).
       
       PROCEDURE DIVISION.
           DISPLAY "Enter the employment number: ".
           ACCEPT EMPLOYMENT-NUMBER
           EVALUATE EMPLOYMENT-NUMBER 
              WHEN 1 MOVE "Employed" TO EMPLOYMENT-STATUS 
              WHEN 2 MOVE "Self-employed" TO EMPLOYMENT-STATUS 
              WHEN 3 MOVE "Retired" TO EMPLOYMENT-STATUS 
              WHEN 4 MOVE "Student" TO EMPLOYMENT-STATUS 
              WHEN 5 MOVE "Not employed" TO EMPLOYMENT-STATUS 
              WHEN OTHER MOVE "N/A" TO EMPLOYMENT-STATUS 
           END-EVALUATE.
           DISPLAY "Employment status is " EMPLOYMENT-STATUS.