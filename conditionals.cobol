       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CONDITIONALS.
       AUTHOR. BASQUE.
       DATE-WRITTEN. 03.10.2025.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 

       01 PERSON-AGE   PIC 9(3) VALUE 0.

       01 EMPLOYMENT-NUMBER  PIC 9 VALUE ZERO.
       01 EMPLOYMENT-STATUS  PIC X(20).

       01 INCOME PIC 9(6) VALUE ZERO.
           88 10-PERCENT VALUE 0 THRU 9875.
           88 12-PERCENT VALUE 9876 THRU 40125.
           88 22-PERCENT VALUE 40126 THRU 85525.
           88 24-PERCENT VALUE 85526 THRU 163300.
           88 32-PERCENT VALUE 163301 THRU 207350.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM COND-01.
           PERFORM COND-02.
           PERFORM COND-03.
           STOP RUN.

      *IF-ELSE
       COND-01.
           DISPLAY "Enter the Person's age: " WITH NO ADVANCING. 
           ACCEPT PERSON-AGE.
           IF PERSON-AGE >= 16 and PERSON-AGE < 70 THEN
               DISPLAY "You are allowed to drive"
           ELSE
              DISPLAY "You are not allowed to drive"
           END-IF.

      *SWITCH
       COND-02.
           DISPLAY SPACE.
           DISPLAY "Enter the employment number: " WITH NO ADVANCING.
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

      *88-LEVEL
       COND-03.
           DISPLAY SPACE.
           DISPLAY "Enter your income: " WITH NO ADVANCING.
           ACCEPT INCOME
           EVALUATE TRUE
                 WHEN 10-PERCENT 
                    DISPLAY "You are in the 10% tax bracket"
                 WHEN 12-PERCENT 
                    DISPLAY "You are in the 12% tax bracket"
                 WHEN 22-PERCENT 
                    DISPLAY "You are in the 22% tax bracket"
                 WHEN 24-PERCENT 
                    DISPLAY "You are in the 24% tax bracket"
                 WHEN 32-PERCENT 
                    DISPLAY "You are in the 32% tax bracket"
                 WHEN OTHER
                    DISPLAY "Income out of defined tax brackets"
           END-EVALUATE.
