       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-VARIABLES.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
      * AUFGABE 1 
       01 FULL-NAME          PIC X(15).
       01 AGE                PIC 9(2).
       01 SALARY             PIC 9(5)V9(2).
      * AUFGABE 2
       01 FIRST-NAME         PIC X(3) VALUE "Max".
       01 ZIP-CODE           PIC 9(5) VALUE 80331.
       01 SALDO              PIC 9(4)V9(2) VALUE 1000.00.
      * AUFGABE 3
       01 PERSON-DATA.
           05 FIRSTNAME      PIC X(10).
           05 LASTNAME       PIC X(15).
           05 AGE            PIC 9(2).
      * AUFGABE 4
       01 CUSTOMER-ADDRESS.
           05 STREET         PIC X(20).
           05 HOUSE-NUMBER   PIC 9(3).
           05 CITY-INFO.
              10 ZIP-CODE2   PIC 9(5).
              10 CITY-NAME   PIC X(20).
      * AUFGABE 5
       01 STATE              PIC X(1).
           88 ACTIVE   VALUE "A".
           88 INACTIVE VALUE "I".

       PROCEDURE DIVISION.

           SET ACTIVE TO TRUE.
           MOVE 12000.99 TO SALARY 
           MOVE 44135 TO ZIP-CODE2 
           MOVE "Dortmund" TO CITY-NAME 

           IF ACTIVE
              DISPLAY FIRST-NAME
              DISPLAY SALARY
              DISPLAY CITY-INFO  
           END-IF

           STOP RUN.



  

       




