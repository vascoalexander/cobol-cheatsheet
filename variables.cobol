       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-VARIABLES.
       AUTHOR. BASQUE.
       DATE-WRITTEN. 03.10.2025.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 

      *GANZZAHLEN
       01 WS-COUNTER           PIC 9(4).            *> 0-9999
       01 WS-AGE               PIC 99.              *> 0-99
       01 WS-YEAR              PIC 9999.            *> 0-9999
       01 WS-BIG-NUMBER        PIC 9(10).           *> 10-stellig

      *VORZEICHENBEHAFTET (signed)
       01 WS-TEMPERATURE       PIC S999.            *> -999 bis +999
       01 WS-BALANCE           PIC S9(7).           *> Mit Vorzeichen
       01 WS-PROFIT            PIC S9(8) COMP.      *> Binary, schneller

      *DEZIMALZAHLEN (V = implied decimal)
       01 WS-PRICE             PIC 9(5)V99.         *> 12345.67
       01 WS-SALARY            PIC 9(6)V99.         *> 123456.78
       01 WS-PERCENTAGE        PIC 99V99.           *> 12.34
       01 WS-INTEREST          PIC V999.            *> 0.123

      *VORZEICHEN UND DEZIMAL
       01 WS-TOTAL             PIC S9(9)V99 VALUE ZERO.
       01 WS-AMOUNT-TOTAL      PIC S9(7)V99.        *> -1234567.89 bis
                                                    *> +1234567.89

      *PACKED DECIMALS (speichereffizient)
       01 WS-COMP3-VALUE       PIC S9(7)V99 COMP-3.
       01 WS-SALES             PIC 9(9)V99 COMP-3.

      *BINARY
       01 WS-BINARY-NUM        PIC 9(4) COMP.       *> Binary Integer
       01 WS-INDEX             PIC 9(8) COMP.

      *TEXT-STRING
       01 WS-NAME              PIC X(30).           *> 30 Zeichen
       01 WS-ADDRESS           PIC X(50).           *> 50 Zeichen
       01 WS-CODE              PIC X(10).           *> 10 Zeichen
       01 WS-SINGLE-CHAR       PIC X.               *> 1 Zeichen

      *ALPHABETISCH (nur Buchstaben)
       01 WS-LAST              PIC A(20).           *> Nur A-Z, a-z
       01 WS-FIRST             PIC A(15).

      *INITIALISIERIERUNG MIT VALUE
       01 WS-COUNTER1          PIC 9(3) VALUE 0.
       01 WS-COUNTER2          PIC 9(3) VALUE ZERO.
       01 WS-COUNTER3          PIC 9(3) VALUE ZEROS.
       01 WS-COUNTER4          PIC 9(3) VALUE ZEROES.
       
       01 WS-FULL-NAME         PIC X(20) VALUE "Max Mustermann".
       01 WS-EMPTY             PIC X(10) VALUE SPACE.
       01 WS-EMPTY2            PIC X(10) VALUE SPACES.
       
       01 WS-FLAG              PIC X VALUE "Y".
       01 WS-STATUS            PIC XX VALUE "OK".
       
       01 WS-RATE              PIC 99V99 VALUE 12.50.

      *GRUPPEN
       01 WS-PERSON.
          05 WS-FIRST-NAME     PIC X(15).
          05 WS-LAST-NAME      PIC X(20).
          05 WS-PER-AGE        PIC 99.
          05 WS-PER-SALARY     PIC 9(6)V99.
       
       01 WS-DATE.
          05 WS-DATE-YEAR      PIC 9999.
          05 WS-DATE-MONTH     PIC 99.
          05 WS-DATE-DAY       PIC 99.
       
       01 WS-PRIVATE-ADDRESS.
          05 WS-STREET         PIC X(30).
          05 WS-CITY           PIC X(20).
          05 WS-ZIP            PIC X(10).
          05 WS-COUNTRY        PIC X(20).

      *VERSCHACHTELTE GRUPPEN
       01 WS-EMPLOYEE.
           05 WS-EMP-ID         PIC 9(6).
           05 WS-EMP-NAME.
              10 WS-FIRSTNAME   PIC X(15).
              10 WS-LASTNAME    PIC X(20).
           05 WS-EMP-CONTACT.
              10 WS-PHONE       PIC X(15).
              10 WS-EMAIL       PIC X(40).
           05 WS-EMP-SALARY     PIC 9(7)V99 COMP-3.

      *REDEFINES (Verschiedene Sichten derselben Daten)
       01 WS-DATE-NUM          PIC 9(8).              *> 20231015
       01 WS-DATE-ALPHA REDEFINES WS-DATE-NUM.
          05 WS-ALPHA-YEAR     PIC 9999.
          05 WS-ALPHA-MONTH    PIC 99.
          05 WS-ALPHA-DAY      PIC 99.
       
       01 WS-AMOUNT-RAW        PIC 9(5)V99.
       01 WS-AMOUNT-EDITED REDEFINES WS-AMOUNT-RAW PIC ZZZZ9.99.

      *OCCURS SIMPLE ARRAY
       01 WS-MONTHS            PIC X(10) OCCURS 12 TIMES.
       01 WS-NUMBERS           PIC 9(4) OCCURS 100 TIMES.
       01 WS-SCORES            PIC 999 OCCURS 50 TIMES.

      *ARRAY VON GRUPPEN
       01 WS-STUDENTS OCCURS 30 TIMES.
          05 WS-STUDENT-ID     PIC 9(6).
          05 WS-STUDENT-NAME   PIC X(30).
          05 WS-STUDENT-GRADE  PIC 99V99.

      *MEHRDIMENSIONALES ARRAY
       01 WS-MATRIX.
          05 WS-ROW OCCURS 10 TIMES.
             10 WS-COL         PIC 9(4) OCCURS 10 TIMES.

      *ZAHLENFORMATIERUNG
       01 WS-DISPLAY-AMOUNT    PIC ZZZ,ZZ9.99.        *> 1,234.56
       01 WS-CURRENCY          PIC $$$,$$9.99.        *> $1,234.56
       01 WS-WITH-SIGN         PIC +ZZ,ZZ9.99.        *> +1,234.56
       01 WS-WITH-MINUS        PIC -ZZ,ZZ9.99.        *> -1,234.56
       01 WS-LEADING-ZERO      PIC 000009.            *> 000123

      *DATUMSFORMATIERUNG
       01 WS-DATE-FORMATTED-1  PIC 99/99/9999.        *> 15/10/2023
       01 WS-DATE-FORMATTED-2.
          05 WS-DAY-F          PIC 99.
          05 FILLER            PIC X VALUE ".".
          05 WS-MONTH-F        PIC 99.
          05 FILLER            PIC X VALUE ".".
          05 WS-YEAR-F         PIC 9999.               *> 15.10.2023

      *JUSTIFIED
       01 WS-RIGHT-ALIGNED     PIC X(10) JUSTIFIED RIGHT.

      *BLANK WITH ZEROS
       01 WS-OPTIONAL-NUM      PIC ZZZ9 BLANK WHEN ZERO.

      *88-LEVEL-CONDITIONS
       01 WS-STATE             PIC X.
          88 ACTIVE            VALUE "A".
          88 INACTIVE          VALUE "I".
          88 PENDING           VALUE "P".
       
       01 WS-DAY-NUM           PIC 9.
          88 MONDAY            VALUE 1.
          88 TUESDAY           VALUE 2.
          88 WEEKEND           VALUES 6 7.
       
      *POINTER UND SPECIAL TYPES
       01 WS-POINTER           USAGE POINTER.
       01 WS-BINARY            PIC 9(4) BINARY.
       01 WS-COMP-1            USAGE COMP-1.   *> Single precision float
       01 WS-COMP-2            USAGE COMP-2.   *> Double precision float
       01 WS-DISP              PIC X(20).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM 01-GANZZAHLEN.
           PERFORM 02-VORZEICHENBEHAFTET.
           PERFORM 03-DEZIMALZAHLEN.
           PERFORM 04-VORZEICHEN-UND-DEZIMAL.
           PERFORM 05-PACKED-DECIMAL.
           PERFORM 06-BINARY.
           PERFORM 10-TEXT-STRING.
           PERFORM 11-ALPHABETISCH.
           PERFORM 20-INITIALISIERT.
           PERFORM 30-GRUPPEN.
           PERFORM 40-VERSCHACHTELT.
           PERFORM 50-REDEFINES.
           PERFORM 60-OCCURS.
           PERFORM 61-OCCURS-GROUPED.
           PERFORM 62-MATRIX.
           PERFORM 70-ZAHLENFORMATIERUNG.
           PERFORM 71-DATUMSFORMATIERUNG.
           PERFORM 72-JUSTIFIED.
           PERFORM 73-BLANK.
           PERFORM 80-KONSTANTEN.
           PERFORM 90-SPECIAL-TYPES.
           STOP RUN.
           
      *NUMERISCHE VARIABLE  
       01-GANZZAHLEN.
           DISPLAY "> GANZZAHLEN"
           DISPLAY "--------------------"
           MOVE 1234 TO WS-COUNTER
           MOVE 48 TO WS-AGE
           MOVE 2025 TO WS-YEAR
           MOVE 9876543210 TO WS-BIG-NUMBER
           DISPLAY "COUNTER=" WS-COUNTER
           DISPLAY "AGE=" WS-AGE
           DISPLAY "YEAR=" WS-YEAR
           DISPLAY "BIG-NUMBER=" WS-BIG-NUMBER
           DISPLAY SPACE.

       02-VORZEICHENBEHAFTET.
           DISPLAY "> VORZEICHENBEHAFTET"
           DISPLAY "--------------------"
           MOVE -123 TO WS-TEMPERATURE
           MOVE 45000 TO WS-BALANCE
           MOVE -9999999 TO WS-PROFIT
           DISPLAY "TEMP=" WS-TEMPERATURE
           DISPLAY "BALANCE=" WS-BALANCE
           DISPLAY "PROFIT=" WS-PROFIT
           DISPLAY SPACE.

       03-DEZIMALZAHLEN.
           DISPLAY "> DEZIMALZAHLEN"
           DISPLAY "--------------------"       
           MOVE 12345.67 TO WS-PRICE
           MOVE 654321.99 TO WS-PER-SALARY
           MOVE 12.34 TO WS-PERCENTAGE
           MOVE 0.123 TO WS-INTEREST
           DISPLAY "PRICE=" WS-PRICE
           DISPLAY "SALARY=" WS-PER-SALARY
           DISPLAY "PERCENT=" WS-PERCENTAGE
           DISPLAY "INTEREST=" WS-INTEREST
           DISPLAY SPACE.

       04-VORZEICHEN-UND-DEZIMAL.
           DISPLAY "> VORZEICHEN-UND-DEZIAML"
           DISPLAY "--------------------"       
           MOVE -12345.67 TO WS-AMOUNT-TOTAL
           MOVE 9876543.21 TO WS-TOTAL
           DISPLAY "AMOUNT=" WS-AMOUNT-TOTAL
           DISPLAY "TOTAL=" WS-TOTAL
           DISPLAY SPACE.

       05-PACKED-DECIMAL.
           DISPLAY "> PACKED-DECIMAL"
           DISPLAY "--------------------"
           MOVE 1234567.89 TO WS-COMP3-VALUE
           MOVE 987654321.12 TO WS-SALES
           DISPLAY "COMP-3 VALUE=" WS-COMP3-VALUE
           DISPLAY "SALES=" WS-SALES
           DISPLAY SPACE.

       06-BINARY.
           DISPLAY "> BINARY"
           DISPLAY "--------------------"
           MOVE 1000 TO WS-BINARY-NUM
           MOVE 123456 TO WS-INDEX
           DISPLAY "BINARY=" WS-BINARY-NUM
           DISPLAY "INDEX=" WS-INDEX
           DISPLAY SPACE.

      *ALPHANUMERISCHE WERTE 
       10-TEXT-STRING.
           DISPLAY "> TEXT-STRING"
           DISPLAY "--------------------"
           MOVE "John Doe" TO WS-NAME
           MOVE "Berlin" TO WS-ADDRESS
           MOVE "A1B2C3" TO WS-CODE
           MOVE "Z" TO WS-SINGLE-CHAR
           DISPLAY "NAME=" WS-NAME
           DISPLAY "ADDRESS=" WS-ADDRESS
           DISPLAY "CODE=" WS-CODE
           DISPLAY "CHAR=" WS-SINGLE-CHAR
           DISPLAY SPACE.

       11-ALPHABETISCH.
           DISPLAY "> ALPHABETISCH"
           DISPLAY "--------------------"
           MOVE "MUSTERMANN" TO WS-LAST-NAME
           MOVE "MAX" TO WS-FIRST-NAME
           DISPLAY "LAST=" WS-LAST-NAME
           DISPLAY "FIRST=" WS-FIRST-NAME
           DISPLAY SPACE.

      *INITIALISIERUNG MIT VALUE 
       20-INITIALISIERT.
           DISPLAY "> INITIALISIERT"
           DISPLAY "--------------------"
           DISPLAY "COUNTER2=" WS-COUNTER
           DISPLAY "NAME2=" WS-FULL-NAME
           DISPLAY "EMPTY=" WS-EMPTY
           DISPLAY "FLAG=" WS-FLAG
           DISPLAY "STATUS=" WS-STATUS
           DISPLAY "RATE=" WS-RATE
           DISPLAY SPACE.

      *GRUPPENVARIABLEN
       30-GRUPPEN.
           DISPLAY "> GRUPPEN"
           DISPLAY "--------------------"
           MOVE "Alice" TO WS-FIRST-NAME
           MOVE "Wonderland" TO WS-LAST-NAME
           MOVE 30 TO WS-PER-AGE
           MOVE 50000.50 TO WS-PER-SALARY OF WS-PERSON
           DISPLAY "PERSON=" FUNCTION TRIM(WS-FIRST-NAME)
                   " " WS-LAST-NAME
           DISPLAY "AGE=" WS-PER-AGE
           DISPLAY "SALARY=" WS-PER-SALARY OF WS-PERSON
           DISPLAY SPACE.

      *VERSCHACHTELTE GRUPPEN
       40-VERSCHACHTELT.
           DISPLAY "> VERSCHACHTELT"
           DISPLAY "--------------------"
           MOVE 123456 TO WS-EMP-ID
           MOVE "Bob" TO WS-FIRST
           MOVE "Builder" TO WS-LAST
           MOVE "123456789" TO WS-PHONE
           MOVE "bob@builder.com" TO WS-EMAIL
           MOVE 35000.75 TO WS-EMP-SALARY
           DISPLAY "EMPLOYEE ID=" WS-EMP-ID
           DISPLAY "NAME=" WS-FIRSTNAME " " WS-LASTNAME
           DISPLAY "PHONE=" WS-PHONE
           DISPLAY "EMAIL=" WS-EMAIL
           DISPLAY "SALARY=" WS-EMP-SALARY
           DISPLAY SPACE.

      *REDEFINES
       50-REDEFINES.
           DISPLAY "> REDEFINES"
           DISPLAY "--------------------"
           MOVE 20251003 TO WS-DATE-NUM
           DISPLAY "DATE NUM=" WS-DATE-NUM
           DISPLAY "YEAR=" WS-ALPHA-YEAR
           DISPLAY "MONTH=" WS-ALPHA-MONTH
           DISPLAY "DAY=" WS-ALPHA-DAY
           MOVE 1234.56 TO WS-AMOUNT-RAW
           DISPLAY "AMOUNT RAW=" WS-AMOUNT-RAW
           DISPLAY "AMOUNT EDITED=" WS-AMOUNT-EDITED
           DISPLAY SPACE.

      *ARRAYS / TABELLEN
       60-OCCURS.
           DISPLAY "> OCCURS"
           DISPLAY "--------------------"
           MOVE "JANUARY" TO WS-MONTHS(1)
           MOVE "FEBRUARY" TO WS-MONTHS(2)
           MOVE 1001 TO WS-NUMBERS(1)
           MOVE 2002 TO WS-NUMBERS(2)
           DISPLAY "MONTH1=" WS-MONTHS(1)
           DISPLAY "MONTH2=" WS-MONTHS(2)
           DISPLAY "NUMBER1=" WS-NUMBERS(1)
           DISPLAY "NUMBER2=" WS-NUMBERS(2)
           DISPLAY SPACE.

       61-OCCURS-GROUPED.
           DISPLAY "> OCCURS-GROUPED"
           DISPLAY "--------------------"
           MOVE 1001 TO WS-STUDENT-ID(1)
           MOVE "Alice" TO WS-STUDENT-NAME(1)
           MOVE 95 TO WS-STUDENT-GRADE(1)
           MOVE 1002 TO WS-STUDENT-ID(2)
           MOVE "Bob" TO WS-STUDENT-NAME(2)
           MOVE 88 TO WS-STUDENT-GRADE(2)
           DISPLAY "STUDENT1 ID=" WS-STUDENT-ID(1)
           DISPLAY "STUDENT1 NAME=" WS-STUDENT-NAME(1)
           DISPLAY "STUDENT1 GRADE=" WS-STUDENT-GRADE(1)
           DISPLAY "STUDENT2 ID=" WS-STUDENT-ID(2)
           DISPLAY "STUDENT2 NAME=" WS-STUDENT-NAME(2)
           DISPLAY "STUDENT2 GRADE=" WS-STUDENT-GRADE(2)
           DISPLAY SPACE.

       62-MATRIX.
           DISPLAY "> MATRIX"
           DISPLAY "--------------------"
           MOVE 11 TO WS-COL(1,1)
           MOVE 22 TO WS-COL(1,2)
           MOVE 33 TO WS-COL(2,1)
           MOVE 44 TO WS-COL(2,2)
           DISPLAY "MATRIX(1,1)=" WS-COL(1,1)
           DISPLAY "MATRIX(1,2)=" WS-COL(1,2)
           DISPLAY "MATRIX(2,1)=" WS-COL(2,1)
           DISPLAY "MATRIX(2,2)=" WS-COL(2,2)
           DISPLAY SPACE.

      *EDITIED PICTURES
       70-ZAHLENFORMATIERUNG.
           DISPLAY "> ZAHLENFORMATIERUNG"
           DISPLAY "--------------------"
           MOVE 1234.56 TO WS-DISPLAY-AMOUNT
           MOVE 9876.54 TO WS-CURRENCY
           DISPLAY "DISPLAY AMOUNT=" WS-DISPLAY-AMOUNT
           DISPLAY "CURRENCY=" WS-CURRENCY
           DISPLAY SPACE.

       71-DATUMSFORMATIERUNG.
           DISPLAY "> DATUMSFORMATIERUNG"
           DISPLAY "--------------------"
      *    Die Syntax ist (Start:Länge)     
           MOVE 15 TO WS-DATE-FORMATTED-1(1:2)
           MOVE 10 TO WS-DATE-FORMATTED-1(4:2)
           MOVE 2023 TO WS-DATE-FORMATTED-1(7:4)
           DISPLAY "DATE FORMATTED 1=" WS-DATE-FORMATTED-1
           MOVE 15 TO WS-DAY-F
           MOVE 10 TO WS-MONTH-F
           MOVE 2023 TO WS-YEAR-F
           DISPLAY "DATE FORMATTED 2=" WS-DATE-FORMATTED-2
           DISPLAY SPACE.

       72-JUSTIFIED.
           DISPLAY "> JUSTIFIED"
           DISPLAY "--------------------"         
           MOVE "COBOL" TO WS-RIGHT-ALIGNED
           DISPLAY "RIGHT ALIGNED='" WS-RIGHT-ALIGNED "'"
           DISPLAY SPACE.

       73-BLANK.
           DISPLAY "> BLANK"
           DISPLAY "--------------------"
           MOVE 0 TO WS-OPTIONAL-NUM
           DISPLAY "OPTIONAL NUM=" WS-OPTIONAL-NUM
           MOVE 1234 TO WS-OPTIONAL-NUM
           DISPLAY "OPTIONAL NUM=" WS-OPTIONAL-NUM
           DISPLAY SPACE.

      *88 LEVEL-CONDITION
       80-KONSTANTEN.
           DISPLAY "> KONSTANTEN"
           DISPLAY "--------------------"
           MOVE "A" TO WS-STATE
           IF ACTIVE DISPLAY "STATE=ACTIVE"
           MOVE "I" TO WS-STATE
           IF INACTIVE DISPLAY "STATE=INACTIVE"
           DISPLAY SPACE.

      *POINTER UND SPECIAL TYPES
       90-SPECIAL-TYPES.
           DISPLAY "> SPECIAL-TYPES"
           DISPLAY "--------------------"
           SET WS-POINTER TO NULL
           DISPLAY "POINTER=" WS-POINTER
           MOVE 1234 TO WS-BINARY
           DISPLAY "BINARY=" WS-BINARY
           MOVE 123.45 TO WS-COMP-1
           DISPLAY "COMP-1=" WS-COMP-1
           MOVE 12345.6789 TO WS-COMP-2
           DISPLAY "COMP-2=" WS-COMP-2
           DISPLAY SPACE.
          



  

       




