       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOPS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       01 COUNTER-NUM PIC 9(1) VALUE 0.
       
       PROCEDURE DIVISION.
       100-BEGIN.
           PERFORM 200-SHOW-COUNTER 7 TIMES.
           PERFORM 300-LOOP-UNTIL.
           PERFORM 400-DO-WHILE.
           PERFORM 500-END.
       200-SHOW-COUNTER.
           ADD 1 TO COUNTER-NUM.

           DISPLAY "Loop number " COUNTER-NUM.

       300-LOOP-UNTIL.
           PERFORM UNTIL COUNTER-NUM = 0
              SUBTRACT 1 FROM COUNTER-NUM
              DISPLAY "Loop number " COUNTER-NUM
           END-PERFORM.

       400-DO-WHILE.
           PERFORM WITH TEST AFTER UNTIL COUNTER-NUM >= 7
              ADD 1 TO COUNTER-NUM 
              DISPLAY "Loop number " COUNTER-NUM 
           END-PERFORM.

       500-END.
           STOP RUN.
