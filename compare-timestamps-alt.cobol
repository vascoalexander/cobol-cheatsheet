       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-CHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Eingabe-Timestamp
       01  WS-INPUT-TIMESTAMP      PIC X(23).

      * Datumsfelder im YYYYMMDD Format
       01  WS-TS-DATE              PIC 9(8).
       01  WS-CURRENT-DATE-NUM     PIC 9(8).
       01  WS-THRESHOLD-DATE       PIC 9(8).

      * Hilfsfelder für 6-Monats-Berechnung
       01  WS-YEAR                 PIC 9(4).
       01  WS-MONTH                PIC 9(2).
       01  WS-DAY                  PIC 9(2).

       01  WS-RESULT               PIC X(30).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      * Beispiel-Timestamp setzen
           MOVE '2024-03-15 14:30:45.123' TO WS-INPUT-TIMESTAMP

      * Datum aus Timestamp extrahieren (ohne Bindestriche)
           MOVE WS-INPUT-TIMESTAMP(1:4)  TO WS-TS-DATE(1:4)
           MOVE WS-INPUT-TIMESTAMP(6:2)  TO WS-TS-DATE(5:2)
           MOVE WS-INPUT-TIMESTAMP(9:2)  TO WS-TS-DATE(7:2)

      * Aktuelles Datum holen (bereits im YYYYMMDD Format)
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE-NUM

      * 6 Monate zurückrechnen
           MOVE WS-CURRENT-DATE-NUM(1:4) TO WS-YEAR
           MOVE WS-CURRENT-DATE-NUM(5:2) TO WS-MONTH
           MOVE WS-CURRENT-DATE-NUM(7:2) TO WS-DAY

           SUBTRACT 6 FROM WS-MONTH

           IF WS-MONTH < 1
               SUBTRACT 1 FROM WS-YEAR
               ADD 12 TO WS-MONTH
           END-IF

      * Schwellenwert zusammensetzen
           MOVE WS-YEAR  TO WS-THRESHOLD-DATE(1:4)
           MOVE WS-MONTH TO WS-THRESHOLD-DATE(5:2)
           MOVE WS-DAY   TO WS-THRESHOLD-DATE(7:2)

      * Direkter Vergleich der YYYYMMDD Zahlen
           IF WS-TS-DATE < WS-THRESHOLD-DATE
               MOVE 'Älter als 6 Monate' TO WS-RESULT
           ELSE
               MOVE 'Jünger als 6 Monate' TO WS-RESULT
           END-IF

           DISPLAY 'Ergebnis:        ' WS-RESULT
           DISPLAY 'Timestamp-Datum: ' WS-TS-DATE
           DISPLAY 'Schwellenwert:   ' WS-THRESHOLD-DATE

           STOP RUN.