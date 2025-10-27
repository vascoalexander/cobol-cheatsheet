       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIMESTAMP-CHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Eingabe-Timestamp
       01  WS-INPUT-TIMESTAMP      PIC X(23).
           88  VALID-FORMAT        VALUE SPACES THRU HIGH-VALUES.

      * Zerlegte Timestamp-Felder
       01  WS-TS-DATE.
           05  WS-TS-YEAR          PIC 9(4).
           05  FILLER              PIC X VALUE '-'.
           05  WS-TS-MONTH         PIC 9(2).
           05  FILLER              PIC X VALUE '-'.
           05  WS-TS-DAY           PIC 9(2).

       01  WS-TS-TIME.
           05  WS-TS-HOUR          PIC 9(2).
           05  FILLER              PIC X VALUE ':'.
           05  WS-TS-MINUTE        PIC 9(2).
           05  FILLER              PIC X VALUE ':'.
           05  WS-TS-SECOND        PIC 9(2).
           05  FILLER              PIC X VALUE '.'.
           05  WS-TS-MILLISEC      PIC 9(3).

      * Aktuelle Systemzeit
       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR        PIC 9(4).
           05  WS-CURR-MONTH       PIC 9(2).
           05  WS-CURR-DAY         PIC 9(2).
           05  WS-CURR-HOUR        PIC 9(2).
           05  WS-CURR-MINUTE      PIC 9(2).
           05  WS-CURR-SECOND      PIC 9(2).
           05  WS-CURR-MILLISEC    PIC 9(2).

      * Berechnungsfelder für 6 Monate zurück
       01  WS-CALC-FIELDS.
           05  WS-CALC-YEAR        PIC 9(4).
           05  WS-CALC-MONTH       PIC 9(2).
           05  WS-CALC-DAY         PIC 9(2).

      * Vergleichsfelder (YYYYMMDD Format)
       01  WS-TS-COMPARE           PIC 9(8).
       01  WS-THRESHOLD-COMPARE    PIC 9(8).

       01  WS-RESULT               PIC X(30).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      * Beispiel-Timestamp setzen
           MOVE '2025-04-26 14:30:45.123' TO WS-INPUT-TIMESTAMP

      * Timestamp zerlegen
           UNSTRING WS-INPUT-TIMESTAMP DELIMITED BY '-' OR ' ' OR ':'
               INTO WS-TS-YEAR
                    WS-TS-MONTH
                    WS-TS-DAY
                    WS-TS-HOUR
                    WS-TS-MINUTE
                    WS-TS-SECOND
           END-UNSTRING

      * Aktuelle Systemzeit holen
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE

      * 6 Monate vom aktuellen Datum zurückrechnen
           COMPUTE WS-CALC-YEAR = WS-CURR-YEAR
           COMPUTE WS-CALC-MONTH = WS-CURR-MONTH - 6
           COMPUTE WS-CALC-DAY = WS-CURR-DAY

      * Jahresübertrag behandeln
           IF WS-CALC-MONTH < 1
               SUBTRACT 1 FROM WS-CALC-YEAR
               ADD 12 TO WS-CALC-MONTH
           END-IF

      * Vergleichswerte erstellen (YYYYMMDD)
           COMPUTE WS-TS-COMPARE =
               (WS-TS-YEAR * 10000) +
               (WS-TS-MONTH * 100) +
               WS-TS-DAY

           COMPUTE WS-THRESHOLD-COMPARE =
               (WS-CALC-YEAR * 10000) +
               (WS-CALC-MONTH * 100) +
               WS-CALC-DAY

      * Vergleich durchführen
           IF WS-TS-COMPARE < WS-THRESHOLD-COMPARE
               MOVE 'Timestamp ist älter als 6 Monate' TO WS-RESULT
               DISPLAY WS-RESULT
           ELSE
               MOVE 'Timestamp ist jünger als 6 Monate' TO WS-RESULT
               DISPLAY WS-RESULT
           END-IF

           DISPLAY 'Timestamp-Datum: ' WS-TS-COMPARE
           DISPLAY 'Schwellenwert:   ' WS-THRESHOLD-COMPARE

           STOP RUN.