       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-CHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Eingabe-Timestamp (nur Datum wird verwendet)
       01  WS-INPUT-TIMESTAMP      PIC X(23).

      * Zerlegte Datumsfelder
       01  WS-TS-YEAR              PIC 9(4).
       01  WS-TS-MONTH             PIC 9(2).
       01  WS-TS-DAY               PIC 9(2).

      * Aktuelle Systemzeit
       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR        PIC 9(4).
           05  WS-CURR-MONTH       PIC 9(2).
           05  WS-CURR-DAY         PIC 9(2).
           05  FILLER              PIC X(14).

      * Berechnungsfelder für 6 Monate zurück
       01  WS-CALC-YEAR            PIC 9(4).
       01  WS-CALC-MONTH           PIC 9(2).
       01  WS-CALC-DAY             PIC 9(2).

      * Vergleichsfelder (YYYYMMDD Format)
       01  WS-TS-COMPARE           PIC 9(8).
       01  WS-THRESHOLD-COMPARE    PIC 9(8).

       01  WS-RESULT               PIC X(30).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      * Beispiel-Timestamp setzen
           MOVE '2025-04-27 14:30:45.123' TO WS-INPUT-TIMESTAMP

      * Nur Datumsteil extrahieren (erste 10 Zeichen: YYYY-MM-DD)
           UNSTRING WS-INPUT-TIMESTAMP(1:10) DELIMITED BY '-'
               INTO WS-TS-YEAR
                    WS-TS-MONTH
                    WS-TS-DAY
           END-UNSTRING

      * Aktuelle Systemzeit holen
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE

      * 6 Monate vom aktuellen Datum zurückrechnen
           MOVE WS-CURR-YEAR  TO WS-CALC-YEAR
           MOVE WS-CURR-MONTH TO WS-CALC-MONTH
           MOVE WS-CURR-DAY   TO WS-CALC-DAY

           SUBTRACT 6 FROM WS-CALC-MONTH

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
               MOVE 'Älter als 6 Monate' TO WS-RESULT
           ELSE
               MOVE 'Jünger als 6 Monate' TO WS-RESULT
           END-IF

           DISPLAY 'Ergebnis:        ' WS-RESULT
           DISPLAY 'Timestamp-Datum: ' WS-TS-COMPARE
           DISPLAY 'Schwellenwert:   ' WS-THRESHOLD-COMPARE

           STOP RUN.