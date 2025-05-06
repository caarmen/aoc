       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY06.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA EXTERNAL.
       01  F-DATA-RECORD                 PIC X(130).

       WORKING-STORAGE SECTION.
       01  C-DIR-UP                      CONSTANT 1.
       01  C-DIR-RIGHT                   CONSTANT 2.
       01  C-DIR-DOWN                    CONSTANT 3.
       01  C-DIR-LEFT                    CONSTANT 4.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                  PIC X(20).
      *> Grid state
       01  LS-GUARD-DIR                  PIC 9(1) VALUE C-DIR-UP.
       COPY "grid" IN "06".
      *> For calculating the number of Xs the guard marked:
       01  LS-ROW-X-COUNT                PIC 9(3) USAGE COMP VALUE 0.
       01  LS-TOTAL-X-COUNT              PIC 9(5) USAGE COMP VALUE 0.

       PROCEDURE DIVISION.
           ACCEPT LS-FILE-PATH FROM COMMAND-LINE
           CALL "PARSE-GRID" USING
               BY REFERENCE GRID-GRP

           display "read grid of size " GRID-SIZE
               " with guard at " GUARD-ROW "," GUARD-COL
           PERFORM UNTIL EXIT
      *> Mark each cell the guard occupies with an X.
               SET GRID-CELL(GUARD-ROW, GUARD-COL) to "X"
      *> Move, or set the next direction, of the guard, based on
      *> the next cell in the guard's current direction.
               EVALUATE LS-GUARD-DIR
                   WHEN C-DIR-UP
                       IF GRID-CELL(GUARD-ROW - 1, GUARD-COL) = "#"
                       THEN
                           SET LS-GUARD-DIR TO C-DIR-RIGHT
                       ELSE
                           ADD -1 TO GUARD-ROW
                       END-IF
                   WHEN C-DIR-RIGHT
                       IF GRID-CELL(GUARD-ROW, GUARD-COL + 1) = "#"
                       THEN
                           SET LS-GUARD-DIR TO C-DIR-DOWN
                       ELSE
                           ADD 1 TO GUARD-COL
                       END-IF
                   WHEN C-DIR-DOWN
                       IF GRID-CELL(GUARD-ROW + 1, GUARD-COL) = "#"
                       THEN
                           SET LS-GUARD-DIR TO C-DIR-LEFT
                       ELSE
                           ADD 1 TO GUARD-ROW
                       END-IF
                   WHEN C-DIR-LEFT
                       IF GRID-CELL(GUARD-ROW, GUARD-COL - 1) = "#"
                       THEN
                           SET LS-GUARD-DIR TO C-DIR-UP
                       ELSE
                           ADD -1 TO GUARD-COL
                       END-IF
               END-EVALUATE
               IF GUARD-ROW = 0 OR GUARD-ROW = GRID-SIZE + 1
                   OR GUARD-COL = 0 OR GUARD-COL = GRID-SIZE + 1
               THEN
                   display "exited at " GUARD-ROW "," GUARD-COL
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *> Calculate the number of X's we marked
           PERFORM VARYING GRID-ROW-INDEX
               FROM 1 BY 1 UNTIL GRID-ROW-INDEX > GRID-SIZE
               SET LS-ROW-X-COUNT TO 0
               INSPECT GRID-ROW(GRID-ROW-INDEX)
                   TALLYING LS-ROW-X-COUNT
                   FOR ALL "X"
               ADD LS-ROW-X-COUNT TO LS-TOTAL-X-COUNT
           END-PERFORM

           DISPLAY "Guard crossed " LS-TOTAL-X-COUNT " locations."

           GOBACK.

       END PROGRAM DAY06.

      *> ===============================================================
      *> PARSE-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-GRID.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA EXTERNAL.
       01  F-DATA-RECORD                 PIC X(130).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                       PIC X(130).
       LINKAGE SECTION.
           COPY "grid" IN "06".

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP.
           OPEN INPUT FD-DATA

           SET GRID-ROW-INDEX TO 1
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-DATA-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-DATA-RECORD TO LS-LINE
                       IF GRID-SIZE = 0
                       THEN
                           COMPUTE GRID-SIZE = LENGTH OF FUNCTION
                               TRIM(LS-LINE)
                       END-IF
                       MOVE LS-LINE TO GRID-ROW(
                           GRID-ROW-INDEX
                       )
      *> Look for the guard position if we don't have it already.
                       IF GUARD-ROW = 0
                       THEN
                           PERFORM VARYING GRID-COL-INDEX
                               FROM 1 BY 1
                               UNTIL GRID-COL-INDEX > GRID-SIZE
                               IF GRID-CELL(
                                   GRID-ROW-INDEX,
                                   GRID-COL-INDEX
                               ) = "^"
                               THEN
                                   SET GUARD-ROW TO GRID-ROW-INDEX
                                   SET GUARD-COL TO GRID-COL-INDEX
                               END-IF
                           END-PERFORM
                       END-IF
                       ADD 1 TO GRID-ROW-INDEX
           END-PERFORM
           CLOSE FD-DATA

           GOBACK.

       END PROGRAM PARSE-GRID.
