       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY15.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-SCORE                  PIC 9(15).
       COPY "grid" IN "15".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PROCESS-FILE" USING
               BY REFERENCE LS-FILE-PATH
               GRID-GRP

           CALL "CALCULATE-SCORE" USING
               BY REFERENCE GRID-GRP
               LS-SCORE

           DISPLAY "Score: " LS-SCORE
           CALL "DISPLAY-GRID" USING
               BY REFERENCE GRID-GRP
       .
       END PROGRAM DAY15.

      *> ===============================================================
      *> PROCESS-FILE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO IN-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD             PIC X(1000).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(1000).
       01  LS-ROW                    PIC 9(2) VALUE 0.
       01  LS-COL                    PIC 9(2) VALUE 0.
       01  LS-INSTR-PTR              PIC 9(4).
       01  LS-MOVE                   PIC X(1).
       01  LS-SCORE                  PIC 9(15) VALUE 0.

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "grid" IN "15".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           GRID-GRP.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       IF GRID-SIZE = 0
                           SET GRID-SIZE TO LENGTH OF FUNCTION
                               TRIM(LS-LINE)
                       END-IF
                       ADD 1 TO LS-ROW
                       IF LS-ROW <= GRID-SIZE
                           MOVE LS-LINE TO GRID-ROWS(LS-ROW)
                           PERFORM VARYING LS-COL FROM 1 BY 1
                               UNTIL LS-COL > GRID-SIZE
                               IF GRID-CELL(LS-ROW,
                                   LS-COL) = "@"
                                   MOVE LS-ROW TO ROBOT-ROW
                                   MOVE LS-COL TO ROBOT-COL
                               END-IF
                           END-PERFORM
                       END-IF
                       IF LS-ROW > GRID-SIZE AND LS-LINE NOT =
                           SPACE
                           PERFORM VARYING LS-INSTR-PTR FROM 1 BY 1
                               UNTIL LS-INSTR-PTR > LENGTH OF FUNCTION
                                   TRIM(LS-LINE)

                               MOVE LS-LINE(LS-INSTR-PTR:1) TO LS-MOVE
                               CALL "PROCESS-MOVE" USING
                                   GRID-GRP
                                   LS-MOVE
                               CALL "CALCULATE-SCORE" USING
                                   BY REFERENCE GRID-GRP
                                   LS-SCORE

                           END-PERFORM
                       END-IF
           END-PERFORM
           CLOSE FD-DATA

           GOBACK.
       END PROGRAM PROCESS-FILE.
      *> ===============================================================
      *> PROCESS-MOVE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-MOVE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ROW                      PIC 9(2).
       01  LS-COL                      PIC 9(2).
       01  LS-DIRECTION                PIC S9(1).

       LINKAGE SECTION.
       COPY "grid" IN "15".
       01  IN-MOVE                     PIC X(1).

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP
           IN-MOVE.

      *>     DISPLAY "MOVE " IN-MOVE

           MOVE ROBOT-ROW TO LS-ROW
           MOVE ROBOT-COL TO LS-COL

           PERFORM FOREVER
               EVALUATE IN-MOVE
                   WHEN "^"
                       ADD -1 TO LS-ROW
                   WHEN ">"
                       ADD 1 TO LS-COL
                   WHEN "v"
                       ADD 1 TO LS-ROW
                   WHEN "<"
                       ADD -1 TO LS-COL
               END-EVALUATE
      *> Stop if we've looked beyond the grid, or if we've found a wall
      *> or dot.
               IF LS-ROW = 0
                   OR LS-COL = 0
                   OR LS-ROW = GRID-SIZE + 1
                   OR LS-COL = GRID-SIZE + 1
                   OR GRID-CELL(LS-ROW, LS-COL) = "#"
                   OR GRID-CELL(LS-ROW, LS-COL) = "."

                   EXIT PERFORM
               END-IF
           END-PERFORM

      *> If we stopped at anything but a dot, exit.
           IF GRID-CELL(LS-ROW, LS-COL) NOT = "."
               GOBACK
           END-IF

      *> Move all Os between the robot position and the dot, towards
      *> the dot.
      *> Example
      *>
      *> Move >:
      *> Before: #..@OO.#
      *> After:  #...@OO#
      *>
      *> Before: #..@...#
      *> After:  #...@..#
       EVALUATE IN-MOVE
           WHEN "^"
           WHEN "v"
               IF IN-MOVE = "^"
                   SET LS-DIRECTION TO 1
               ELSE
                   SET LS-DIRECTION TO -1
               END-IF

               PERFORM VARYING LS-ROW FROM LS-ROW BY LS-DIRECTION
                   UNTIL LS-ROW = ROBOT-ROW
                   MOVE GRID-CELL(LS-ROW + LS-DIRECTION, LS-COL)
                       TO GRID-CELL(LS-ROW, LS-COL)
               END-PERFORM
               MOVE "." TO GRID-CELL(ROBOT-ROW, ROBOT-COL)
               COMPUTE ROBOT-ROW = ROBOT-ROW - LS-DIRECTION
           WHEN ">"
           WHEN "<"
               IF IN-MOVE = ">"
                   SET LS-DIRECTION TO -1
               ELSE
                   SET LS-DIRECTION TO 1
               END-IF
               PERFORM VARYING LS-COL FROM LS-COL BY LS-DIRECTION
                   UNTIL LS-COL = ROBOT-COL
                   MOVE GRID-CELL(LS-ROW, LS-COL + LS-DIRECTION)
                       TO GRID-CELL(LS-ROW, LS-COL)
               END-PERFORM
               MOVE "." TO GRID-CELL(ROBOT-ROW, ROBOT-COL)
               COMPUTE ROBOT-COL = ROBOT-COL - LS-DIRECTION
       END-EVALUATE
       MOVE "@" TO GRID-CELL(ROBOT-ROW, ROBOT-COL)

       GOBACK.
       END PROGRAM PROCESS-MOVE.

      *> ===============================================================
      *> CALCULATE-SCORE
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-SCORE.

       DATA DIVISION.

       LINKAGE SECTION.
       COPY "grid" IN "15".
       01  OUT-SCORE             PIC 9(15).

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP
           OUT-SCORE.

           SET OUT-SCORE TO 0

           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-SIZE
                   IF GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX) = "O"
                       COMPUTE OUT-SCORE = OUT-SCORE +
                           (100 * (GRID-ROW-INDEX - 1)) +
                           (GRID-COL-INDEX - 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           GOBACK.
       END PROGRAM CALCULATE-SCORE.

      *> ===============================================================
      *> DISPLAY-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-GRID.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "grid" IN "15".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.

           DISPLAY "size " GRID-SIZE
           DISPLAY "Robot @ " ROBOT-ROW "," ROBOT-COL

           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-SIZE
                   DISPLAY GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACES
           END-PERFORM
           GOBACK.
       END PROGRAM DISPLAY-GRID.
