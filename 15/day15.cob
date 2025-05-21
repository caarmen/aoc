       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY15.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(30).
       01  LS-PART                   PIC 9(1).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-SCORE                  PIC 9(15).
       COPY "grid" IN "15".

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " "
               INTO LS-PART LS-FILE-PATH
           END-UNSTRING

           CALL "PROCESS-FILE" USING BY REFERENCE
               LS-PART
               LS-FILE-PATH
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
       01  LS-LINE-PTR               PIC 9(3).
       01  LS-ROW                    PIC 9(2) VALUE 0.
       01  LS-COL                    PIC 9(2) VALUE 0.
       01  LS-INSTR-PTR              PIC 9(4).
       01  LS-MOVE                   PIC X(1).
       01  LS-SCORE                  PIC 9(15) VALUE 0.

       LINKAGE SECTION.
       01  IN-PART                   PIC 9(1).
       01  IN-FILE-PATH              PIC X(30).
       COPY "grid" IN "15".

       PROCEDURE DIVISION USING BY REFERENCE
           IN-PART
           IN-FILE-PATH
           GRID-GRP.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       IF GRID-WIDTH = 0
                           COMPUTE GRID-HEIGHT =
                               LENGTH OF FUNCTION TRIM(LS-LINE)
                           COMPUTE GRID-WIDTH = IN-PART *
                               GRID-HEIGHT
                       END-IF
                       ADD 1 TO LS-ROW
                       IF LS-ROW <= GRID-HEIGHT
      *> For part 1, it's simpler: we can write the line directly
      *> to the grid row.
                           IF IN-PART = 1
                               MOVE LS-LINE TO GRID-ROWS(LS-ROW)
                               PERFORM VARYING LS-COL FROM 1 BY 1
                                  UNTIL LS-COL > GRID-WIDTH
                                  IF GRID-CELL(LS-ROW, LS-COL) = "@"
                                      MOVE LS-ROW TO ROBOT-ROW
                                      MOVE LS-COL TO ROBOT-COL
                                  END-IF
                              END-PERFORM
      *> For part 2, we need to transform the input, duplicating
      *> cells in our grid.
                           ELSE
                               SET LS-COL TO 0
                               PERFORM VARYING LS-LINE-PTR FROM 1 BY 1
                                   UNTIL LS-LINE-PTR >
                                       LENGTH OF FUNCTION TRIM(LS-LINE)
                                   ADD 1 TO LS-COL
                                   EVALUATE LS-LINE(LS-LINE-PTR:1)
                                       WHEN "#"
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "#"
                                           ADD 1 TO LS-COL
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "#"
                                       WHEN "O"
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "["
                                           ADD 1 TO LS-COL
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "]"
                                       WHEN "."
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "."
                                           ADD 1 TO LS-COL
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "."
                                       WHEN "@"
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "@"
                                           MOVE LS-ROW TO ROBOT-ROW
                                           MOVE LS-COL TO ROBOT-COL
                                           ADD 1 TO LS-COL
                                           SET GRID-CELL(LS-ROW, LS-COL)
                                               TO "."
                                   END-EVALUATE
                               END-PERFORM
                           END-IF
                       END-IF
                       IF LS-LINE = SPACE
                           CALL "DISPLAY-GRID" USING
                               GRID-GRP
                       END-IF
                       IF LS-ROW > GRID-HEIGHT AND LS-LINE NOT = SPACE
                           PERFORM VARYING LS-INSTR-PTR FROM 1 BY 1
                               UNTIL LS-INSTR-PTR > LENGTH OF FUNCTION
                                   TRIM(LS-LINE)

                               MOVE LS-LINE(LS-INSTR-PTR:1) TO LS-MOVE
                               CALL "PROCESS-MOVE" USING
                                   GRID-GRP
                                   LS-MOVE
                                   IN-PART
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
       01  LS-MOVE-VERTICAL            PIC 9(1).

       LINKAGE SECTION.
       COPY "grid" IN "15".
       01  IN-MOVE                     PIC X(1).
       01  IN-PART                     PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP
           IN-MOVE
           IN-PART.

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
                   OR LS-ROW = GRID-HEIGHT + 1
                   OR LS-COL = GRID-WIDTH + 1
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
       EVALUATE IN-MOVE ALSO IN-PART
      *> Vertical movements are different for parts 1 and 2.
           WHEN "^" ALSO 1
           WHEN "v" ALSO 1
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
           WHEN "^" ALSO 2
           WHEN "v" ALSO 2
               IF IN-MOVE = "^"
                   SET LS-DIRECTION TO -1
               ELSE
                   SET LS-DIRECTION TO 1
               END-IF
               CALL "MOVE-VERTICAL" USING
                   BY REFERENCE GRID-GRP
                   LS-DIRECTION
                   RETURNING LS-MOVE-VERTICAL
               IF LS-MOVE-VERTICAL NOT = 0
                   GOBACK
               END-IF
      *> Horizontal movements are the same for parts 1 and 2.
           WHEN ">" ALSO ANY
           WHEN "<" ALSO ANY
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
      *> MOVE-VERTICAL.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE-VERTICAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PUSH-TO-STACK
           FUNCTION POP-STACK.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ROW                      PIC 9(3).
       01  LS-COL                      PIC 9(3).
       01  LS-POP-RESULT               PIC 9(1).
       01  LS-PUSH-RESULT              PIC 9(1).
       01  LS-POS-INT                  PIC 9(6).
       01  NEW-POS-GRP.
           05  NEW-POS-SIZE            PIC 9(3) VALUE 0.
           05  NEW-POS-LIST OCCURS 1 TO 999
               DEPENDING ON NEW-POS-SIZE
               ASCENDING KEY IS NEW-POS
               INDEXED BY NEW-POS-INDEX.
               10  NEW-POS             PIC 9(6).
       COPY "stack" IN "15".

       LINKAGE SECTION.
       COPY "grid" IN "15".
       01  IN-DIRECTION                PIC S9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP
           IN-DIRECTION.

      *> Traverse all cells going up, following any
      *> boxes we run into, until we reach a "."
      *> (ok to move up to this cell) or a "#"
      *> (not ok to move up to this cell).

      *> Keep track of all the visited cells in NEW-POS
           SET RETURN-CODE TO 0
           SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                   ROBOT-ROW
                   ROBOT-COL
                   STACK-GRP)

           PERFORM UNTIL STACK-SIZE = 0
               SET LS-POP-RESULT TO POP-STACK(
                   STACK-GRP
                   LS-ROW
                   LS-COL)
               IF LS-POP-RESULT = 1
                   EXIT PERFORM
               END-IF


               SET NEW-POS-INDEX TO 0
               COMPUTE LS-POS-INT = LS-ROW * 1000 + LS-COL
               SEARCH NEW-POS-LIST
                   VARYING NEW-POS-INDEX
                   AT END
                       ADD 1 TO NEW-POS-SIZE
                       SET NEW-POS(NEW-POS-SIZE) TO LS-POS-INT
                   WHEN NEW-POS(NEW-POS-INDEX) = LS-POS-INT
                       CONTINUE
               END-SEARCH
               COMPUTE LS-ROW = LS-ROW + IN-DIRECTION
               EVALUATE GRID-CELL(LS-ROW, LS-COL)
      *> Simple case: we're going up to occupy an empty cell.
      *> ....
      *> ..@.
                   WHEN "."
                       CONTINUE
      *> .[].
      *> ..@.
                   WHEN "]"
                       SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                           LS-ROW
                           LS-COL
                           STACK-GRP)

                       COMPUTE LS-COL = LS-COL - 1
                       SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                           LS-ROW
                           LS-COL
                           STACK-GRP)

      *> .[].
      *> .@..
                   WHEN "["
                       SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                           LS-ROW
                           LS-COL
                           STACK-GRP)
                       COMPUTE LS-COL = LS-COL + 1
                       SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                           LS-ROW
                           LS-COL
                           STACK-GRP)
                   WHEN "#"
                       MOVE 1 TO RETURN-CODE
                       GOBACK
               END-EVALUATE

           END-PERFORM

      *> Go through all the NEW-POS positions in order,
      *> moving boxes or the robot.
           SET NEW-POS-INDEX TO 0
           IF IN-DIRECTION = -1
               SORT NEW-POS-LIST ASCENDING NEW-POS
           ELSE
               SORT NEW-POS-LIST DESCENDING NEW-POS
           END-IF
           PERFORM VARYING NEW-POS-INDEX FROM 1 BY 1
               UNTIL NEW-POS-INDEX > NEW-POS-SIZE
               COMPUTE LS-ROW = NEW-POS(NEW-POS-INDEX) / 1000
               COMPUTE LS-COL =
                   FUNCTION MOD(NEW-POS(NEW-POS-INDEX), 1000)
               SET GRID-CELL(LS-ROW + IN-DIRECTION, LS-COL) TO
                   GRID-CELL(LS-ROW, LS-COL)
               SET GRID-CELL(LS-ROW, LS-COL) TO "."
               IF GRID-CELL(LS-ROW + IN-DIRECTION, LS-COL) = "@"
                   COMPUTE ROBOT-ROW = LS-ROW + IN-DIRECTION
               END-IF
           END-PERFORM

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM MOVE-VERTICAL.

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
               UNTIL GRID-ROW-INDEX > GRID-HEIGHT
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-WIDTH
                   IF GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX) = "O"
                       OR GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX) =
                           "["
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

           DISPLAY "size " GRID-HEIGHT "x" GRID-WIDTH
           DISPLAY "Robot @ " ROBOT-ROW "," ROBOT-COL

           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-HEIGHT
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-WIDTH
                   DISPLAY GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACES
           END-PERFORM
           GOBACK.
       END PROGRAM DISPLAY-GRID.


      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "10".
       01  OUT-ITEM-ROW                    PIC 9(3).
       01  OUT-ITEM-COL                    PIC 9(3).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE STACK-GRP OUT-ITEM-ROW OUT-ITEM-COL
           RETURNING OUT-RESULT
           .

           IF STACK-SIZE > 0
               MOVE STACK-ITEM-ROW(STACK-SIZE) TO OUT-ITEM-ROW
               MOVE STACK-ITEM-COL(STACK-SIZE) TO OUT-ITEM-COL
               COMPUTE STACK-SIZE = STACK-SIZE - 1
               MOVE 0 TO OUT-RESULT
           ELSE
               MOVE 1 TO OUT-RESULT
           END-IF
           GOBACK.

       END FUNCTION POP-STACK.

      *> ===============================================================
      *> PUSH-TO-STACK.
      *> Add an item to the end of the stack
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. PUSH-TO-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ITEM-ROW                     PIC 9(3).
       01  IN-ITEM-COL                     PIC 9(3).
       COPY "stack" IN "10".
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ITEM-ROW IN-ITEM-COL STACK-GRP
           RETURNING OUT-RESULT.

           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-ROW(STACK-SIZE) TO IN-ITEM-ROW
           SET STACK-ITEM-COL(STACK-SIZE) TO IN-ITEM-COL

           MOVE 0 TO OUT-RESULT
           GOBACK.
       END FUNCTION PUSH-TO-STACK.
