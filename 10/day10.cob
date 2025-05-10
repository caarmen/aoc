       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY10.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "constants" IN "10".

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(30).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-SCORE-METHOD           PIC 9(1).
       01  LS-PART                   PIC 9(1).
       01  LS-TOTAL-SCORE            PIC 9(5).
       COPY "grid" IN "10".

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " "
               INTO LS-PART, LS-FILE-PATH
           END-UNSTRING

           IF LS-PART = 1
           THEN
               SET LS-SCORE-METHOD TO C-METHOD-SCORE
           ELSE
               SET LS-SCORE-METHOD TO C-METHOD-DISTINCT
           END-IF

           CALL "PARSE-GRID" USING
               BY REFERENCE LS-FILE-PATH
               BY REFERENCE GRID-GRP

      *>     CALL "DISPLAY-GRID" USING
      *>         BY REFERENCE GRID-GRP

           CALL "PROCESS-GRID" USING
               BY REFERENCE GRID-GRP
               BY REFERENCE LS-SCORE-METHOD
               RETURNING LS-TOTAL-SCORE

           DISPLAY "Total score: " LS-TOTAL-SCORE

           .
       END PROGRAM DAY10.


      *> ===============================================================
      *> PARSE-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-GRID.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO IN-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD             PIC X(47).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(47).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "grid" IN "10".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           BY REFERENCE GRID-GRP.

           OPEN INPUT FD-DATA
           SET GRID-ROW-INDEX TO 0
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       ADD 1 TO GRID-ROW-INDEX
                       MOVE F-FILE-RECORD TO LS-LINE
                       IF GRID-SIZE = 0
                       THEN
                           COMPUTE GRID-SIZE = LENGTH OF FUNCTION
                               TRIM(LS-LINE)
                       END-IF

                       PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                           UNTIL GRID-COL-INDEX > GRID-SIZE
                           MOVE LS-LINE(GRID-COL-INDEX:1) TO
                               GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                           IF GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                               = 0
                           THEN
                               ADD 1 TO TRAIL-HEADS-SIZE
                               SET TRAIL-HEAD-ROW(TRAIL-HEADS-SIZE) TO
                                   GRID-ROW-INDEX
                               SET TRAIL-HEAD-COL(TRAIL-HEADS-SIZE) TO
                                   GRID-COL-INDEX
                           END-IF
                       END-PERFORM

           END-PERFORM
           CLOSE FD-DATA

           GOBACK.
       END PROGRAM PARSE-GRID.

      *> ===============================================================
      *> DISPLAY-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-GRID.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "grid" IN "10".

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP.
           DISPLAY "Grid size " GRID-SIZE

           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1 UNTIL
               GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1 UNTIL
                   GRID-COL-INDEX > GRID-SIZE

                   DISPLAY GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM

           DISPLAY "Trail heads:"
           PERFORM VARYING TRAIL-HEADS-INDEX FROM 1 BY 1
               UNTIL TRAIL-HEADS-INDEX > TRAIL-HEADS-SIZE
               DISPLAY TRAIL-HEAD-ROW(TRAIL-HEADS-INDEX)
                   "," TRAIL-HEAD-COL(TRAIL-HEADS-INDEX)

           END-PERFORM

           GOBACK.
       END PROGRAM DISPLAY-GRID.

      *> ===============================================================
      *> PROCESS-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-GRID.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-TRAIL-SCORE                      PIC 9(5) VALUE 0.
       01  LS-TOTAL-SCORE                      PIC 9(5) VALUE 0.

       LINKAGE SECTION.
       COPY "grid" IN "10".
       01  IN-SCORE-METHOD                     PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP
           BY REFERENCE IN-SCORE-METHOD
           .

           PERFORM VARYING TRAIL-HEADS-INDEX FROM 1 BY 1
               UNTIL TRAIL-HEADS-INDEX > TRAIL-HEADS-SIZE
               CALL "PROCESS-TRAIL" USING
                   BY REFERENCE GRID-GRP
                   BY REFERENCE TRAIL-HEAD-ROW(TRAIL-HEADS-INDEX)
                   BY REFERENCE TRAIL-HEAD-COL(TRAIL-HEADS-INDEX)
                   BY REFERENCE IN-SCORE-METHOD
                   RETURNING LS-TRAIL-SCORE

               ADD LS-TRAIL-SCORE TO LS-TOTAL-SCORE
           END-PERFORM

           MOVE LS-TOTAL-SCORE TO RETURN-CODE

           GOBACK.
       END PROGRAM PROCESS-GRID.

      *> ===============================================================
      *> PROCESS-TRAIL.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-TRAIL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "10".

       LOCAL-STORAGE SECTION.
       01  LS-SCORE                            PIC 9(5) VALUE 0.
       01  LS-CUR-ROW                          PIC 9(2).
       01  LS-CUR-COL                          PIC 9(2).
       01  LS-CUR-HEIGHT                       PIC 9(1).
       01  LS-NEIGHBOR-ROW                     PIC 9(2).
       01  LS-NEIGHBOR-COL                     PIC 9(2).
       01  LS-NEIGHBOR-HEIGHT                  PIC 9(1).
       COPY "stack" IN "10".

       LINKAGE SECTION.
       COPY "grid" IN "10".
       01  IN-TRAIL-HEAD-ROW                   PIC 9(2).
       01  IN-TRAIL-HEAD-COL                   PIC 9(2).
       01  IN-SCORE-METHOD                     PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP
           BY REFERENCE IN-TRAIL-HEAD-ROW
           BY REFERENCE IN-TRAIL-HEAD-COL
           BY REFERENCE IN-SCORE-METHOD
           .

      *> Mark all the cells as not visited
           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               AFTER GRID-COL-INDEX FROM 1 BY 1
               UNTIL GRID-COL-INDEX > GRID-SIZE
               SET GRID-CELL-VISITED(GRID-ROW-INDEX, GRID-COL-INDEX) TO
                   0
           END-PERFORm


           CALL "PUSH-TO-STACK" USING
               BY REFERENCE IN-TRAIL-HEAD-ROW
               BY REFERENCE IN-TRAIL-HEAD-COL
               BY REFERENCE STACK-GRP

           PERFORM UNTIL EXIT
               CALL "POP-STACK" USING
                   BY REFERENCE STACK-GRP
                   BY REFERENCE LS-CUR-ROW
                   BY REFERENCE LS-CUR-COL

               SET LS-CUR-HEIGHT TO GRID-CELL(LS-CUR-ROW, LS-CUR-COL)

               IF RETURN-CODE NOT = 0
               THEN
                   EXIT PERFORM
               END-IF

      *> Skip the node if it's already been visited, unless
      *> we're counting distinct trails.
               IF IN-SCORE-METHOD=C-METHOD-DISTINCT OR
                   GRID-CELL-VISITED(LS-CUR-ROW, LS-CUR-COL) = 0
               THEN
                   SET GRID-CELL-VISITED(LS-CUR-ROW, LS-CUR-COL) TO 1
                   IF LS-CUR-HEIGHT = 9
                   THEN
                       ADD 1 TO LS-SCORE
                   END-IF
      *> Check cell above
                   IF LS-CUR-ROW > 1
                       COMPUTE LS-NEIGHBOR-ROW = LS-CUR-ROW - 1
                       COMPUTE LS-NEIGHBOR-COL = LS-CUR-COL
                       SET LS-NEIGHBOR-HEIGHT TO GRID-CELL(
                           LS-NEIGHBOR-ROW
                           LS-NEIGHBOR-COL
                       )
                       IF LS-NEIGHBOR-HEIGHT = LS-CUR-HEIGHT + 1
                       THEN
                           CALL "PUSH-TO-STACK" USING
                               BY REFERENCE LS-NEIGHBOR-ROW
                               BY REFERENCE LS-NEIGHBOR-COL
                               BY REFERENCE STACK-GRP
                       END-IF
                   END-IF
      *> Check cell to the right
                   IF LS-CUR-COL < GRID-SIZE
                       COMPUTE LS-NEIGHBOR-ROW = LS-CUR-ROW
                       COMPUTE LS-NEIGHBOR-COL = LS-CUR-COL + 1
                       SET LS-NEIGHBOR-HEIGHT TO GRID-CELL(
                           LS-NEIGHBOR-ROW
                           LS-NEIGHBOR-COL
                       )
                       IF LS-NEIGHBOR-HEIGHT = LS-CUR-HEIGHT + 1
                       THEN
                           CALL "PUSH-TO-STACK" USING
                               BY REFERENCE LS-NEIGHBOR-ROW
                               BY REFERENCE LS-NEIGHBOR-COL
                               BY REFERENCE STACK-GRP
                       END-IF
                   END-IF
      *> Check cell to the bottom
                   IF LS-CUR-ROW < GRID-SIZE
                       COMPUTE LS-NEIGHBOR-ROW = LS-CUR-ROW + 1
                       COMPUTE LS-NEIGHBOR-COL = LS-CUR-COL
                       SET LS-NEIGHBOR-HEIGHT TO GRID-CELL(
                           LS-NEIGHBOR-ROW
                           LS-NEIGHBOR-COL
                       )
                       IF LS-NEIGHBOR-HEIGHT = LS-CUR-HEIGHT + 1
                       THEN
                           CALL "PUSH-TO-STACK" USING
                               BY REFERENCE LS-NEIGHBOR-ROW
                               BY REFERENCE LS-NEIGHBOR-COL
                               BY REFERENCE STACK-GRP
                       END-IF
                   END-IF
      *> Check cell to the left
                   IF LS-CUR-COL > 1
                       COMPUTE LS-NEIGHBOR-ROW = LS-CUR-ROW
                       COMPUTE LS-NEIGHBOR-COL = LS-CUR-COL - 1
                       SET LS-NEIGHBOR-HEIGHT TO GRID-CELL(
                           LS-NEIGHBOR-ROW
                           LS-NEIGHBOR-COL
                       )
                       IF LS-NEIGHBOR-HEIGHT = LS-CUR-HEIGHT + 1
                       THEN
                           CALL "PUSH-TO-STACK" USING
                               BY REFERENCE LS-NEIGHBOR-ROW
                               BY REFERENCE LS-NEIGHBOR-COL
                               BY REFERENCE STACK-GRP
                       END-IF
                   END-IF
               END-IF


           END-PERFORM

           MOVE LS-SCORE TO RETURN-CODE

           GOBACK.
       END PROGRAM PROCESS-TRAIL.

      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "10".
       01  OUT-ITEM-ROW                    PIC 9(2).
       01  OUT-ITEM-COL                    PIC 9(2).

       PROCEDURE DIVISION USING
           BY REFERENCE STACK-GRP
           BY REFERENCE OUT-ITEM-ROW
           BY REFERENCE OUT-ITEM-COL
           .

           IF STACK-SIZE > 0
               MOVE STACK-ITEM-ROW(STACK-SIZE) TO OUT-ITEM-ROW
               MOVE STACK-ITEM-COL(STACK-SIZE) TO OUT-ITEM-COL
               COMPUTE STACK-SIZE = STACK-SIZE - 1
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 1 TO RETURN-CODE
           END-IF

           GOBACK.
       END PROGRAM POP-STACK.

      *> ===============================================================
      *> PUSH-TO-STACK.
      *> Add an item to the end of the stack
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PUSH-TO-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ITEM-ROW                     PIC 9(2).
       01  IN-ITEM-COL                     PIC 9(2).
       COPY "stack" IN "10".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ITEM-ROW
           BY REFERENCE IN-ITEM-COL
           BY REFERENCE STACK-GRP.

           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-ROW(STACK-SIZE) TO IN-ITEM-ROW
           SET STACK-ITEM-COL(STACK-SIZE) TO IN-ITEM-COL

           GOBACK.
       END PROGRAM PUSH-TO-STACK.


