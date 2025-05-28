       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY18.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(30).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-GRID-SIZE              PIC 9(2).
       01  LS-BLOCK-KEEP-COUNT       PIC 9(4).
       COPY "grid" IN "18".
       COPY "block" IN "18".


       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " " INTO
               LS-GRID-SIZE
               LS-BLOCK-KEEP-COUNT
               LS-FILE-PATH
           END-UNSTRING

           COMPUTE GRID-SIZE = LS-GRID-SIZE + 1

           CALL "PARSE-FILE" USING BY REFERENCE
               LS-FILE-PATH
               BLOCKS-GRP

           CALL "FILL-GRID" USING BY REFERENCE
               GRID-GRP
               BLOCKS-GRP
               LS-BLOCK-KEEP-COUNT

           CALL "DISPLAY-GRID" USING BY REFERENCE
               GRID-GRP

           display "----"
           CALL "PROCESS-GRID" USING BY REFERENCE
               GRID-GRP
               BLOCKS-GRP

           display "----"
           CALL "DISPLAY-GRID" USING BY REFERENCE
               GRID-GRP

           .
       END PROGRAM DAY18.

      *> ===============================================================
      *> PARSE-FILE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-FILE.

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
       01  LS-ROW                    PIC 9(2).
       01  LS-COL                    PIC 9(2).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "block" IN "18".

       PROCEDURE DIVISION USING BY REFERENCE
           IN-FILE-PATH
           BLOCKS-GRP.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       ADD 1 TO BLOCKS-SIZE
                       UNSTRING LS-LINE DELIMITED BY ","
                           INTO LS-COL LS-ROW
                       END-UNSTRING
                       COMPUTE BLOCK-ROW(BLOCKS-SIZE) = LS-ROW + 1
                       COMPUTE BLOCK-COL(BLOCKS-SIZE) = LS-COL + 1
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.
      *> ===============================================================
      *> FILL-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILL-GRID.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "grid" IN "18".
       COPY "block" IN "18".
       01  IN-BLOCK-KEEP-COUNT                 PIC 9(4).

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP
           BLOCKS-GRP
           IN-BLOCK-KEEP-COUNT.

           SET GRID-CELL(1, 1) TO "S"
           SET GRID-CELL(GRID-SIZE, GRID-SIZE) TO "E"
           PERFORM VARYING BLOCK-INDEX FROM 1 BY 1
               UNTIL BLOCK-INDEX > IN-BLOCK-KEEP-COUNT
               SET GRID-CELL(
                   BLOCK-ROW(BLOCK-INDEX),
                   BLOCK-COL(BLOCK-INDEX)
               ) TO "#"
           END-PERFORM

           GOBACK.
       END PROGRAM FILL-GRID.

      *> ===============================================================
      *> PROCESS-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-GRID.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       COPY "visited" IN "18".
       COPY "queue" IN "18".
       01  LS-QUEUE-VALUE                      PIC 9(4).
       01  LS-ROW                              PIC 9(2).
       01  LS-COL                              PIC 9(2).
       01  LS-DIST                             PIC 9(4).
       01  LS-VISIT-RESULT                     PIC 9(1).
       01  LS-NEIGHBOR-ROW                     PIC 9(2).
       01  LS-NEIGHBOR-COL                     PIC 9(2).

       LINKAGE SECTION.
       COPY "grid" IN "18".
       COPY "block" IN "18".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP
           BLOCKS-GRP.

           SET LS-ROW TO 1
           SET LS-COL TO 1
           SET LS-DIST TO 0

           CALL "ENQUEUE" USING BY REFERENCE
               QUEUE-GRP
               LS-ROW
               LS-COL
               LS-DIST

           CALL "VISIT" USING BY REFERENCE
               VISITED-GRP
               LS-ROW
               LS-COL

           PERFORM UNTIL QUEUE-SIZE = 0
               CALL "DEQUEUE" USING BY REFERENCE
                   QUEUE-GRP
                   LS-ROW
                   LS-COL
                   LS-DIST
               IF LS-ROW = GRID-SIZE AND LS-COL = GRID-SIZE
                   DISPLAY "Reached exit " LS-DIST
                   EXIT PERFORM
               END-IF
               SET GRID-CELL(LS-ROW, LS-COL) TO "O"
      
               ADD 1 TO LS-DIST
      *> Check top neighbor
               COMPUTE LS-NEIGHBOR-ROW = LS-ROW - 1
               COMPUTE LS-NEIGHBOR-COL = LS-COL
               PERFORM TRY-NEIGHBOR
      *> Check right neighbor
               COMPUTE LS-NEIGHBOR-ROW = LS-ROW
               COMPUTE LS-NEIGHBOR-COL = LS-COL + 1
               PERFORM TRY-NEIGHBOR
      *> Check bottom neighbor
               COMPUTE LS-NEIGHBOR-ROW = LS-ROW + 1
               COMPUTE LS-NEIGHBOR-COL = LS-COL
               PERFORM TRY-NEIGHBOR
      *> Check left neighbor
               COMPUTE LS-NEIGHBOR-ROW = LS-ROW
               COMPUTE LS-NEIGHBOR-COL = LS-COL - 1
               PERFORM TRY-NEIGHBOR
           END-PERFORM
               

           GOBACK.

           TRY-NEIGHBOR.

           IF LS-NEIGHBOR-ROW >= 1
               AND LS-NEIGHBOR-ROW <= GRID-SIZE
               AND LS-NEIGHBOR-COL >= 1
               AND LS-NEIGHBOR-COL <= GRID-SIZE
               AND (
                   GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = " "
                   OR
                   GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "E"
               )
               CALL "VISIT" USING BY REFERENCE
                   VISITED-GRP
                   LS-NEIGHBOR-ROW
                   LS-NEIGHBOR-COL
                   RETURNING
                   LS-VISIT-RESULT
               IF LS-VISIT-RESULT = 0
                   CALL "ENQUEUE" USING BY REFERENCE
                       QUEUE-GRP
                       LS-NEIGHBOR-ROW
                       LS-NEIGHBOR-COL
                       LS-DIST
               END-IF
           END-IF
           .

       END PROGRAM PROCESS-GRID.

      *> ===============================================================
      *> DISPLAY-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-GRID.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "grid" IN "18".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.

           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-SIZE
                   DISPLAY GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM

           GOBACK.

       END PROGRAM DISPLAY-GRID.

      *> ===============================================================
      *> VISIT.
      *> Return 0 if the node has just been marked as visited, 1 if it
      *> was already marked as visited.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VISIT.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "visited" IN "18".
       01  IN-ROW                           PIC 9(2).
       01  IN-COL                           PIC 9(2).
       PROCEDURE DIVISION USING BY REFERENCE
           VISITED-GRP
           IN-ROW
           IN-COL.

           PERFORM VARYING VISITED-INDEX FROM 1 BY 1
               UNTIL VISITED-INDEX > VISITED-SIZE

               IF VISITED-ROW(VISITED-INDEX) = IN-ROW
                   AND VISITED-COL(VISITED-INDEX) = IN-COL
                   MOVE 1 TO RETURN-CODE
                   GOBACK
               END-IF
           END-PERFORM

           ADD 1 TO VISITED-SIZE
           SET VISITED-ROW(VISITED-SIZE) TO IN-ROW
           SET VISITED-COL(VISITED-SIZE) TO IN-COL

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM VISIT.

      *> ===============================================================
      *> ENQUEUE.
      *>
      *> Return 0 if the item was enqueued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENQUEUE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queue" IN "18".
       01 IN-QUEUE-VALUE-ROW                 PIC 9(2).
       01 IN-QUEUE-VALUE-COL                 PIC 9(2).
       01 IN-QUEUE-VALUE-DIST                PIC 9(4).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           IN-QUEUE-VALUE-ROW
           IN-QUEUE-VALUE-COL
           IN-QUEUE-VALUE-DIST
           .

           IF QUEUE-SIZE = QUEUE-MAX-SIZE
               DISPLAY "Queue full"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           ADD 1 TO QUEUE-SIZE
           COMPUTE QUEUE-TAIL = FUNCTION MOD(QUEUE-TAIL + 1,
               QUEUE-MAX-SIZE)

           SET QUEUE-VALUE-ROW(QUEUE-TAIL) TO IN-QUEUE-VALUE-ROW
           SET QUEUE-VALUE-COL(QUEUE-TAIL) TO IN-QUEUE-VALUE-COL
           SET QUEUE-VALUE-DIST(QUEUE-TAIL) TO IN-QUEUE-VALUE-DIST

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM ENQUEUE.

      *> ===============================================================
      *> DEQUEUE.
      *>
      *> Return 0 if the item was dequeued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEQUEUE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queue" IN "18".
       01 OUT-QUEUE-VALUE-ROW                  PIC 9(2).
       01 OUT-QUEUE-VALUE-COL                  PIC 9(2).
       01 OUT-QUEUE-VALUE-DIST                 PIC 9(4).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           OUT-QUEUE-VALUE-ROW
           OUT-QUEUE-VALUE-COL
           OUT-QUEUE-VALUE-DIST.

           IF QUEUE-SIZE = 0
               DISPLAY "Queue empty"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           SET OUT-QUEUE-VALUE-ROW TO QUEUE-VALUE-ROW(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-COL TO QUEUE-VALUE-COL(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-DIST TO QUEUE-VALUE-DIST(QUEUE-HEAD)

           SUBTRACT 1 FROM QUEUE-SIZE
           COMPUTE QUEUE-HEAD = FUNCTION MOD(QUEUE-HEAD + 1,
               QUEUE-MAX-SIZE)

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM DEQUEUE.

