       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY20.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-FULL-PATH-LENGTH       PIC 9(5).
       01  LS-CHEAT-PATH-LENGTH      PIC 9(5).
       01  LS-PATH-SAVED             PIC 9(5).
       01  LS-CHEAT-PATH-START-INIT  PIC X(1).
       01  LS-CHEAT-PATH-END-INIT    PIC X(1).
       01  LS-TOTAL-BIG-CHEATS       PIC 9(5) VALUE 0.
       COPY "grid" IN "20".
       COPY "cheat" IN "20".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               GRID-GRP.

           CALL "DISPLAY-GRID" USING
               BY REFERENCE GRID-GRP

           CALL "PROCESS-GRID" USING
               BY REFERENCE GRID-GRP
               RETURNING LS-FULL-PATH-LENGTH
           display "full path length " ls-full-path-length

           CALL "CALCULATE-DISTS" USING
               BY REFERENCE GRID-GRP

           CALL "FIND-CHEATS" USING
               BY REFERENCE GRID-GRP
               CHEAT-GRP
           DISPLAY "Found " CHEAT-SIZE " cheats."

           PERFORM VARYING CHEAT-INDEX FROM 1 BY 1
               UNTIL CHEAT-INDEX > CHEAT-SIZE
               IF CHEAT-DISTANCE-SAVED(CHEAT-INDEX) > 0
                   DISPLAY CHEAT-START-ROW(CHEAT-INDEX) ","
                       CHEAT-START-COL(CHEAT-INDEX) " -> "
                       CHEAT-END-ROW(CHEAT-INDEX) ","
                       CHEAT-END-COL(CHEAT-INDEX) ": "
                       "Saved " CHEAT-DISTANCE-SAVED(CHEAT-INDEX)
               END-IF
               IF CHEAT-DISTANCE-SAVED(CHEAT-INDEX) >= 100
                   ADD 1 TO LS-TOTAL-BIG-CHEATS
               END-IF
           END-PERFORM

           CALL "DISPLAY-GRID" USING
               BY REFERENCE GRID-GRP

           DISPLAY LS-TOTAL-BIG-CHEATS " saved at least 100"

           .

       END PROGRAM DAY20.

      *> ===============================================================
      *> CALCULATE-DISTS.
      *> For all of the cells on the path, calculate the distance from
      *> that cell to the end.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-DISTS.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-PATH-NODE-ROW                    PIC 9(3).
       01  LS-PATH-NODE-COL                    PIC 9(3).
       01  LS-PATH-NODE-PARENT-ROW             PIC 9(3).
       01  LS-PATH-NODE-PARENT-COL             PIC 9(3).
       01  LS-DIST                             PIC 9(5) VALUE 0.

       LINKAGE SECTION.
       COPY "grid" IN "20".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.

      *> Proceed from the end, backward through the path, via parent
      *> cells.
           SET LS-PATH-NODE-ROW TO GRID-END-ROW
           SET LS-PATH-NODE-COL TO GRID-END-COL
           PERFORM UNTIL EXIT
               IF LS-PATH-NODE-ROW = 0
                   EXIT PERFORM
               END-IF
               SET GRID-DIST-TO-END(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               ) TO LS-DIST
               ADD 1 TO LS-DIST

      *> Go to the next parent
               SET LS-PATH-NODE-PARENT-ROW TO GRID-PATH-PARENT-ROW(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               )
               SET LS-PATH-NODE-PARENT-COL TO GRID-PATH-PARENT-COL(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               )
               SET LS-PATH-NODE-ROW TO LS-PATH-NODE-PARENT-ROW
               SET LS-PATH-NODE-COL TO LS-PATH-NODE-PARENT-COL
           END-PERFORM
           .

       END PROGRAM CALCULATE-DISTS.

      *> ===============================================================
      *> FIND-CHEATS.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-CHEATS.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-PATH-NODE-ROW                    PIC 9(3).
       01  LS-PATH-NODE-COL                    PIC 9(3).
       01  LS-PATH-NODE-PARENT-ROW             PIC 9(3).
       01  LS-PATH-NODE-PARENT-COL             PIC 9(3).
       01  LS-CHEAT-START-ROW                  PIC 9(3).
       01  LS-CHEAT-START-COL                  PIC 9(3).
       01  LS-CHEAT-END-ROW                    PIC 9(3).
       01  LS-CHEAT-END-COL                    PIC 9(3).

       01  LS-DIST-TO-CHEAT-START              PIC 9(5).
       01  LS-DIST-FROM-CHEAT-END              PIC 9(5).

       LINKAGE SECTION.
       COPY "grid" IN "20".
       COPY "cheat" IN "20".
       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP
           CHEAT-GRP.

      *> Proceed from the end, backward through the path, via parent
      *> cells.
           SET LS-PATH-NODE-ROW TO GRID-PATH-PARENT-ROW(
               GRID-END-ROW,
               GRID-END-COL
           )
           SET LS-PATH-NODE-COL TO GRID-PATH-PARENT-COL(
               GRID-END-ROW,
               GRID-END-COL
           )
           PERFORM UNTIL EXIT
               IF LS-PATH-NODE-ROW = 0
                   EXIT PERFORM
               END-IF

               IF GRID-CELL(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               ) = "." OR GRID-CELL(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               ) = "S"

      *> Look for possible cheats
      *> Try up
                   COMPUTE LS-CHEAT-START-ROW = LS-PATH-NODE-ROW - 1
                   COMPUTE LS-CHEAT-START-COL = LS-PATH-NODE-COL
                   COMPUTE LS-CHEAT-END-ROW = LS-PATH-NODE-ROW - 2
                   COMPUTE LS-CHEAT-END-COL = LS-PATH-NODE-COL
                   PERFORM ADD-CHEAT
      *> Try right
                   COMPUTE LS-CHEAT-START-ROW = LS-PATH-NODE-ROW
                   COMPUTE LS-CHEAT-START-COL = LS-PATH-NODE-COL + 1
                   COMPUTE LS-CHEAT-END-ROW = LS-PATH-NODE-ROW
                   COMPUTE LS-CHEAT-END-COL = LS-PATH-NODE-COL + 2
                   PERFORM ADD-CHEAT
      *> Try bottom
                   COMPUTE LS-CHEAT-START-ROW = LS-PATH-NODE-ROW + 1
                   COMPUTE LS-CHEAT-START-COL = LS-PATH-NODE-COL
                   COMPUTE LS-CHEAT-END-ROW = LS-PATH-NODE-ROW + 2
                   COMPUTE LS-CHEAT-END-COL = LS-PATH-NODE-COL
                   PERFORM ADD-CHEAT
      *> Try left
                   COMPUTE LS-CHEAT-START-ROW = LS-PATH-NODE-ROW
                   COMPUTE LS-CHEAT-START-COL = LS-PATH-NODE-COL - 1
                   COMPUTE LS-CHEAT-END-ROW = LS-PATH-NODE-ROW
                   COMPUTE LS-CHEAT-END-COL = LS-PATH-NODE-COL - 2
                   PERFORM ADD-CHEAT
               END-IF

      *> Go to the next parent
               SET LS-PATH-NODE-PARENT-ROW TO GRID-PATH-PARENT-ROW(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               )
               SET LS-PATH-NODE-PARENT-COL TO GRID-PATH-PARENT-COL(
                   LS-PATH-NODE-ROW,
                   LS-PATH-NODE-COL
               )
               SET LS-PATH-NODE-ROW TO LS-PATH-NODE-PARENT-ROW
               SET LS-PATH-NODE-COL TO LS-PATH-NODE-PARENT-COL

           END-PERFORM
           GOBACK.

       ADD-CHEAT.

      *> Check that we're within bounds
           IF  LS-CHEAT-START-ROW >= 1
               AND LS-CHEAT-START-ROW <= GRID-SIZE
               AND LS-CHEAT-START-COL >= 1
               AND LS-CHEAT-START-COL <= GRID-SIZE
               AND LS-CHEAT-END-ROW >= 1
               AND LS-CHEAT-END-ROW <= GRID-SIZE
               AND LS-CHEAT-END-ROW >= 1
               AND LS-CHEAT-END-ROW <= GRID-SIZE
      *> Check that the start and end would go through
      *> a wall and to an open cell
               AND (
                   GRID-CELL(
                       LS-CHEAT-END-ROW,
                       LS-CHEAT-END-COL
                   ) = "."
                   OR GRID-CELL(
                       LS-CHEAT-END-ROW,
                       LS-CHEAT-END-COL
                   ) = "E"
               )
               COMPUTE LS-DIST-TO-CHEAT-START = GRID-FULL-PATH-LENGTH
                   - GRID-DIST-TO-END(
                       LS-PATH-NODE-ROW
                       LS-PATH-NODE-COL
                   )
               COMPUTE LS-DIST-FROM-CHEAT-END = GRID-DIST-TO-END(
                   LS-CHEAT-END-ROW,
                   LS-CHEAT-END-COL
               )
               IF LS-DIST-TO-CHEAT-START + LS-DIST-FROM-CHEAT-END + 2 <
                   GRID-FULL-PATH-LENGTH
                   ADD 1 TO CHEAT-SIZE
                   COMPUTE CHEAT-DISTANCE-SAVED(CHEAT-SIZE) =
                       GRID-FULL-PATH-LENGTH - LS-DIST-TO-CHEAT-START
                       - LS-DIST-FROM-CHEAT-END - 2
                   SET CHEAT-START-ROW(CHEAT-SIZE) TO LS-CHEAT-START-ROW
                   SET CHEAT-START-COL(CHEAT-SIZE) TO LS-CHEAT-START-COL
                   SET CHEAT-END-ROW(CHEAT-SIZE) TO LS-CHEAT-END-ROW
                   SET CHEAT-END-COL(CHEAT-SIZE) TO LS-CHEAT-END-COL
               END-IF

           END-IF

           .

       END PROGRAM FIND-CHEATS.

      *> ===============================================================
      *> PROCESS-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-GRID.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "queue" IN "20".
       COPY "visited" IN "20".
       01  LS-CUR-ROW               PIC 9(3) VALUE 0.
       01  LS-CUR-COL               PIC 9(3) VALUE 0.
       01  LS-CUR-DIST              PIC 9(5) VALUE 0.
       01  LS-NEXT-ROW              PIC 9(3) VALUE 0.
       01  LS-NEXT-COL              PIC 9(3) VALUE 0.
       01  LS-NEXT-DIST             PIC 9(5) VALUE 0.
       01  LS-VISIT-RESULT          PIC 9(1).

       LINKAGE SECTION.
       COPY "grid" IN "20".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.
           CALL "VISIT" USING
               VISITED-GRP
               GRID-START-ROW
               GRID-START-COL
           CALL "ENQUEUE" USING
               QUEUE-GRP
               GRID-START-ROW
               GRID-START-COL
               LS-CUR-DIST

           PERFORM UNTIL QUEUE-SIZE = 0
               CALL "DEQUEUE" USING
                   QUEUE-GRP
                   LS-CUR-ROW
                   LS-CUR-COL
                   LS-CUR-DIST

               IF LS-CUR-ROW = GRID-END-ROW
                   AND LS-CUR-COL = GRID-END-COL
                   MOVE LS-CUR-DIST TO GRID-FULL-PATH-LENGTH
                   MOVE LS-CUR-DIST TO RETURN-CODE
                   GOBACK
               END-IF

      *> Try up
               COMPUTE LS-NEXT-ROW = LS-CUR-ROW - 1
               COMPUTE LS-NEXT-COL = LS-CUR-COL
               PERFORM TRY-NEIGHBOR
      *> Try right
               COMPUTE LS-NEXT-ROW = LS-CUR-ROW
               COMPUTE LS-NEXT-COL = LS-CUR-COL + 1
               PERFORM TRY-NEIGHBOR
      *> Try down
               COMPUTE LS-NEXT-ROW = LS-CUR-ROW + 1
               COMPUTE LS-NEXT-COL = LS-CUR-COL
               PERFORM TRY-NEIGHBOR
      *> Try left
               COMPUTE LS-NEXT-ROW = LS-CUR-ROW
               COMPUTE LS-NEXT-COL = LS-CUR-COL - 1
               PERFORM TRY-NEIGHBOR

           END-PERFORM

           MOVE 0 TO RETURN-CODE

           GOBACK.

       TRY-NEIGHBOR.
           IF LS-NEXT-ROW >= 1
               AND LS-NEXT-ROW <= GRID-SIZE
               AND LS-NEXT-COL >= 1
               AND LS-NEXT-COL <= GRID-SIZE
               AND (
      *> Moving to another empty cell
                   GRID-CELL(LS-NEXT-ROW, LS-NEXT-COL) = "."
      *> Moving to the last cell
                   OR GRID-CELL(LS-NEXT-ROW, LS-NEXT-COL) = "E"
      *> Enter a shortcut
                   OR (
                       (
                           GRID-CELL(LS-CUR-ROW, LS-CUR-COL) = "."
                           OR GRID-CELL(LS-CUR-ROW, LS-CUR-COL) = "S"
                       )
                       AND GRID-CELL(LS-NEXT-ROW, Ls-NEXT-COL) = "1"
                   )
      *> Exit a shortcut
                   OR (
                       GRID-CELL(LS-CUR-ROW, LS-CUR-COL) = "1"
                       AND GRID-CELL(LS-NEXT-ROW, Ls-NEXT-COL) = "2"
                   )
               )

               CALL "VISIT" USING
                   VISITED-GRP
                   LS-NEXT-ROW
                   LS-NEXT-COL
                   RETURNING LS-VISIT-RESULT
               IF LS-VISIT-RESULT = 0
                   SET GRID-PATH-PARENT-ROW(
                       LS-NEXT-ROW,
                       LS-NEXT-COL
                   ) TO LS-CUR-ROW
                   SET GRID-PATH-PARENT-COL(
                       LS-NEXT-ROW,
                       LS-NEXT-COL
                   ) TO LS-CUR-COL
                   COMPUTE LS-NEXT-DIST = LS-CUR-DIST + 1
                   CALL "ENQUEUE" USING
                       QUEUE-GRP
                       LS-NEXT-ROW
                       LS-NEXT-COL
                       LS-NEXT-DIST
               END-IF
           END-IF
           .
       END PROGRAM PROCESS-GRID.

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
       01  F-FILE-RECORD             PIC X(141).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(141).
       01  LS-ITERATION              PIC 9(3) VALUE 0.

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "grid" IN "20".

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
                           SET GRID-SIZE TO LENGTH OF
                               FUNCTION TRIM(LS-LINE)
                       END-IF
                       ADD 1 TO LS-ITERATION
                       PERFORM VARYING GRID-COL-INDEX
                           FROM 1 BY 1 UNTIL GRID-COL-INDEX > GRID-SIZE
                           SET GRID-CELL(
                               LS-ITERATION,
                               GRID-COL-INDEX)
                               TO LS-LINE(GRID-COL-INDEX:1)
                           SET GRID-PATH-PARENT-ROW(
                               LS-ITERATION,
                               GRID-COL-INDEX
                           ) TO 0
                           SET GRID-PATH-PARENT-COL(
                               LS-ITERATION,
                               GRID-COL-INDEX
                           ) TO 0

                           EVALUATE LS-LINE(GRID-COL-INDEX:1)
                               WHEN "S"
                                   SET GRID-START-ROW TO LS-ITERATION
                                   SET GRID-START-COL TO GRID-COL-INDEX
                               WHEN "E"
                                   SET GRID-END-ROW TO LS-ITERATION
                                   SET GRID-END-COL TO GRID-COL-INDEX
                           END-EVALUATE
                       END-PERFORM
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.


      *> ===============================================================
      *> DISPLAY-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-GRID.

       DATA DIVISION.

       LINKAGE SECTION.
       COPY "grid" IN "20".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.

           DISPLAY "Grid size: " GRID-SIZE
           DISPLAY "Start: " GRID-START-ROW "," GRID-START-COL
           DISPLAY "End: " GRID-END-ROW "," GRID-END-COL


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
       COPY "visited" IN "20".
       01  IN-ROW                           PIC 9(3).
       01  IN-COL                           PIC 9(3).
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
       COPY "queue" IN "20".
       01 IN-QUEUE-VALUE-ROW                 PIC 9(3).
       01 IN-QUEUE-VALUE-COL                 PIC 9(3).
       01 IN-QUEUE-VALUE-DIST                PIC 9(5).

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
       COPY "queue" IN "20".
       01 OUT-QUEUE-VALUE-ROW                  PIC 9(3).
       01 OUT-QUEUE-VALUE-COL                  PIC 9(3).
       01 OUT-QUEUE-VALUE-DIST                 PIC 9(5).

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
