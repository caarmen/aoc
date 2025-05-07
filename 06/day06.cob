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

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE               PIC X(20).
       01  LS-FILE-PATH                  PIC X(20).
       01  LS-PART                       PIC 9(1).
       01  LS-TOTAL-X-COUNT              PIC 9(5) USAGE COMP.
       COPY "grid" IN "06".

       PROCEDURE DIVISION.

      *> Read the grid
           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " "
               INTO LS-FILE-PATH LS-PART

           CALL "PARSE-GRID" USING
               BY REFERENCE GRID-GRP

           display "read grid of size " GRID-SIZE
               " with guard at " GUARD-ROW "," GUARD-COL

      *> Call the program for the selected part for day 6.
           IF LS-PART = 1
           THEN
               CALL "PART-1" USING
                   BY REFERENCE GRID-GRP
           ELSE
               CALL "PART-2" USING
                   BY REFERENCE GRID-GRP
           END-IF

           GOBACK.

       END PROGRAM DAY06.

      *> ===============================================================
      *> PART-1.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PART-1.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ROW-X-COUNT                PIC 9(3) USAGE COMP VALUE 0.
       01  LS-TOTAL-X-COUNT              PIC 9(5) USAGE COMP VALUE 0.

       LINKAGE SECTION.
           COPY "grid" IN "06".

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP.
           CALL "RUN-GRID" USING
               BY REFERENCE GRID-GRP

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
       END PROGRAM PART-1.

      *> ===============================================================
      *> PART-2.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PART-2.
       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-OBSTACLE-ROW               PIC 9(3) USAGE COMP.
       01  LS-OBSTACLE-COL               PIC 9(3) USAGE COMP.
       01  LS-SUCCESS-OBSTACLE-COUNT     PIC 9(5) USAGE COMP VALUE 0.
       01  LS-GUARD-START-ROW            PIC 9(3) USAGE COMP.
       01  LS-GUARD-START-COL            PIC 9(3) USAGE COMP.
       LINKAGE SECTION.
           COPY "grid" IN "06".


       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP.

           SET LS-GUARD-START-ROW TO GUARD-ROW
           SET LS-GUARD-START-COL TO GUARD-COL

           PERFORM VARYING LS-OBSTACLE-ROW FROM 1 BY 1
               UNTIL LS-OBSTACLE-ROW > GRID-SIZE
               AFTER LS-OBSTACLE-COL FROM 1 BY 1
               UNTIL LS-OBSTACLE-COL > GRID-SIZE

      *> Reset the grid data to the starting state.
               SET GUARD-ROW TO LS-GUARD-START-ROW
               SET GUARD-COL TO LS-GUARD-START-COL

      *> If the location isn't occupied, place the obstacle
      *> and test the grid.
               IF GRID-CELL(LS-OBSTACLE-ROW, LS-OBSTACLE-COL) NOT = "#"
                   AND GRID-CELL(LS-OBSTACLE-ROW, LS-OBSTACLE-COL)
                       NOT = "^"
               THEN
                   MOVE "#" TO
                       GRID-CELL(LS-OBSTACLE-ROW, LS-OBSTACLE-COL)

                   CALL "RUN-GRID" USING
                       BY REFERENCE GRID-GRP

                   IF RETURN-CODE = 1
                   THEN
      *> This obstacle worked!
                       ADD 1 TO LS-SUCCESS-OBSTACLE-COUNT
                   END-IF

      *> Remove the obstacle.
                   MOVE "." TO
                       GRID-CELL(LS-OBSTACLE-ROW, LS-OBSTACLE-COL)
               END-IF
           END-PERFORM
           DISPLAY LS-SUCCESS-OBSTACLE-COUNT " successful obstacles"
           GOBACK.
       END PROGRAM PART-2.

      *> ===============================================================
      *> RUN-GRID.
      *> Return 0 if the guard exited the grid, 1 if a loop was
      *> detected.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUN-GRID.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-DIR-UP                      CONSTANT 1.
       01  C-DIR-RIGHT                   CONSTANT 2.
       01  C-DIR-DOWN                    CONSTANT 3.
       01  C-DIR-LEFT                    CONSTANT 4.

       LOCAL-STORAGE SECTION.
       01  LS-GUARD-DIR                  PIC 9(1) VALUE C-DIR-UP.
       01  LS-IS-LOOP                    PIC 9(1) VALUE 0.
      *> For calculating the number of path nodes
      *> for which we had a direction change:
       COPY "turn" IN "06".

       LINKAGE SECTION.
           COPY "grid" IN "06".

       PROCEDURE DIVISION USING
           BY REFERENCE GRID-GRP.

           MOVE 0 TO RETURN-CODE
           SET LS-GUARD-DIR TO C-DIR-UP

           PERFORM UNTIL EXIT
      *> Mark each cell the guard occupies with an X.
               SET GRID-CELL(GUARD-ROW, GUARD-COL) to "X"

      *> Move, or set the next direction, of the guard, based on
      *> the next cell in the guard's current direction.
               EVALUATE LS-GUARD-DIR
                   WHEN C-DIR-UP
                       IF GUARD-ROW = 1
                           EXIT PERFORM
                       ELSE IF GRID-CELL(GUARD-ROW - 1, GUARD-COL) = "#"
                           PERFORM CHECK-LOOP
                           SET LS-GUARD-DIR TO C-DIR-RIGHT
                       ELSE
                           ADD -1 TO GUARD-ROW
                       END-IF
                   WHEN C-DIR-RIGHT
                       IF GUARD-COL = GRID-SIZE
                           EXIT PERFORM
                       ELSE IF GRID-CELL(GUARD-ROW, GUARD-COL + 1) = "#"
                           PERFORM CHECK-LOOP
                           SET LS-GUARD-DIR TO C-DIR-DOWN
                       ELSE
                           ADD 1 TO GUARD-COL
                       END-IF
                   WHEN C-DIR-DOWN
                       IF GUARD-ROW = GRID-SIZE
                           EXIT PERFORM
                       ELSE IF GRID-CELL(GUARD-ROW + 1, GUARD-COL) = "#"
                           PERFORM CHECK-LOOP
                           SET LS-GUARD-DIR TO C-DIR-LEFT
                       ELSE
                           ADD 1 TO GUARD-ROW
                       END-IF
                   WHEN C-DIR-LEFT
                       IF GUARD-COL = 1
                           EXIT PERFORM
                       ELSE IF GRID-CELL(GUARD-ROW, GUARD-COL - 1) = "#"
                           PERFORM CHECK-LOOP
                           SET LS-GUARD-DIR TO C-DIR-UP
                       ELSE
                           ADD -1 TO GUARD-COL
                       END-IF
               END-EVALUATE
           END-PERFORM


           GOBACK.

       CHECK-LOOP.
           CALL "CHECK-LOOP" USING
               BY REFERENCE GUARD-ROW
               BY REFERENCE GUARD-COL
               BY REFERENCE LS-GUARD-DIR
               BY REFERENCE TURN-GRP
               RETURNING LS-IS-LOOP

      *> We already hit this turn, this means we're in a loop!
               IF LS-IS-LOOP = 1
                   MOVE 1 TO RETURN-CODE
                   GOBACK
               END-IF
           .

       END PROGRAM RUN-GRID.

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
           SET GUARD-ROW TO 0
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

      *> ===============================================================
      *> CHECK-LOOP.
      *> Returns 1 if we detected a loop, 0 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-LOOP.
       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-TURN-NODE-ROW              PIC 9(3) USAGE COMP.
       01  IN-TURN-NODE-COL              PIC 9(3) USAGE COMP.
       01  IN-TURN-NODE-DIR              PIC 9(1) USAGE COMP.
       COPY "turn" IN "06".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-TURN-NODE-ROW
           BY REFERENCE IN-TURN-NODE-COL
           BY REFERENCE IN-TURN-NODE-DIR
           BY REFERENCE TURN-GRP.
           SET TURN-INDEX TO 0
           SEARCH TURN-NODES
               VARYING TURN-INDEX
               AT END
      *> First time hitting this turn, log it.
                   ADD 1 TO TURN-SIZE
                   SET TURN-NODE-ROW(TURN-SIZE) TO IN-TURN-NODE-ROW
                   SET TURN-NODE-COL(TURN-SIZE) TO IN-TURN-NODE-COL
                   SET TURN-NODE-DIR(TURN-SIZE) TO IN-TURN-NODE-DIR
                   MOVE 0 TO RETURN-CODE

      *> We already hit this turn, this means we're in a loop!
               WHEN TURN-NODE-ROW(TURN-INDEX) = IN-TURN-NODE-ROW
                   AND TURN-NODE-COL(TURN-INDEX) = IN-TURN-NODE-COL
                   AND TURN-NODE-DIR(TURN-INDEX) = IN-TURN-NODE-DIR
                   MOVE 1 TO RETURN-CODE
           END-SEARCH
           GOBACK.
       END PROGRAM CHECK-LOOP.
