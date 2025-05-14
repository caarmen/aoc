       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY12.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       COPY "plot" IN "12".
       01  LS-AREA                   PIC 9(8) COMP.
       01  LS-PERIMETER              PIC 9(8) COMP.
       01  LS-PRICE                  PIC 9(15) COMP.

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               PLOT-GRP
               REGION-GRP

           CALL "DISPLAY-PLOT" USING
               BY REFERENCE PLOT-GRP
               REGION-GRP

           CALL "CALCULATE-PRICE" USING
               BY REFERENCE PLOT-GRP
               LS-PRICE
           DISPLAY "Total price: " LS-PRICE
           .
       END PROGRAM DAY12.

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
       01  F-FILE-RECORD             PIC X(140).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(140).
       01  LS-REGION                 PIC X(1).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "plot" IN "12".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           PLOT-GRP
           REGION-GRP.

           OPEN INPUT FD-DATA
           SET PLOT-ROW-INDEX TO 0
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       IF PLOT-SIZE = 0
                           COMPUTE PLOT-SIZE
                               = LENGTH OF FUNCTION TRIM(LS-LINE)
                       END-IF
                       ADD 1 TO PLOT-ROW-INDEX
                       PERFORM VARYING PLOT-COL-INDEX FROM 1 BY 1
                           UNTIL PLOT-COL-INDEX > PLOT-SIZE

                           MOVE LS-LINE(PLOT-COL-INDEX:1) TO LS-REGION
                           MOVE LS-REGION TO
                               PLOT-CELL(PLOT-ROW-INDEX, PLOT-COL-INDEX)
                           SET REGION-INDEX TO 0
                           SEARCH REGIONS
                               VARYING REGION-INDEX
                               AT END
                                   ADD 1 TO REGION-COUNT
                                   SET REGION(REGION-COUNT) TO LS-REGION

                               WHEN REGION(REGION-INDEX) = LS-REGION
                                   CONTINUE
                           END-SEARCH
                       END-PERFORM
           END-PERFORM
           SORT REGIONS
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> DISPLAY-PLOT.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-PLOT.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "plot" IN "12".

       PROCEDURE DIVISION USING
           BY REFERENCE PLOT-GRP
           REGION-GRP.

           DISPLAY PLOT-SIZE " size plot"

           DISPLAY REGION-COUNT " regions: "
           PERFORM VARYING REGION-INDEX FROM 1 BY 1
               UNTIL REGION-INDEX > REGION-COUNT
               DISPLAY REGION(REGION-INDEX) SPACE NO ADVANCING
           END-PERFORM


           DISPLAY "Plots:"

           PERFORM VARYING PLOT-ROW-INDEX FROM 1 BY 1
               UNTIL PLOT-ROW-INDEX > PLOT-SIZE
               PERFORM VARYING PLOT-COL-INDEX FROM 1 BY 1
                   UNTIL PLOT-COL-INDEX > PLOT-SIZE
                   DISPLAY PLOT-CELL(PLOT-ROW-INDEX, PLOT-COL-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM

           GOBACK.
       END PROGRAM DISPLAY-PLOT.


      *> ===============================================================
      *> CALCULATE-PRICE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-PRICE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PUSH-TO-STACK
           FUNCTION POP-STACK.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "stack" IN "12".
       01  LS-ROW                  PIC 9(3) VALUE 0.
       01  LS-COL                  PIC 9(3) VALUE 0.
       01  LS-PREV-ROW             PIC 9(3) VALUE 0.
       01  LS-PREV-COL             PIC 9(3) VALUE 0.
       01  LS-NEXT-ROW             PIC 9(3).
       01  LS-NEXT-COL             PIC 9(3).
       01  LS-REGION               PIC X(1) VALUE ".".
       01  LS-POP-RESULT           PIC 9(1).
       01  LS-PUSH-RESULT          PIC 9(1).
       01  LS-AREA                 PIC 9(8) COMP VALUE 0.
       01  LS-PERIMETER            PIC 9(8) COMP VALUE 0.
       01  LS-SIDES                PIC 9(8) COMP VALUE 0.
       01  LS-SIDE-ALREADY-COUNTED PIC 9(1).

       LINKAGE SECTION.
       COPY "plot" IN "12".
       01  OUT-PRICE               PIC 9(15) COMP.

       PROCEDURE DIVISION USING
           BY REFERENCE PLOT-GRP
           OUT-PRICE.

           SET OUT-PRICE TO 0
           SET LS-PERIMETER TO 0
           SET LS-SIDES TO 0
           SET LS-AREA TO 0



           PERFORM FOREVER
               SET LS-POP-RESULT TO POP-STACK(
                   STACK-GRP
                   LS-ROW
                   LS-COL
                   LS-PREV-ROW
                   LS-PREV-COL
                   )
      *> Nothing in the stack, see if there are any unvisited cells
               IF LS-POP-RESULT = 1
                   COMPUTE OUT-PRICE = OUT-PRICE +
                       LS-AREA * LS-SIDES
                   SET LS-AREA TO 0
                   SET LS-PERIMETER TO 0
                   SET LS-SIDES TO 0
                   SET LS-PREV-ROW TO 0
                   SET LS-PREV-COL TO 0
                   PERFORM VARYING LS-ROW FROM 1 BY 1
                       UNTIL LS-ROW > PLOT-SIZE
                       OR STACK-SIZE > 0
                       PERFORM VARYING LS-COL FROM 1 BY 1
                           UNTIL LS-COL > PLOT-SIZE
                           OR STACK-SIZE > 0
                           IF VISITED(LS-ROW, LS-COL) = 0
                               SET LS-REGION TO PLOT-CELL(
                                   LS-ROW, LS-COL)

                               SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                                   LS-ROW
                                   LS-COL
                                   LS-PREV-ROW
                                   LS-PREV-COL
                                   STACK-GRP)
                           END-IF
                       END-PERFORM
                   END-PERFORM
      *> We're done! No more unvisited plots were found.
                   IF STACK-SIZE = 0
                       EXIT PERFORM
                   END-IF
               ELSE

      *> Visit a plot:
                   IF VISITED(LS-ROW, LS-COL) = 0
                       SET VISITED(LS-ROW, LS-COL) TO 1
                       ADD 1 TO LS-AREA
      *> ====================
      *> Add to the sides
      *> ====================
      *>   To the top:
                       IF LS-ROW = 1 OR
                           PLOT-CELL(LS-ROW - 1, LS-COL) NOT = LS-REGION

                           SET LS-SIDE-ALREADY-COUNTED TO 0
      *>     We didn't count this top side on the left already
                           COMPUTE LS-PREV-COL = LS-COL - 1
                           PERFORM VARYING LS-PREV-COL FROM LS-PREV-COL
                               BY -1 UNTIL LS-PREV-COL < 1
                               IF PLOT-CELL(LS-ROW, LS-PREV-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-ROW > 1 AND
                                   PLOT-CELL(LS-ROW - 1, LS-PREV-COL)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-ROW, LS-PREV-COL) = 1
                                   AND (LS-ROW = 1
                                       OR PLOT-CELL(
                                           LS-ROW - 1, LS-PREV-COL)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
      *>     We didn't count this top side on the right already
                           COMPUTE LS-PREV-COL = LS-COL + 1
                           PERFORM VARYING LS-PREV-COL FROM LS-PREV-COL
                               BY 1 UNTIL LS-PREV-COL > PLOT-SIZE
                               IF PLOT-CELL(LS-ROW, LS-PREV-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-ROW > 1 AND
                                   PLOT-CELL(LS-ROW - 1, LS-PREV-COL)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-ROW, LS-PREV-COL) = 1
                                   AND (LS-ROW = 1
                                       OR PLOT-CELL(
                                           LS-ROW - 1, LS-PREV-COL)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM

                           IF LS-SIDE-ALREADY-COUNTED = 0
                               ADD 1 TO LS-SIDES
                           END-IF
                       END-IF
      *>   To the right:
                       IF LS-COL = PLOT-SIZE OR
                           PLOT-CELL(LS-ROW, LS-COL + 1) NOT = LS-REGION

      *>     We didn't count this right side on the top already
                           SET LS-SIDE-ALREADY-COUNTED TO 0
                           COMPUTE LS-PREV-ROW = LS-ROW - 1
                           PERFORM VARYING LS-PREV-ROW FROM LS-PREV-ROW
                               BY -1 UNTIL LS-PREV-ROW < 1
                               IF PLOT-CELL(LS-PREV-ROW, LS-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-COL < PLOT-SIZE AND
                                   PLOT-CELL(LS-PREV-ROW, LS-COL + 1)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-PREV-ROW, LS-COL) = 1
                                   AND (LS-COL = PLOT-SIZE
                                       OR PLOT-CELL(
                                           LS-PREV-ROW, LS-COL + 1)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
      *>     We didn't count this right side on the bottom already
                           COMPUTE LS-PREV-ROW = LS-ROW + 1
                           PERFORM VARYING LS-PREV-ROW FROM LS-PREV-ROW
                               BY 1 UNTIL LS-PREV-ROW > PLOT-SIZE
                               IF PLOT-CELL(LS-PREV-ROW, LS-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-COL < PLOT-SIZE AND
                                   PLOT-CELL(LS-PREV-ROW, LS-COL + 1)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-PREV-ROW, LS-COL) = 1
                                   AND (LS-COL = PLOT-SIZE
                                       OR PLOT-CELL(
                                           LS-PREV-ROW, LS-COL + 1)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
                           IF LS-SIDE-ALREADY-COUNTED = 0
                               ADD 1 TO LS-SIDES
                           END-IF
                       END-IF
      *>   To the bottom:
                       IF LS-ROW = PLOT-SIZE OR
                           PLOT-CELL(LS-ROW + 1, LS-COL) NOT = LS-REGION

      *>     We didn't count this bottom side on the left already
                           SET LS-SIDE-ALREADY-COUNTED TO 0
                           COMPUTE LS-PREV-COL = LS-COL - 1
                           PERFORM VARYING LS-PREV-COL FROM LS-PREV-COL
                               BY -1 UNTIL LS-PREV-COL < 1
                               IF PLOT-CELL(LS-ROW, LS-PREV-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-ROW < PLOT-SIZE AND
                                   PLOT-CELL(LS-ROW + 1, LS-PREV-COL)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-ROW, LS-PREV-COL) = 1
                                   AND (LS-ROW = PLOT-SIZE
                                       OR PLOT-CELL(
                                           LS-ROW + 1, LS-PREV-COL)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
      *>     We didn't count this bottom side on the right already
                           COMPUTE LS-PREV-COL = LS-COL + 1
                           PERFORM VARYING LS-PREV-COL FROM LS-PREV-COL
                               BY 1 UNTIL LS-PREV-COL > PLOT-SIZE
                               IF PLOT-CELL(LS-ROW, LS-PREV-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-ROW < PLOT-SIZE AND
                                   PLOT-CELL(LS-ROW + 1, LS-PREV-COL)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-ROW, LS-PREV-COL) = 1
                                   AND (LS-ROW = PLOT-SIZE
                                       OR PLOT-CELL(
                                           LS-ROW + 1, LS-PREV-COL)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
                           IF LS-SIDE-ALREADY-COUNTED = 0
                               ADD 1 TO LS-SIDES
                           END-IF
                       END-IF
      *>   To the left:
                       IF LS-COL = 1 OR
                           PLOT-CELL(LS-ROW, LS-COL - 1) NOT = LS-REGION

      *>     We didn't count this left side on the top already
                           SET LS-SIDE-ALREADY-COUNTED TO 0
                           COMPUTE LS-PREV-ROW = LS-ROW - 1
                           PERFORM VARYING LS-PREV-ROW FROM LS-PREV-ROW
                               BY -1 UNTIL LS-PREV-ROW < 1
                               IF PLOT-CELL(LS-PREV-ROW, LS-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-COL > 1 AND
                                   PLOT-CELL(LS-PREV-ROW, LS-COL - 1)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-PREV-ROW, LS-COL) = 1
                                   AND (LS-COL = 1
                                       OR PLOT-CELL(
                                           LS-PREV-ROW, LS-COL - 1)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
      *>     We didn't count this left side on the bottom already
                           COMPUTE LS-PREV-ROW = LS-ROW + 1
                           PERFORM VARYING LS-PREV-ROW FROM LS-PREV-ROW
                               BY 1 UNTIL LS-PREV-ROW > PLOT-SIZE
                               IF PLOT-CELL(LS-PREV-ROW, LS-COL)
                                   NOT = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF LS-COL > 1 AND
                                   PLOT-CELL(LS-PREV-ROW, LS-COL - 1)
                                   = LS-REGION
                                   EXIT PERFORM
                               END-IF
                               IF VISITED(LS-PREV-ROW, LS-COL) = 1
                                   AND (LS-COL = 1
                                       OR PLOT-CELL(
                                           LS-PREV-ROW, LS-COL - 1)
                                           NOT = LS-REGION)
                                   SET LS-SIDE-ALREADY-COUNTED TO 1
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
                           IF LS-SIDE-ALREADY-COUNTED = 0
                               ADD 1 TO LS-SIDES
                           END-IF
                       END-IF

      *> ====================
      *> Add to the perimeter
      *> ====================
      *>   To the top:
                       IF LS-ROW = 1 OR
                           PLOT-CELL(LS-ROW - 1,
                           LS-COL) NOT = LS-REGION
                           ADD 1 TO LS-PERIMETER
                       END-IF
      *>   To the right:
                       IF LS-COL = PLOT-SIZE OR
                           PLOT-CELL(LS-ROW,
                           LS-COL + 1) NOT = LS-REGION
                           ADD 1 TO LS-PERIMETER
                       END-IF
      *>   To the bottom:
                       IF LS-ROW = PLOT-SIZE OR
                           PLOT-CELL(LS-ROW + 1,
                           LS-COL) NOT = LS-REGION
                           ADD 1 TO LS-PERIMETER
                       END-IF
      *>   To the left:
                       IF LS-COL = 1 OR
                           PLOT-CELL(LS-ROW,
                           LS-COL - 1) NOT = LS-REGION
                           ADD 1 TO LS-PERIMETER
                       END-IF

      *> Push all neighboring cells in this region.
      *>   To the top:
                       IF LS-ROW > 1 AND
                           PLOT-CELL(LS-ROW - 1, LS-COL) = LS-REGION
                           AND VISITED(LS-ROW - 1, LS-COL) = 0

                           COMPUTE LS-NEXT-ROW = LS-ROW - 1
                           COMPUTE LS-NEXT-COL = LS-COL
                           SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                               LS-NEXT-ROW
                               LS-NEXT-COL
                               LS-ROW
                               LS-COL
                               STACK-GRP)
                       END-IF
      *>   To the right:
                       IF LS-COL < PLOT-SIZE AND
                           PLOT-CELL(LS-ROW, LS-COL + 1) = LS-REGION
                           AND VISITED(LS-ROW, LS-COL + 1) = 0

                           COMPUTE LS-NEXT-ROW = LS-ROW
                           COMPUTE LS-NEXT-COL = LS-COL + 1
                           SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                               LS-NEXT-ROW
                               LS-NEXT-COL
                               LS-ROW
                               LS-COL
                               STACK-GRP)
                       END-IF
      *>   To the bottom:
                       IF LS-ROW < PLOT-SIZE AND
                           PLOT-CELL(LS-ROW + 1, LS-COL) = LS-REGION
                           AND VISITED(LS-ROW + 1, LS-COL) = 0

                           COMPUTE LS-NEXT-ROW = LS-ROW + 1
                           COMPUTE LS-NEXT-COL = LS-COL
                           SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                               LS-NEXT-ROW
                               LS-NEXT-COL
                               LS-ROW
                               LS-COL
                               STACK-GRP)
                       END-IF
      *>   To the left:
                       IF LS-COL > 1 AND
                           PLOT-CELL(LS-ROW, LS-COL - 1) = LS-REGION
                           AND VISITED(LS-ROW, LS-COL - 1) = 0

                           COMPUTE LS-NEXT-ROW = LS-ROW
                           COMPUTE LS-NEXT-COL = LS-COL - 1
                           SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                               LS-NEXT-ROW
                               LS-NEXT-COL
                               LS-ROW
                               LS-COL
                               STACK-GRP)
                       END-IF
      *> We've finished this region, start a new one.
                   END-IF
               END-IF
           END-PERFORM


           GOBACK.
       END PROGRAM CALCULATE-PRICE.


      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "12".
       01  OUT-ITEM-ROW                    PIC 9(3).
       01  OUT-ITEM-COL                    PIC 9(3).
       01  OUT-PREV-ITEM-ROW               PIC 9(3).
       01  OUT-PREV-ITEM-COL               PIC 9(3).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE STACK-GRP OUT-ITEM-ROW OUT-ITEM-COL
           OUT-PREV-ITEM-ROW
           OUT-PREV-ITEM-COL
           RETURNING OUT-RESULT
           .

           IF STACK-SIZE > 0
               MOVE STACK-ITEM-ROW(STACK-SIZE) TO OUT-ITEM-ROW
               MOVE STACK-ITEM-COL(STACK-SIZE) TO OUT-ITEM-COL
               MOVE STACK-PREV-ITEM-ROW(STACK-SIZE) TO OUT-PREV-ITEM-ROW
               MOVE STACK-PREV-ITEM-COL(STACK-SIZE) TO OUT-PREV-ITEM-COL
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
       01  IN-PREV-ITEM-ROW                PIC 9(3).
       01  IN-PREV-ITEM-COL                PIC 9(3).
       COPY "stack" IN "12".
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ITEM-ROW IN-ITEM-COL
           IN-PREV-ITEM-ROW
           IN-PREV-ITEM-COL
           STACK-GRP
           RETURNING OUT-RESULT.

           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-ROW(STACK-SIZE) TO IN-ITEM-ROW
           SET STACK-ITEM-COL(STACK-SIZE) TO IN-ITEM-COL
           SET STACK-PREV-ITEM-ROW(STACK-SIZE) TO IN-PREV-ITEM-ROW
           SET STACK-PREV-ITEM-COL(STACK-SIZE) TO IN-PREV-ITEM-COL

           MOVE 0 TO OUT-RESULT
           GOBACK.
       END FUNCTION PUSH-TO-STACK.
