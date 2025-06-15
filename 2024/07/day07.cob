      *> =================================================================
      *> Copyright 2025 - Present, Carmen Alvarez
      *>
      *> This file is part of Advent of code - @caarmen.
      *>
      *> Advent of code - @caarmen is free software: you can redistribute
      *> it and/or modify it under the terms of the GNU General Public
      *> License as published by the Free Software Foundation, either
      *> version 3 of the License, or (at your option) any later version.
      *>
      *> Advent of code - @caarmen is distributed in the hope that it will
      *> be useful, but WITHOUT ANY WARRANTY; without even the implied
      *> warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
      *> See the GNU General Public License for more details.
      *>
      *> You should have received a copy of the GNU General Public License
      *> along with Advent of code - @caarmen. If not, see
      *> <https://www.gnu.org/licenses/>.
      *> =================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY07.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.



       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-DATA-RECORD                  PIC X(50).

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                   PIC X(30).
       01  LS-LINE                        PIC X(50).
       01  LS-SUM-VALUES                  PIC 9(18) COMP-3 VALUE 0.
       COPY "numbers" IN "07".


       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE
           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-DATA-RECORD
               AT END
                   EXIT PERFORM
               NOT AT END
                   MOVE F-DATA-RECORD TO LS-LINE
                   CALL "PARSE-LINE" USING
                       BY REFERENCE LS-LINE
                       BY REFERENCE NUMS-GRP
                   CALL "PROCESS-LINE" USING
                       BY REFERENCE NUMS-GRP
                       IF RETURN-CODE = 1
                           COMPUTE LS-SUM-VALUES =
                               LS-SUM-VALUES + CALC-VALUE
                       END-IF

           END-PERFORM
           display LS-SUM-VALUES

           CLOSE FD-DATA
       .
       END PROGRAM DAY07.

      *> ===============================================================
      *> PROCESS-LINE.
      *> Return 1 if at least one combination of operators on the
      *> numbers allows to calculate the test value, 0 otherwise
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-LINE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "operators" IN "07".

       LOCAL-STORAGE SECTION.
       COPY "stack" IN "07".
       01  LS-CALC-RESULT             PIC 9(1).

       LINKAGE SECTION.
       COPY "numbers" IN "07".
       PROCEDURE DIVISION USING
           BY REFERENCE NUMS-GRP.

           CALL "PUSH-TO-STACK" USING
               BY VALUE C-OPERATOR-ADD
               BY REFERENCE STACK-GRP

           PERFORM UNTIL STACK-SIZE = 0
               EVALUATE TRUE
                   WHEN STACK-SIZE < NUMS-SIZE - 1
                       EVALUATE TRUE
                           WHEN STACK-ITEM-VISITED(STACK-SIZE) = 0
                               CALL "PUSH-TO-STACK" USING
                                   BY VALUE C-OPERATOR-ADD
                                   BY REFERENCE STACK-GRP
                           WHEN STACK-ITEM-VISITED(STACK-SIZE) = 1
                               AND STACK-ITEM-OPERATOR(STACK-SIZE)
                                   = C-OPERATOR-ADD
                                   CALL "POP-STACK" USING
                                       BY REFERENCE STACK-GRP
                                   CALL "PUSH-TO-STACK" USING
                                       BY VALUE C-OPERATOR-MUL
                                       BY REFERENCE STACK-GRP
                           WHEN STACK-ITEM-VISITED(STACK-SIZE) = 1
                               AND STACK-ITEM-OPERATOR(STACK-SIZE)
                                   = C-OPERATOR-MUL
                                   CALL "POP-STACK" USING
                                       BY REFERENCE STACK-GRP
                                   CALL "PUSH-TO-STACK" USING
                                       BY VALUE C-OPERATOR-CONCAT
                                       BY REFERENCE STACK-GRP
                           WHEN OTHER
                               CALL "POP-STACK" USING
                                   BY REFERENCE STACK-GRP
                       END-EVALUATE
                   WHEN OTHER
                       CALL "CALCULATE-STACK" USING
                           BY REFERENCE NUMS-GRP
                           BY REFERENCE STACK-GRP
                           RETURNING LS-CALC-RESULT

                       IF LS-CALC-RESULT = 1
                           MOVE 1 TO RETURN-CODE
                           GOBACK
                       END-IF

                       EVALUATE STACK-ITEM-OPERATOR(STACK-SIZE)
                           WHEN C-OPERATOR-ADD
                                   CALL "POP-STACK" USING
                                       BY REFERENCE STACK-GRP
                                   CALL "PUSH-TO-STACK" USING
                                       BY VALUE C-OPERATOR-MUL
                                       BY REFERENCE STACK-GRP
                           WHEN C-OPERATOR-MUL
                                   CALL "POP-STACK" USING
                                       BY REFERENCE STACK-GRP
                                   CALL "PUSH-TO-STACK" USING
                                       BY VALUE C-OPERATOR-CONCAT
                                       BY REFERENCE STACK-GRP
                           WHEN OTHER
                               CALL "POP-STACK" USING
                                   BY REFERENCE STACK-GRP
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM


           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM PROCESS-LINE.

      *> ===============================================================
      *> PARSE-LINE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-LINE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-NUMS-TEXT                   PIC X(50).
       01  LS-NUMS-PTR                    PIC 9(2) USAGE COMP.

       LINKAGE SECTION.
       01  IN-LINE                        PIC X(50).
       COPY "numbers" IN "07".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LINE
           BY REFERENCE NUMS-GRP.

           UNSTRING IN-LINE
               DELIMITED BY ":"
               INTO CALC-VALUE, LS-NUMS-TEXT
           END-UNSTRING

           SET NUMS-SIZE TO 0
           SET LS-NUMS-PTR TO 1

           PERFORM UNTIL LS-NUMS-PTR > LENGTH OF FUNCTION
                   TRIM(LS-NUMS-TEXT)
               ADD 1 TO NUMS-SIZE
               UNSTRING FUNCTION TRIM(LS-NUMS-TEXT)
                   DELIMITED BY " "
                   INTO NUM(NUMS-SIZE)
                   WITH POINTER LS-NUMS-PTR
               END-UNSTRING
           END-PERFORM


           GOBACK.
       END PROGRAM PARSE-LINE.

      *> ===============================================================
      *> CALCULATE-STACK.
      *> Return 1 if the result equals the test value, 0 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-STACK.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "operators" IN "07".

       LOCAL-STORAGE SECTION.
       01  LS-RESULT                        PIC 9(18) COMP-3 VALUE 0.
       01  LS-CONCAT-LEFT                   PIC Z(17)9.
       01  LS-CONCAT-RIGHT                  PIC Z(17)9.
       01  LS-CONCAT-RESULT-STR             PIC Z(36)9 VALUE SPACES.

       LINKAGE SECTION.
       COPY "stack" IN "07".
       COPY "numbers" IN "07".

       PROCEDURE DIVISION USING
           BY REFERENCE NUMS-GRP
           BY REFERENCE STACK-GRP.

           SET LS-RESULT TO NUM(1)
           PERFORM VARYING NUMS-INDEX FROM 2 BY 1
               UNTIL NUMS-INDEX > NUMS-SIZE
               EVALUATE STACK-ITEM-OPERATOR(NUMS-INDEX - 1)
                   WHEN C-OPERATOR-ADD
                       ADD NUM(NUMS-INDEX) TO LS-RESULT
                   WHEN C-OPERATOR-MUL
                       COMPUTE LS-RESULT = LS-RESULT * NUM(NUMS-INDEX)
                   WHEN OTHER
                       MOVE LS-RESULT TO LS-CONCAT-LEFT
                       MOVE NUM(NUMS-INDEX) TO LS-CONCAT-RIGHT
                       STRING FUNCTION TRIM(LS-CONCAT-LEFT)
                           FUNCTION TRIM(LS-CONCAT-RIGHT)
                           INTO LS-CONCAT-RESULT-STR
                       MOVE LS-CONCAT-RESULT-STR TO LS-RESULT
               END-EVALUATE
               IF LS-RESULT > CALC-VALUE
                   GOBACK
               END-IF
           END-PERFORM

           IF LS-RESULT = CALC-VALUE
           THEN
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF


           GOBACK.
       END PROGRAM CALCULATE-STACK.

      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "07".

       PROCEDURE DIVISION USING
           BY REFERENCE STACK-GRP.

           IF STACK-SIZE > 0
               COMPUTE STACK-SIZE = STACK-SIZE - 1
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
       01  IN-ITEM-OPERATOR                     PIC 9(1).
       COPY "stack" IN "07".

       PROCEDURE DIVISION USING
           BY VALUE IN-ITEM-OPERATOR
           BY REFERENCE STACK-GRP.

      *> Mark the top item as "visited" before pushing a new item
           IF STACK-SIZE > 0
               SET STACK-ITEM-VISITED(STACK-SIZE) TO 1
           END-IF

      *> Push the new item.
           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-OPERATOR(STACK-SIZE) TO IN-ITEM-OPERATOR
           SET STACK-ITEM-VISITED(STACK-SIZE) TO 0

           GOBACK.
       END PROGRAM PUSH-TO-STACK.

      *> ===============================================================
      *> DISPLAY-STACK.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-STACK.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "operators" IN "07".

       LINKAGE SECTION.
       COPY "stack" IN "07".

       PROCEDURE DIVISION USING
           BY REFERENCE STACK-GRP.

           display "Size: " stack-size
           PERFORM VARYING STACK-INDEX FROM STACK-SIZE BY -1
               UNTIL STACK-INDEX = 0
               EVALUATE STACK-ITEM-OPERATOR(STACK-INDEX)
                   WHEN C-OPERATOR-ADD
                       DISPLAY "+" NO ADVANCING
                   WHEN C-OPERATOR-MUL
                       DISPLAY "*" NO ADVANCING
                   WHEN C-OPERATOR-CONCAT
                       DISPLAY "||" NO ADVANCING
                   WHEN OTHER
                       DISPLAY "." NO ADVANCING
               END-EVALUATE
               IF STACK-ITEM-VISITED(STACK-INDEX) = 1
                   DISPLAY " v" NO ADVANCING
               END-IF
               DISPLAY SPACE
           END-PERFORm
           GOBACK.
       END PROGRAM DISPLAY-STACK.
