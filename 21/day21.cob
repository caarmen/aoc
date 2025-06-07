       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY21.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD             PIC X(47).

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(100).
       01  LS-MODE                   PIC X(1).
       01  LS-LINE                   PIC X(100).
       01  LS-INPUT                  PIC X(100).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-KP-IDX                 PIC 9(1) VALUE 1.
       01  LS-COMPLEXITY             PIC 9(6).
       01  LS-TOTAL-COMPLEXITY       PIC 9(7) VALUE 0.
       01  LS-SHORTEST-SEQUENCE      PIC X(100).
       COPY "keypad" IN "21/src".

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY SPACE
               INTO LS-MODE LS-INPUT
           END-UNSTRING


      *> Init the first three directional keypads
           PERFORM VARYING LS-KP-IDX FROM 1 BY 1 UNTIL
               LS-KP-IDX > 2
               CALL "INIT-DIRECTIONAL-KEYPAD" USING BY REFERENCE
                   KP-GRP
                   LS-KP-IDX
           END-PERFORM

      *> Init the last numeric keypad
           SET LS-KP-IDX TO 3
           CALL "INIT-NUMERIC-KEYPAD" USING BY REFERENCE
               KP-GRP
               LS-KP-IDX

           EVALUATE LS-MODE
               WHEN ">"

                   SET LS-KP-IDX TO 1
                   CALL "USE-KEYPAD-SEQUENCE" USING
                       KP-GRP
                       LS-KP-IDX
                       LS-INPUT
                   PERFORM VARYING LS-KP-IDX FROM 1 BY 1
                       UNTIL LS-KP-IDX > 3
                       CALL "DISPLAY-KEYPAD" USING BY REFERENCE
                           KP-GRP
                           LS-KP-IDX
                   END-PERFORm

               WHEN "<"

                   SET LS-FILE-PATH TO FUNCTION TRIM(LS-INPUT)

                   OPEN INPUT FD-DATA
                   PERFORM UNTIL EXIT
                       READ FD-DATA INTO F-FILE-RECORD
                           AT END
                               EXIT PERFORM
                           NOT AT END
                               MOVE F-FILE-RECORD TO LS-LINE
                               CALL "CALCULATE-COMPLEXITY" USING
                                   LS-LINE
                                   LS-COMPLEXITY
                               ADD LS-COMPLEXITY TO LS-TOTAL-COMPLEXITY
                   END-PERFORM
                   DISPLAY "Total complexity: " LS-TOTAL-COMPLEXITY
                   CLOSE FD-DATA

               WHEN "?"
                   SET LS-KP-IDX TO LS-INPUT(1:1)
                   SET KP-CUR-ROW(LS-KP-IDX) TO LS-INPUT(2:1)
                   SET KP-CUR-COL(LS-KP-IDX) TO LS-INPUT(3:1)
                   CALL "GET-STEP-PATH" USING
                       KP-TYPE(LS-KP-IDX)
                       LS-INPUT(2:1)
                       LS-INPUT(3:1)
                       LS-SHORTEST-SEQUENCE
                   DISPLAY "Shortest: " LS-SHORTEST-SEQUENCE

           END-EVALUATE



           PERFORM VARYING LS-KP-IDX FROM 1 BY 1 UNTIL
               LS-KP-IDX > 3
               CALL "DISPLAY-KEYPAD" USING BY REFERENCE
                   KP-GRP
                   LS-KP-IDX
           END-PERFORM

      *>     CALL "PARSE-FILE" USING
      *>         BY REFERENCE LS-FILE-PATH
           .
       END PROGRAM DAY21.

      *> ===============================================================
      *> CALCULATE-COMPLEXITY
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-COMPLEXITY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-KP-IDX                             PIC 9(1) VALUE 3.
       01  LS-KP-TYPE                            PIC 9(1).
       01  LS-TARGET-SEQUENCE                    PIC X(100).
       01  LS-SOURCE-SEQUENCE                    PIC X(100).
       LINKAGE SECTION.
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-COMPLEXITY                        PIC 9(6).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-TARGET-SEQUENCE
           OUT-COMPLEXITY.

           display "calculate complexity '"
           function trim(in-target-sequence) "'"
           SET LS-TARGET-SEQUENCE TO IN-TARGET-SEQUENCE
           SET LS-KP-TYPE TO C-TYPE-NUMERIC

           PERFORM VARYING LS-KP-IDX FROM 3 BY -1 UNTIL
               LS-KP-IDX = 0

               DISPLAY SPACE
               DISPLAY "Level " LS-KP-IDX ": " ls-target-sequence

               CALL "FIND-SHORTEST-INPUT-SEQUENCE" USING
                   BY REFERENCE
                   LS-KP-TYPE
                   LS-TARGET-SEQUENCE
                   LS-SOURCE-SEQUENCE


               DISPLAY "(" LS-KP-IDX ") Shortest sequences for "
                   function trim(ls-target-sequence) ": "
                   LS-SOURCE-SEQUENCE

               MOVE LS-SOURCE-SEQUENCE TO LS-TARGET-SEQUENCE
               SET LS-KP-TYPE TO C-TYPE-DIRECTIONAL
           END-PERFORM
      *> Find the shortest input sequence now
           COMPUTE OUT-COMPLEXITY =
               LENGTH OF FUNCTION TRIM(
                   LS-SOURCE-SEQUENCE
               ) * FUNCTION NUMVAL(IN-TARGET-SEQUENCE)
           DISPLAY "Complexity "  LENGTH OF FUNCTION TRIM(
                   LS-SOURCE-SEQUENCE
               ) "*" FUNCTION NUMVAL(IN-TARGET-SEQUENCE)
               " = "OUT-COMPLEXITY

           GOBACK.
       END PROGRAM CALCULATE-COMPLEXITY.

      *> ===============================================================
      *> FIND-SHORTEST-INPUT-SEQUENCE
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-SHORTEST-INPUT-SEQUENCE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-TARGET-IDX                         PIC 9(3).
       01  LS-TARGET-SEQUENCE                    PIC X(100).
       01  LS-SOURCE-STEP-SEQUENCE               PIC X(100).
       01  LS-SOURCE-SEQUENCE                    PIC X(100).
       LINKAGE SECTION.
       01  IN-KP-TYPE                            PIC 9(1).
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-SOURCE-SEQUENCE                   PIC x(100).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KP-TYPE
           IN-TARGET-SEQUENCE
           OUT-SOURCE-SEQUENCE.

           SET OUT-SOURCE-SEQUENCE TO SPACES
           STRING "A" IN-TARGET-SEQUENCE INTO LS-TARGET-SEQUENCE

           PERFORM VARYING LS-TARGET-IDX FROM 1 BY 1 UNTIL
               LS-TARGET-IDX = LENGTH OF
               FUNCTION TRIM(LS-TARGET-SEQUENCE)

               CALL "GET-STEP-PATH" USING
                   IN-KP-TYPE
                   LS-TARGET-SEQUENCE(LS-TARGET-IDX:1)
                   LS-TARGET-SEQUENCE(LS-TARGET-IDX + 1:1)
                   LS-SOURCE-STEP-SEQUENCE

               STRING
                   FUNCTION TRIM(OUT-SOURCE-SEQUENCE)
                   FUNCTION TRIM(LS-SOURCE-STEP-SEQUENCE)
                   INTO OUT-SOURCE-SEQUENCE
               END-STRING
           END-PERFORM

           .
       END PROGRAM FIND-SHORTEST-INPUT-SEQUENCE.

      *> ===============================================================
      *> GET-STEP-PATH.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-STEP-PATH.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-LEFT                               PIC X(3) VALUE "<<<".
       01  C-RIGHT                              PIC X(3) VALUE ">>>".
       01  C-TOP                                PIC X(3) VALUE "^^^".
       01  C-BOTTOM                             PIC X(3) VALUE "vvv".
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-TARGET-START-ROW                  PIC 9(1).
       01  LS-TARGET-START-COL                  PIC 9(1).
       01  LS-TARGET-END-ROW                    PIC 9(1).
       01  LS-TARGET-END-COL                    PIC 9(1).
       01  LS-TARGET-DELTA-ROW                  PIC S9(1).
       01  LS-TARGET-DELTA-COL                  PIC S9(1).
       01  LS-SOURCE-SEQUENCE-UP                PIC X(3) VALUE SPACES.
       01  LS-SOURCE-SEQUENCE-RIGHT             PIC X(3) VALUE SPACES.
       01  LS-SOURCE-SEQUENCE-DOWN              PIC X(3) VALUE SPACES.
       01  LS-SOURCE-SEQUENCE-LEFT              PIC X(3) VALUE SPACES.
       LINKAGE SECTION.
       01  IN-KP-TYPE                           PIC 9(1).
       01  IN-TARGET-START-KEY                  PIC X(1).
       01  IN-TARGET-END-KEY                    PIC X(1).
       01  OUT-SOURCE-SEQUENCE                  PIC x(100).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KP-TYPE
           IN-TARGET-START-KEY
           IN-TARGET-END-KEy
           OUT-SOURCE-SEQUENCE.

           SET OUT-SOURCE-SEQUENCE TO SPACES

           CALL "GET-POSITION" USING
               IN-KP-TYPE
               IN-TARGET-START-KEY
               LS-TARGET-START-ROW
               LS-TARGET-START-COL

           CALL "GET-POSITION" USING
               IN-KP-TYPE
               IN-TARGET-END-KEY
               LS-TARGET-END-ROW
               LS-TARGET-END-COL

           COMPUTE LS-TARGET-DELTA-ROW = 
               LS-TARGET-END-ROW - LS-TARGET-START-ROW
           COMPUTE LS-TARGET-DELTA-COL = 
               LS-TARGET-END-COL - LS-TARGET-START-COL

           EVALUATE LS-TARGET-DELTA-ROW
               WHEN < 0
                   SET LS-SOURCE-SEQUENCE-UP TO
                       C-TOP(1: - LS-TARGET-DELTA-ROW)
               WHEN > 0
                   SET LS-SOURCE-SEQUENCE-DOWN TO
                       C-BOTTOM(1: LS-TARGET-DELTA-ROW)
           END-EVALUATE
           EVALUATE LS-TARGET-DELTA-COL
               WHEN < 0
                   SET LS-SOURCE-SEQUENCE-LEFT TO
                       C-LEFT(1: - LS-TARGET-DELTA-COL)
               WHEN > 0
                   SET LS-SOURCE-SEQUENCE-RIGHT TO
                       C-RIGHT(1: LS-TARGET-DELTA-COL)
           END-EVALUATE

      *> TODO FIX
           EVALUATE IN-KP-TYPE
               WHEN C-TYPE-NUMERIC
                   IF (
                           LS-TARGET-START-COL = 1
                           AND LS-TARGET-END-COL > 1
                           AND LS-TARGET-END-ROW = 4
                       ) OR (
                           LS-TARGET-START-ROW = 4
                           AND LS-TARGET-END-COL = 1
                           AND LS-TARGET-END-ROW < 4
                       ) 
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   ELSE
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   END-IF
               WHEN C-TYPE-DIRECTIONAL
                   IF (
                           LS-TARGET-START-ROW = 1
                           AND LS-TARGET-END-COL = 1
                       ) OR (
                           LS-TARGET-START-COL = 1
                           AND LS-TARGET-END-ROW = 1
                       ) 
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   ELSE
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   END-IF

           END-EVALUATE
           .
       END PROGRAM GET-STEP-PATH.


      *> ===============================================================
      *> GET-POSITION.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-POSITION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-TEMP PIC 9(1).
       LINKAGE SECTION.
       01  IN-KP-TYPE                           PIC 9(1).
       01  IN-KEY                               PIC X(1).
       01  OUT-ROW                              PIC 9(1).
       01  OUT-COL                              PIC 9(1).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KP-TYPE
           IN-KEY
           OUT-ROW
           OUT-COL.

           EVALUATE IN-KEY ALSO IN-KP-TYPE
      *> Directional
               WHEN "^" ALSO ANY
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 2
               WHEN "A" ALSO C-TYPE-DIRECTIONAL
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 3
               WHEN "<" ALSO ANY
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 1
               WHEN "v" ALSO ANY
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 2
               WHEN ">" ALSO ANY
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 3
      *> Numeric
               WHEN "7" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 1
               WHEN "8" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 2
               WHEN "9" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 3
               WHEN "4" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 1
               WHEN "5" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 2
               WHEN "6" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 3
               WHEN "1" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 3
                   SET OUT-COL TO 1
               WHEN "2" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 3
                   SET OUT-COL TO 2
               WHEN "3" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 3
                   SET OUT-COL TO 3
               WHEN "0" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 4
                   SET OUT-COL TO 2
               WHEN "A" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 4
                   SET OUT-COL TO 3
           END-EVALUATE
           .
       END PROGRAM GET-POSITION.
