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

           IF LS-MODE = ">"

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
               GOBACK
           END-IF

           SET LS-FILE-PATH TO FUNCTION TRIM(LS-INPUT)

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       CALL "CALCULATE-COMPLEXITY" USING
                           KP-GRP
                           LS-LINE
                           LS-COMPLEXITY
                       ADD LS-COMPLEXITY TO LS-TOTAL-COMPLEXITY
           END-PERFORM
           DISPLAY "Total complexity: " LS-TOTAL-COMPLEXITY
           CLOSE FD-DATA



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
       LOCAL-STORAGE SECTION.
       01  LS-KP-IDX                             PIC 9(1) VALUE 3.
       01  LS-NEXT-TEST-SEQUENCE                 PIC X(100).
       01  LS-SHORTEST-INPUT-SEQUENCE            PIC X(100).
       01  LS-TARGET-SEQUENCES-IDX               PIC 9(3).
       01  LS-TARGET-SEQUENCES-GRP.
           05  LS-TARGET-SEQUENCES-SIZE          PIC 9(3).
           05  LS-TARGET-SEQUENCES
               OCCURS 999 TIMES.
               10  LS-TARGET-SEQUENCE            PIC X(100) VALUE SPACE.
       01  LS-SHORTEST-INPUTS-IDX                PIC 9(3).
       01  LS-SHORTEST-INPUTS-GRP.
           05  LS-SHORTEST-INPUTS-SIZE           PIC 9(3).
           05  LS-SHORTEST-INPUTS
               OCCURS 999 TIMES.
               10  LS-SHORTEST-INPUT             PIC X(100) VALUE SPACE.
       01  LS-NEXT-TARGETS-GRP.
           05  LS-NEXT-TARGETS-SIZE              PIC 9(3).
           05  LS-NEXT-TARGETS
               OCCURS 999 TIMES.
               10  LS-NEXT-TARGET                PIC X(100) VALUE SPACE.
       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-COMPLEXITY                        PIC 9(6).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-TARGET-SEQUENCE
           OUT-COMPLEXITY.

           display "calculate complexity '"
           function trim(in-target-sequence) "'"
           SET LS-TARGET-SEQUENCES-SIZE TO 1
           SET LS-TARGET-SEQUENCE(1) TO IN-TARGET-SEQUENCE

           PERFORM UNTIL LS-KP-IDX = 0

               DISPLAY SPACE
               DISPLAY "Level " LS-KP-IDX
               SET LS-NEXT-TARGETS-SIZE TO 0

               PERFORM VARYING LS-TARGET-SEQUENCES-IDX FROM 1 BY 1
                   UNTIL LS-TARGET-SEQUENCES-IDX >
                       LS-TARGET-SEQUENCES-SIZE
                   SET LS-NEXT-TEST-SEQUENCE TO
                       LS-TARGET-SEQUENCE(LS-TARGET-SEQUENCES-IDX)

                   IF LS-KP-IDX < 3
                       CALL "INIT-DIRECTIONAL-KEYPAD" USING
                           BY REFERENCE
                           KP-GRP
                           LS-KP-IDX
                   END-IF
                   CALL "FIND-SHORTEST-INPUT-SEQUENCE" USING
                       BY REFERENCE
                       KP-GRP
                       LS-KP-IDX
                       LS-NEXT-TEST-SEQUENCE
                       LS-SHORTEST-INPUTS-GRP

                   DISPLAY "Shortest sequences for "
                       function trim(ls-next-test-sequence) ": "
                   PERFORM VARYING LS-SHORTEST-INPUTS-IDX FROM 1 BY 1
                       UNTIL LS-SHORTEST-INPUTS-IDX >
                       LS-SHORTEST-INPUTS-SIZE

                       ADD 1 TO LS-NEXT-TARGETS-SIZE
                       display  LS-SHORTEST-INPUT(
                           LS-SHORTEST-INPUTS-IDX)
                       MOVE LS-SHORTEST-INPUT(LS-SHORTEST-INPUTS-IDX)
                           TO LS-NEXT-TARGET(LS-NEXT-TARGETS-SIZE)
                   END-PERFORM
               END-PERFORM
               MOVE LS-NEXT-TARGETS-GRP TO LS-TARGET-SEQUENCES-GRP
               ADD -1 TO LS-KP-IDX
           END-PERFORM
      *> Find the shortest input sequence now
           SET LS-SHORTEST-INPUT-SEQUENCE TO SPACES
           PERFORM VARYING LS-TARGET-SEQUENCES-IDX FROM 1 BY 1
               UNTIL LS-TARGET-SEQUENCES-IDX >
                   LS-TARGET-SEQUENCES-SIZE
               IF LS-SHORTEST-INPUT-SEQUENCE = SPACES OR
                   LENGTH OF FUNCTION TRIM(LS-SHORTEST-INPUT-SEQUENCE)
                       > LS-TARGET-SEQUENCE(LS-TARGET-SEQUENCES-IDX)

                   SET Ls-SHORTEST-INPUT-SEQUENCE TO
                       LS-TARGET-SEQUENCE(LS-TARGET-SEQUENCES-IDX)
               END-IF

           END-PERFORM
           DISPLAY "Shortest lowest level input sequence "
               LS-SHORTEST-INPUT-SEQUENCE
           COMPUTE OUT-COMPLEXITY =
               LENGTH OF FUNCTION TRIM(
                   LS-SHORTEST-INPUT-SEQUENCE
               ) * FUNCTION NUMVAL(IN-TARGET-SEQUENCE)
           DISPLAY "Complexity "  LENGTH OF FUNCTION TRIM(
                   LS-SHORTEST-INPUT-SEQUENCE
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
       LOCAL-STORAGE SECTION.
       01  LS-QUEUE-VALUE-ROW                    PIC 9(1).
       01  LS-QUEUE-VALUE-COL                    PIC 9(1).
       01  LS-QUEUE-VALUE-MOV                    PIC X(1).
       01  LS-QUEUE-VALUE-KEYPRESS-HIST          PIC X(100).
       01  LS-QUEUE-VALUE-MOV-HIST               PIC X(100).
       01  LS-NEXT-ROW                           PIC 9(1).
       01  LS-NEXT-COL                           PIC 9(1).
       01  LS-NEXT-MOV                           PIC X(1).
       01  LS-NEXT-KEYPRESS-HIST                 PIC X(100).
       01  LS-NEXT-MOV-HIST                      PIC X(100).
       01  LS-PREV-MOV                           PIC X(2).
       01  LS-IS-VALID-MOVE                      PIC 9(1).

       01  LS-NEXT-CANDIDATE-KEYPRESS-HIST       PIC X(100).
       01  LS-VISIT-RESULT                       PIC 9(1).
       COPY "queue" IN "21/src".
       COPY "visited" IN "21/src".

       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
       01  IN-KP-IDX                             PIC 9(1) VALUE 1.
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-SHORTEST-INPUTS-GRP.
           05  OUT-SHORTEST-INPUTS-SIZE          PIC 9(3).
           05  OUT-SHORTEST-INPUTS
               OCCURS 999 TIMES.
               10  OUT-SHORTEST-INPUT            PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-TARGET-SEQUENCE
           OUT-SHORTEST-INPUTS-GRP.

           *>display "find-shortest-input-sequence for "
      *>    in-target-sequence

           SET OUT-SHORTEST-INPUTS-SIZE TO 0
           SET LS-QUEUE-VALUE-ROW TO KP-CUR-ROW(IN-KP-IDX)
           SET LS-QUEUE-VALUE-COL TO KP-CUR-COL(IN-KP-IDX)
           SET LS-QUEUE-VALUE-MOV TO SPACE
           SET LS-QUEUE-VALUE-KEYPRESS-HIST TO SPACE
           SET LS-QUEUE-VALUE-MOV-HIST TO SPACE

           CALL "VISIT" USING
               VISITED-GRP
               LS-QUEUE-VALUE-ROW
               LS-QUEUE-VALUE-COL
               LS-QUEUE-VALUE-KEYPRESS-HIST
               LS-QUEUE-VALUE-MOV-HIST
           CALL "ENQUEUE" USING
               QUEUE-GRP
               LS-QUEUE-VALUE-ROW
               LS-QUEUE-VALUE-COL
               LS-QUEUE-VALUE-MOV
               LS-QUEUE-VALUE-KEYPRESS-HIST
               LS-QUEUE-VALUE-MOV-HIST

           PERFORM UNTIL QUEUE-SIZE = 0
               CALL "DEQUEUE" USING
               QUEUE-GRP
               LS-QUEUE-VALUE-ROW
               LS-QUEUE-VALUE-COL
               LS-QUEUE-VALUE-MOV
               LS-QUEUE-VALUE-KEYPRESS-HIST
               LS-QUEUE-VALUE-MOV-HIST

      *>         display space
      *>         display "dequeue:"
      *>             ", key: " kp-key(in-kp-idx, ls-queue-value-row,
      *>             ls-queue-value-col)
      *>             " mov: " ls-queue-value-mov
      *>             ", keypress hist: "
      *>             function trim(ls-queue-value-keypress-hist)
      *>             ", mov hist:"
      *>             function trim(ls-queue-value-mov-hist)

               SET LS-NEXT-MOV-HIST TO SPACE

               SET LS-NEXT-KEYPRESS-HIST TO LS-QUEUE-VALUE-KEYPRESS-HIST

               STRING FUNCTION TRIM(LS-QUEUE-VALUE-MOV-HIST)
                   LS-QUEUE-VALUE-MOV
                   INTO LS-NEXT-MOV-HIST
               END-STRING


               STRING FUNCTION TRIM(LS-QUEUE-VALUE-KEYPRESS-HIST)
                   KP-KEY(
                       IN-KP-IDX,
                       LS-QUEUE-VALUE-ROW,
                       LS-QUEUE-VALUE-COL
                   )
                   INTO LS-NEXT-CANDIDATE-KEYPRESS-HIST
               END-STRING

      *> We found the target sequence, done!
               IF LS-NEXT-CANDIDATE-KEYPRESS-HIST = IN-TARGET-SEQUENCE
                   ADD 1 TO OUT-SHORTEST-INPUTS-SIZE
                   STRING
                       FUNCTION TRIM(LS-NEXT-MOV-HIST)
                       "A"
                       INTO OUT-SHORTEST-INPUT(OUT-SHORTEST-INPUTS-SIZE)
                   END-STRING
                   GOBACK
      *>             display "'"
      *>                 FUNCTION TRIM(
      *>                     OUT-SHORTEST-INPUT(OUT-SHORTEST-INPUTS-SIZE)
      *>                 ) "'"
               ELSE
      *> We found a substring:
                   IF IN-TARGET-SEQUENCE(
                       1:LENGTH OF FUNCTION TRIM(
                           LS-NEXT-CANDIDATE-KEYPRESS-HIST)
                   ) = LS-NEXT-CANDIDATE-KEYPRESS-HIST
                       STRING
                           FUNCTION TRIM(LS-NEXT-MOV-HIST)
                           "A"
                           INTO LS-NEXT-MOV-HIST
                       END-STRING
                       SET LS-NEXT-KEYPRESS-HIST TO
                           LS-NEXT-CANDIDATE-KEYPRESS-HIST
      *>             ELSE
      *>                 display "Passing through"
                   END-IF
      *> Try bottom
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW + 1
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL
                   SET LS-NEXT-MOV TO "v"
                   PERFORM TRY-NEIGHBOR
      *> Try right
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL + 1
                   SET LS-NEXT-MOV TO ">"
                   PERFORM TRY-NEIGHBOR
      *> Try left
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL - 1
                   SET LS-NEXT-MOV TO "<"
                   PERFORM TRY-NEIGHBOR
      *> Try up
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW - 1
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL
                   SET LS-NEXT-MOV TO "^"
                   PERFORM TRY-NEIGHBOR
      *> Try the same?
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL
                   SET LS-NEXT-MOV TO " "
                   PERFORM TRY-NEIGHBOR


               END-IF

           END-PERFORM

           GOBACK.

       TRY-NEIGHBOR.
           IF LS-NEXT-ROW >= 1 AND LS-NEXT-ROW <= KP-HEIGHT(IN-KP-IDX)
               AND LS-NEXT-COL >= 1 AND LS-NEXT-COL <= 3
               AND KP-KEY(IN-KP-IDX, LS-NEXT-ROW, LS-NEXT-COL) NOT =
               SPACE

      *> Forbid some movements which we know are not optimal
               SET LS-PREV-MOV TO LS-NEXT-MOV-HIST(
                       LENGTH OF FUNCTION TRIM(LS-NEXT-MOV-HIST) - 1:2
                   )
               SET LS-IS-VALID-MOVE TO 1
               IF IN-KP-IDX = 3
                   EVALUATE
                       LS-PREV-MOV
                       ALSO LS-NEXT-MOV

                       WHEN "A<" ALSO "^"
                       WHEN "Av" ALSO ">"
                       WHEN "A<" ALSO "v"
                           SET LS-IS-VALID-MOVE TO 0
                   END-EVALUATE
               END-IF

               IF LS-IS-VALID-MOVE = 1
                   CALL "VISIT" USING
                       VISITED-GRP
                       LS-NEXT-ROW
                       LS-NEXT-COL
                       LS-NEXT-KEYPRESS-HIST
                       LS-NEXT-MOV-HIST
                       RETURNING LS-VISIT-RESULT

                   IF LS-VISIT-RESULT = 0
      *>             display "visited "
      *>             kp-key(in-kp-idx,ls-next-row,ls-next-col)
      *>             "," ls-next-mov
      *>             "," function trim(ls-next-mov-hist)
                       CALL "ENQUEUE" USING BY REFERENCE
                           QUEUE-GRP
                           LS-NEXT-ROW
                           LS-NEXT-COL
                           LS-NEXT-MOV
                           LS-NEXT-KEYPRESS-HIST
                           LS-NEXT-MOV-HIST
      *>         ELSE
      *>             display "already visited "
      *>             kp-key(in-kp-idx,ls-next-row,ls-next-col)
      *>             "," ls-next-mov
      *>             "," function trim(ls-next-mov-hist)
                   END-IF
               END-IF



           END-IF
           .
       END PROGRAM FIND-SHORTEST-INPUT-SEQUENCE.

