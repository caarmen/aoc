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
       01  LS-LINE                   PIC X(100).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-KP-IDX                 PIC 9(1) VALUE 1.
       01  LS-COMPLEXITY             PIC 9(6).
       01  LS-TOTAL-COMPLEXITY       PIC 9(7) VALUE 0.
       COPY "keypad" IN "21".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

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
      *> INIT-NUMERIC-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT-NUMERIC-KEYPAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21".
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           .

           SET KP-TYPE(IN-KP-IDX) TO C-TYPE-NUMERIC
           SET KP-HEIGHT(IN-KP-IDX) TO 4
           MOVE "789" TO KP-ROWS(IN-KP-IDX,1)
           MOVE "456" TO KP-ROWS(IN-KP-IDX,2)
           MOVE "123" TO KP-ROWS(IN-KP-IDX,3)
           MOVE " 0A" TO KP-ROWS(IN-KP-IDX,4)
           SET KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX) TO 0
           SET KP-CUR-ROW(IN-KP-IDX) TO 4
           SET KP-CUR-COL(IN-KP-IDX) TO 3

           .
       END PROGRAM INIT-NUMERIC-KEYPAD.

      *> ===============================================================
      *> INIT-DIRECTIONAL-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT-DIRECTIONAL-KEYPAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21".
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX.

           SET KP-TYPE(IN-KP-IDX) TO C-TYPE-DIRECTIONAL
           SET KP-HEIGHT(IN-KP-IDX) TO 2
           MOVE " ^A" TO KP-ROWS(IN-KP-IDX,1)
           MOVE "<v>" TO KP-ROWS(IN-KP-IDX,2)
           SET KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX) TO 0
           SET KP-CUR-ROW(IN-KP-IDX) TO 1
           SET KP-CUR-COL(IN-KP-IDX) TO 3

           .
       END PROGRAM INIT-DIRECTIONAL-KEYPAD.

      *> ===============================================================
      *> DISPLAY-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-KEYPAD.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-KEY-SEQUENCE-IDX       PIC 9(3).
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX.
           DISPLAY "Keypad #" IN-KP-IDX
           DISPLAY "@" KP-KEY(
               IN-KP-IDX,
               KP-CUR-ROW(IN-KP-IDX),
               KP-CUR-COL(IN-KP-IDX)
           )
           PERFORM VARYING LS-KEY-SEQUENCE-IDX FROM 1 BY 1
               UNTIL LS-KEY-SEQUENCE-IDX >
                   KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX)
               DISPLAY KP-KEY-SEQUENCE-KEY(
                   IN-KP-IDX, LS-KEY-SEQUENCE-IDX
               ) NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           DISPLAY "---"
           .

       END PROGRAM DISPLAY-KEYPAD.

      *> ===============================================================
      *> USE-KEYPAD-SEQUENCE.
      *> Apply the sequence of actions, coming from a directional
      *> keypad, to a numerical or directional keypad.
      *>
      *> Returns 0 if the action sequence was successful, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USE-KEYPAD-SEQUENCE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-SEQUENCE-LENGTH                      PIC 9(3).
       01  LS-SEQUENCE-IDX                         PIC 9(3).
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                               PIC 9(1) VALUE 1.
       01  IN-SEQUENCE                             PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-SEQUENCE.

           SET LS-SEQUENCE-LENGTH TO LENGTH OF FUNCTION
           TRIM(IN-SEQUENCE)

           PERFORM VARYING LS-SEQUENCE-IDX FROM 1 BY 1
               UNTIL LS-SEQUENCE-IDX > LS-SEQUENCE-LENGTH

               CALL "USE-KEYPAD" USING BY REFERENCE
                   KP-GRP
                   IN-KP-IDX
                   IN-SEQUENCE(LS-SEQUENCE-IDX:1)
               IF RETURN-CODE NOT = 0
                   DISPLAY "Invalid move at " LS-SEQUENCE-IDX "("
                       IN-SEQUENCE(LS-SEQUENCE-IDX:1) ")"
                   MOVE 1 TO RETURN-CODE
                   GOBACK
               END-IF
           END-PERFORM
           MOVE 0 TO RETURN-CODE.

       END PROGRAM USE-KEYPAD-SEQUENCE.

      *> ===============================================================
      *> USE-KEYPAD.
      *> Returns 0 if the action was successful, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USE-KEYPAD RECURSIVE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "constants" IN "21".
       01  LS-NEXT-KP-IDX                          PIC 9(1).
       01  LS-NEXT-ROW                             PIC 9(1).
       01  LS-NEXT-COL                             PIC 9(1).
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                               PIC 9(1) VALUE 1.
       01  IN-ACTION                               PIC X(1).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-ACTION.

           display "keypad #" in-kp-idx ", hit " in-action

           COMPUTE LS-NEXT-KP-IDX = IN-KP-IDX + 1
           SET LS-NEXT-ROW TO KP-CUR-ROW(IN-KP-IDX)
           SET LS-NEXT-COL TO KP-CUR-COL(IN-KP-IDX)
           EVALUATE IN-ACTION
               WHEN "^"
                   COMPUTE LS-NEXT-ROW = KP-CUR-ROW(IN-KP-IDX)- 1
               WHEN ">"
                   COMPUTE LS-NEXT-COL = KP-CUR-COL(IN-KP-IDX)+ 1
               WHEN "<"
                   COMPUTE LS-NEXT-COL = KP-CUR-COL(IN-KP-IDX)- 1
               WHEN "v"
                   COMPUTE LS-NEXT-ROW = KP-CUR-ROW(IN-KP-IDX)+ 1
               WHEN "A"
      *> A button was pressed, add it to the sequence and exit.
                   ADD 1 TO KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX)
                   SET KP-KEY-SEQUENCE-KEY(
                       IN-KP-IDX,
                       KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX)
                   ) TO KP-KEY(
                       IN-KP-IDX,
                       KP-CUR-ROW(IN-KP-IDX),
                       KP-CUR-COL(IN-KP-IDX)
                   )
                   IF KP-TYPE(IN-KP-IDX) = C-TYPE-DIRECTIONAL
      *> When a directional A key is pushed, we uhuh??
                       CALL "USE-KEYPAD" USING BY REFERENCE
                           KP-GRP
                           LS-NEXT-KP-IDX
                           KP-KEY(
                               IN-KP-IDX,
                               KP-CUR-ROW(IN-KP-IDX),
                               KP-CUR-COL(IN-KP-IDX)
                       )
                   MOVE 0 TO RETURN-CODE
                   GOBACK
           END-EVALUATE

      *> We're directed to move to another key.
      *> Make sure we're in the bounds:
           EVALUATE LS-NEXT-ROW ALSO LS-NEXT-COL
      *> Can't go above or below the keyboard
               WHEN = 0 OR > KP-HEIGHT(IN-KP-IDX) ALSO ANY
                   DISPLAY "Off the keypad vertically"
                   MOVE 1 TO RETURN-CODE
                   GOBACK
      *> Can't go left or right of the keyboard
               WHEN ANY ALSO = 0 OR = 4
                   DISPLAY "Off the keypad horizontally at "
                       LS-NEXT-ROW "," LS-NEXT-COL

                   MOVE 1 TO RETURN-CODE
                   GOBACK
           END-EVALUATE
      *> Make sure we're not on a gap
           IF KP-KEY(IN-KP-IDX, LS-NEXT-ROW, LS-NEXT-COL) = " "
               MOVE 1 TO RETURN-CODE
           END-IF
      *> Valid key movement, move our position there.
           SET KP-CUR-ROW(IN-KP-IDX) TO LS-NEXT-ROW
           SET KP-CUR-COL(IN-KP-IDX) TO LS-NEXT-COL
           MOVE 0 TO RETURN-CODE
           .
       END PROGRAM USE-KEYPAD.

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
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-COMPLEXITY                        PIC 9(6).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-TARGET-SEQUENCE
           OUT-COMPLEXITY.

           display "calculate complexity '"
           function trim(in-target-sequence) "'"
           SET LS-NEXT-TEST-SEQUENCE TO IN-TARGET-SEQUENCE

           PERFORM UNTIL LS-KP-IDX = 0
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
                   LS-SHORTEST-INPUT-SEQUENCE
               DISPLAY "Shortest sequence: "
                   LS-SHORTEST-INPUT-SEQUENCE
                   " (" LENGTH OF FUNCTION TRIM(
                   LS-SHORTEST-INPUT-SEQUENCE
               ) ")"
               SET LS-NEXT-TEST-SEQUENCE TO
                   LS-SHORTEST-INPUT-SEQUENCE
               ADD -1 TO LS-KP-IDX
           END-PERFORM
           COMPUTE OUT-COMPLEXITY =
               LENGTH OF FUNCTION TRIM(
                   LS-SHORTEST-INPUT-SEQUENCE
               ) * FUNCTION NUMVAL(IN-TARGET-SEQUENCE)
           DISPLAY "Complexity " OUT-COMPLEXITY

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
       01  LS-QUEUE-VALUE-ACTION                 PIC X(1).
       01  LS-QUEUE-VALUE-SEQUENCE               PIC X(100).
       01  LS-QUEUE-VALUE-ACTION-HIST            PIC X(100).
       01  LS-TOTAL-SEQUENCE-SO-FAR              PIC X(100) VALUE SPACE.
       01  LS-NEXT-ROW                           PIC 9(1).
       01  LS-NEXT-COL                           PIC 9(1).
       01  LS-NEXT-ACTION                        PIC 9(1).
       01  LS-NEXT-SEQUENCE                      PIC X(100).
       01  LS-NEXT-ACTION-HIST                   PIC X(100).
       COPY "queue" IN "21".

       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                             PIC 9(1) VALUE 1.
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-SHORTEST-INPUT-SEQUENCE           PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-TARGET-SEQUENCE
           OUT-SHORTEST-INPUT-SEQUENCE.

           SET LS-QUEUE-VALUE-ROW TO KP-CUR-ROW(IN-KP-IDX)
           SET LS-QUEUE-VALUE-COL TO KP-CUR-COL(IN-KP-IDX)
           SET LS-QUEUE-VALUE-ACTION TO SPACE
           SET LS-QUEUE-VALUE-SEQUENCE TO SPACE
           SET LS-QUEUE-VALUE-ACTION-HIST TO SPACE
           CALL "ENQUEUE" USING
               QUEUE-GRP
               LS-QUEUE-VALUE-ROW
               LS-QUEUE-VALUE-COL
               LS-QUEUE-VALUE-ACTION
               LS-QUEUE-VALUE-SEQUENCE
               LS-QUEUE-VALUE-ACTION-HIST

           PERFORM UNTIL QUEUE-SIZE = 0
               CALL "DEQUEUE" USING
               QUEUE-GRP
               LS-QUEUE-VALUE-ROW
               LS-QUEUE-VALUE-COL
               LS-QUEUE-VALUE-ACTION
               LS-QUEUE-VALUE-SEQUENCE
               LS-QUEUE-VALUE-ACTION-HIST

      *>         display "dequeue. so far: "
      *>             function trim(ls-total-sequence-so-far)
      *>             ", action: " ls-queue-value-action
      *>             ", seq: " function trim(ls-queue-value-sequence)
      *>             ", act hist:"
      *>             function trim(ls-queue-value-action-hist)
      *>             ", key: " kp-key(in-kp-idx, ls-queue-value-row,
      *>             ls-queue-value-col)

               IF LENGTH OF FUNCTION TRIM(LS-QUEUE-VALUE-SEQUENCE)
                   = LENGTH OF FUNCTION TRIM(LS-TOTAL-SEQUENCE-SO-FAR)

                   STRING FUNCTION TRIM(LS-QUEUE-VALUE-SEQUENCE)
                       KP-KEY(
                           IN-KP-IDX,
                           LS-QUEUE-VALUE-ROW,
                           LS-QUEUE-VALUE-COL
                       )
                       INTO LS-NEXT-SEQUENCE
                   END-STRING

                   STRING FUNCTION TRIM(LS-QUEUE-VALUE-ACTION-HIST)
                       LS-QUEUE-VALUE-ACTION
                       INTO LS-NEXT-ACTION-HIST
                   END-STRING

      *> We found the target sequence, done!
                   IF LS-NEXT-SEQUENCE = IN-TARGET-SEQUENCE
                       STRING
                           FUNCTION TRIM(LS-QUEUE-VALUE-ACTION-HIST)
                           LS-QUEUE-VALUE-ACTION
                           "A"
                           INTO OUT-SHORTEST-INPUT-SEQUENCE
                       END-STRING
                       GOBACK
                   END-IF
      *> We found a substring:
                   IF IN-TARGET-SEQUENCE(
                       1:LENGTH OF FUNCTION TRIM(LS-NEXT-SEQUENCE)
                   ) = LS-NEXT-SEQUENCE
                       STRING FUNCTION TRIM(LS-NEXT-ACTION-HIST)
                           "A"
                           INTO LS-NEXT-ACTION-HIST
                       END-STRING
                       SET LS-TOTAL-SEQUENCE-SO-FAR TO LS-NEXT-SEQUENCE
                   END-IF

      *> Try up
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW - 1
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL
                   SET LS-NEXT-ACTION TO "^"
                   PERFORM TRY-NEIGHBOR
      *> Try right
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL + 1
                   SET LS-NEXT-ACTION TO ">"
                   PERFORM TRY-NEIGHBOR
      *> Try bottom
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW + 1
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL
                   SET LS-NEXT-ACTION TO "v"
                   PERFORM TRY-NEIGHBOR
      *> Try left
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL - 1
                   SET LS-NEXT-ACTION TO "<"
                   PERFORM TRY-NEIGHBOR
      *> Try the same?
                   COMPUTE LS-NEXT-ROW = LS-QUEUE-VALUE-ROW
                   COMPUTE LS-NEXT-COL = LS-QUEUE-VALUE-COL
                   SET LS-NEXT-ACTION TO " "
                   PERFORM TRY-NEIGHBOR

               END-IF


           END-PERFORM

           GOBACK.

       TRY-NEIGHBOR.
           IF LS-NEXT-ROW >= 1 AND LS-NEXT-ROW <= KP-HEIGHT(IN-KP-IDX)
               AND LS-NEXT-COL >= 1 AND LS-NEXT-COL <= 3
               AND KP-KEY(IN-KP-IDX, LS-NEXT-ROW, LS-NEXT-COL) NOT =
               SPACE

               CALL "ENQUEUE" USING BY REFERENCE
                   QUEUE-GRP
                   LS-NEXT-ROW
                   LS-NEXT-COL
                   LS-NEXT-ACTION
                   LS-TOTAL-SEQUENCE-SO-FAR
                   LS-NEXT-ACTION-HIST


           END-IF
           .
       END PROGRAM FIND-SHORTEST-INPUT-SEQUENCE.

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

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       display ls-line
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.
