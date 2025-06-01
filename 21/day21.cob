       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY21.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-TEST-SEQUENCE          PIC X(30).
       COPY "nk" IN "21".

       PROCEDURE DIVISION.

           ACCEPT LS-TEST-SEQUENCE FROM COMMAND-LINE

           CALL "INIT-NUMERIC-KEYPAD" USING BY REFERENCE
               NK-GRP

           CALL "DISPLAY-NUMERIC-KEYPAD" USING BY REFERENCE
               NK-GRP

           CALL "USE-NUMERIC-KEYPAD-SEQUENCE" USING BY REFERENCE
               NK-GRP
               LS-TEST-SEQUENCE

           CALL "DISPLAY-NUMERIC-KEYPAD" USING BY REFERENCE
               NK-GRP
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
       LINKAGE SECTION.
       COPY "nk" IN "21".

       PROCEDURE DIVISION USING BY REFERENCE
           NK-GRP.

           MOVE "789" TO NK-ROWS(1)
           MOVE "456" TO NK-ROWS(2)
           MOVE "123" TO NK-ROWS(3)
           MOVE " 0A" TO NK-ROWS(4)
           SET NK-KEY-SEQUENCE-LENGTH TO 0
           SET NK-CUR-ROW TO 4
           SET NK-CUR-COL TO 3

           .
       END PROGRAM INIT-NUMERIC-KEYPAD.

      *> ===============================================================
      *> DISPLAY-NUMERIC-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-NUMERIC-KEYPAD.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "nk" IN "21".

       PROCEDURE DIVISION USING BY REFERENCE
           NK-GRP.
           DISPLAY "@" NK-KEY(NK-CUR-ROW, NK-CUR-COL)
           PERFORM VARYING NK-KEY-SEQUENCE-IDX FROM 1 BY 1
               UNTIL NK-KEY-SEQUENCE-IDX > NK-KEY-SEQUENCE-LENGTH
               DISPLAY NK-KEY-SEQUENCE-KEY(NK-KEY-SEQUENCE-IDX)
                   NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           .

       END PROGRAM DISPLAY-NUMERIC-KEYPAD.

      *> ===============================================================
      *> USE-NUMERIC-KEYPAD-SEQUENCE.
      *> Apply the sequence of actions, coming from a directional
      *> keypad, to the numerical keypad.
      *>
      *> Returns 0 if the action sequence was successful, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USE-NUMERIC-KEYPAD-SEQUENCE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-SEQUENCE-LENGTH                      PIC 9(3).
       01  LS-SEQUENCE-IDX                         PIC 9(3).
       LINKAGE SECTION.
       COPY "nk" IN "21".
       01  IN-SEQUENCE                             PIC X(30).

       PROCEDURE DIVISION USING BY REFERENCE
           NK-GRP
           IN-SEQUENCE.

           SET LS-SEQUENCE-LENGTH TO LENGTH OF FUNCTION
               TRIM(IN-SEQUENCE)

           PERFORM VARYING LS-SEQUENCE-IDX FROM 1 BY 1
               UNTIL LS-SEQUENCE-IDX > LS-SEQUENCE-LENGTH

               CALL "USE-NUMERIC-KEYPAD" USING BY REFERENCE
                   NK-GRP
                   IN-SEQUENCE(LS-SEQUENCE-IDX:1)
               IF RETURN-CODE NOT = 0
                   DISPLAY "Invalid move at " LS-SEQUENCE-IDX "("
                       IN-SEQUENCE(LS-SEQUENCE-IDX:1) ")"
                   MOVE 1 TO RETURN-CODE
                   GOBACK
               END-IF
           END-PERFORM
           MOVE 0 TO RETURN-CODE.

       END PROGRAM USE-NUMERIC-KEYPAD-SEQUENCE.

      *> ===============================================================
      *> USE-NUMERIC-KEYPAD.
      *> Returns 0 if the action was successful, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USE-NUMERIC-KEYPAD.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-NEXT-ROW                             PIC 9(1).
       01  LS-NEXT-COL                             PIC 9(1).
       LINKAGE SECTION.
       COPY "nk" IN "21".
       01  IN-ACTION                               PIC X(1).

       PROCEDURE DIVISION USING BY REFERENCE
           NK-GRP
           IN-ACTION.

           SET LS-NEXT-ROW TO NK-CUR-ROW
           SET LS-NEXT-COL TO NK-CUR-COL
           EVALUATE IN-ACTION
               WHEN "^"
                   COMPUTE LS-NEXT-ROW = NK-CUR-ROW - 1
               WHEN ">"
                   COMPUTE LS-NEXT-COL = NK-CUR-COL + 1
               WHEN "<"
                   COMPUTE LS-NEXT-COL = NK-CUR-COL - 1
               WHEN "v"
                   COMPUTE LS-NEXT-ROW = NK-CUR-ROW + 1
               WHEN "A"
      *> A button was pressed, add it to the sequence and exit.
                   ADD 1 TO NK-KEY-SEQUENCE-LENGTH
                   SET NK-KEY-SEQUENCE-KEY(NK-KEY-SEQUENCE-LENGTH)
                       TO NK-KEY(NK-CUR-ROW, NK-CUR-COL)
                   MOVE 0 TO RETURN-CODE
                   GOBACK
           END-EVALUATE

      *> We're directed to move to another key.
      *> Make sure it's valid:
           EVALUATE LS-NEXT-ROW ALSO LS-NEXT-COL
      *> Can't go into the gap:
               WHEN = 4 ALSO = 1
                   DISPLAY "Positioned on gap"
                   MOVE 1 TO RETURN-CODE
                   GOBACK
      *> Can't go above or below the keyboard
               WHEN = 0 OR = 5 ALSO ANY
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
      *> Valid key movement, move our position there.
           SET NK-CUR-ROW TO LS-NEXT-ROW
           SET NK-CUR-COL TO LS-NEXT-COL
           MOVE 0 TO RETURN-CODE
           .
       END PROGRAM USE-NUMERIC-KEYPAD.

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
