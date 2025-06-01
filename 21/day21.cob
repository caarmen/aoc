       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY21.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-TEST-SEQUENCE          PIC X(30).
       01  LS-KP-IDX                 PIC 9(1) VALUE 1.
       COPY "keypad" IN "21".

       PROCEDURE DIVISION.

           ACCEPT LS-TEST-SEQUENCE FROM COMMAND-LINE

      *> Init the first three directional keypads
           PERFORM VARYING LS-KP-IDX FROM 1 BY 1 UNTIL
               LS-KP-IDX > 3
               CALL "INIT-DIRECTIONAL-KEYPAD" USING BY REFERENCE
                   KP-GRP
                   LS-KP-IDX
           END-PERFORM

      *> Init the last numeric keypad
           SET LS-KP-IDX TO 4
           CALL "INIT-NUMERIC-KEYPAD" USING BY REFERENCE
               KP-GRP
               LS-KP-IDX

           CALL "DISPLAY-NUMERIC-KEYPAD" USING BY REFERENCE
               KP-GRP
               LS-KP-IDX

           CALL "USE-NUMERIC-KEYPAD-SEQUENCE" USING BY REFERENCE
               KP-GRP
               LS-KP-IDX
               LS-TEST-SEQUENCE

           CALL "DISPLAY-NUMERIC-KEYPAD" USING BY REFERENCE
               KP-GRP
               LS-KP-IDX
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
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           .

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
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX.

           SET KP-HEIGHT(IN-KP-IDX) TO 2
           MOVE " ^A" TO KP-ROWS(IN-KP-IDX,1)
           MOVE "<v>" TO KP-ROWS(IN-KP-IDX,2)
           SET KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX) TO 0
           SET KP-CUR-ROW(IN-KP-IDX) TO 1
           SET KP-CUR-COL(IN-KP-IDX) TO 3

           .
       END PROGRAM INIT-DIRECTIONAL-KEYPAD.

      *> ===============================================================
      *> DISPLAY-NUMERIC-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-NUMERIC-KEYPAD.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX.
           DISPLAY "@" KP-KEY(
               IN-KP-IDX,
               KP-CUR-ROW(IN-KP-IDX),
               KP-CUR-COL(IN-KP-IDX)
           )
           PERFORM VARYING KP-KEY-SEQUENCE-IDX FROM 1 BY 1
               UNTIL KP-KEY-SEQUENCE-IDX >
                   KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX)
               DISPLAY KP-KEY-SEQUENCE-KEY(
                   IN-KP-IDX,KP-KEY-SEQUENCE-IDX
               ) NO ADVANCING
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
       COPY "keypad" IN "21".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.
       01  IN-SEQUENCE                             PIC X(30).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-SEQUENCE.

           SET LS-SEQUENCE-LENGTH TO LENGTH OF FUNCTION
               TRIM(IN-SEQUENCE)

           PERFORM VARYING LS-SEQUENCE-IDX FROM 1 BY 1
               UNTIL LS-SEQUENCE-IDX > LS-SEQUENCE-LENGTH

               CALL "USE-NUMERIC-KEYPAD" USING BY REFERENCE
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
       COPY "keypad" IN "21".
       01  IN-KP-IDX                               PIC 9(1) VALUE 1.
       01  IN-ACTION                               PIC X(1).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-ACTION.

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
