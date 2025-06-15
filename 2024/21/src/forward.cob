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
       COPY "keypad" IN "21/src".
       01  IN-KP-IDX                               PIC 9(1) VALUE 1.
       01  IN-SEQUENCE                             PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-SEQUENCE.

           display "here " in-kp-idx ": "
           kp-key-sequence-length(in-kp-idx)
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
       COPY "constants" IN "21/src".
       01  LS-NEXT-KP-IDX                          PIC 9(1).
       01  LS-NEXT-ROW                             PIC 9(1).
       01  LS-NEXT-COL                             PIC 9(1).
       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
       01  IN-KP-IDX                               PIC 9(1) VALUE 1.
       01  IN-ACTION                               PIC X(1).

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX
           IN-ACTION.

           PERFORM IN-KP-IDX TIMES
               DISPLAY SPACE NO ADVANCING
           END-PERFORM
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

