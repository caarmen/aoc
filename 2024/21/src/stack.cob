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
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "21/src".
       01  OUT-ITEM-LEVEL                   PIC 9(2).
       01  OUT-ITEM-HISTORY                 PIC X(100).
       01  OUT-ITEM-START-KEY               PIC X(1).
       01  OUT-ITEM-END-KEY                 PIC X(1).
       01  OUT-RESULT                       PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           STACK-GRP
           OUT-ITEM-LEVEL
           OUT-ITEM-HISTORY
           OUT-ITEM-START-KEY
           OUT-ITEM-END-KEY
           RETURNING OUT-RESULT
           .

           IF STACK-SIZE > 0
               MOVE STACK-ITEM-LEVEL(STACK-SIZE) TO OUT-ITEM-LEVEL
               MOVE STACK-ITEM-HISTORY(STACK-SIZE) TO OUT-ITEM-HISTORY
               MOVE STACK-ITEM-START-KEY(STACK-SIZE)
                   TO OUT-ITEM-START-KEY
               MOVE STACK-ITEM-END-KEY(STACK-SIZE) TO OUT-ITEM-END-KEY
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
       01  IN-ITEM-LEVEL                   PIC 9(2).
       01  IN-ITEM-HISTORY                 PIC X(100).
       01  IN-ITEM-START-KEY               PIC X(1).
       01  IN-ITEM-END-KEY                 PIC X(1).
       COPY "stack" IN "21/src".
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           STACK-GRP
           IN-ITEM-LEVEL
           IN-ITEM-HISTORY
           IN-ITEM-START-KEY
           IN-ITEM-END-KEY
           RETURNING OUT-RESULT.

           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-LEVEL(STACK-SIZE) TO IN-ITEM-LEVEL
           SET STACK-ITEM-HISTORY(STACK-SIZE) TO IN-ITEM-HISTORY
           SET STACK-ITEM-START-KEY(STACK-SIZE) TO IN-ITEM-START-KEY
           SET STACK-ITEM-END-KEY(STACK-SIZE) TO IN-ITEM-END-KEY

           MOVE 0 TO OUT-RESULT
           GOBACK.
       END FUNCTION PUSH-TO-STACK.

