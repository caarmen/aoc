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

      *> ===============================================================
      *> INIT-NUMERIC-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT-NUMERIC-KEYPAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
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
       COPY "constants" IN "21/src".
       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
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

