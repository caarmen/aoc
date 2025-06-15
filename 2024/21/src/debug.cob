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
      *> DISPLAY-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-KEYPAD.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-KEY-SEQUENCE-IDX       PIC 9(3).
       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
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
           DISPLAY "Hit " KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX) " keys."
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

