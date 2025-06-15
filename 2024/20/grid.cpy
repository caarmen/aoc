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
       01  GRID-GRP.
           05  GRID-SIZE                          PIC 9(3) VALUE 0.
           05  GRID-START-ROW                     PIC 9(3).
           05  GRID-START-COL                     PIC 9(3).
           05  GRID-END-ROW                       PIC 9(3).
           05  GRID-END-COL                       PIC 9(3).
           05  GRID-FULL-PATH-LENGTH              PIC 9(5).
           05  GRID-ROWS OCCURS 1 TO 141 TIMES
               DEPENDING ON GRID-SIZE
               INDEXED BY GRID-ROW-INDEX.
               10  GRID-COLS OCCURS 141 TIMES
                   INDEXED BY GRID-COL-INDEX.
               15  GRID-CELL                      PIC X(1).
               15  GRID-PATH-PARENT-ROW           PIC 9(3).
               15  GRID-PATH-PARENT-COL           PIC 9(3).
               15  GRID-DIST-TO-END               PIC 9(5).
