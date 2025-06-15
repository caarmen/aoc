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
       01  KP-GRP.
           05  KP OCCURS 26 TIMES.
               10  KP-TYPE                         PIC 9(1).
               10  KP-HEIGHT                       PIC 9(1) VALUE 4.
               10  KP-ROWS OCCURS 4 TIMES.
                   15  KP-COLS OCCURS 3 TIMES.
                       20  KP-KEY                  PIC X(1).
               10  KP-CUR-ROW                      PIC 9(1) VALUE 4.
               10  KP-CUR-COL                      PIC 9(1) VALUE 3.
               10  KP-KEY-SEQUENCE.
                   15  KP-KEY-SEQUENCE-LENGTH      PIC 9(3) VALUE 0.
                   15  KP-KEY-SEQUENCE-KEYS OCCURS 100 TIMES.
                       20  KP-KEY-SEQUENCE-KEY     PIC X(1).
