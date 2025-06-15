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
       01  QUEUE-MAX-SIZE                         CONSTANT 999.
       01  QUEUE-GRP.
           05  QUEUE-SIZE                         PIC 9(5) VALUE 0.
           05  QUEUE-HEAD                         PIC 9(4) VALUE 1.
           05  QUEUE-TAIL                         PIC 9(4) VALUE 0.
           05  QUEUE-ARR OCCURS 999 TIMES.
               10  QUEUE-VALUE                    PIC X(50).
