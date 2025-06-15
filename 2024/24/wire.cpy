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
       01  WIRE-GRP.
           05  WIRE-SIZE                    PIC 9(3) VALUE 0.
           05  WIRE-INPUT-BIT-SIZE          PIC 9(2).
           05  WIRES OCCURS 1 TO 999 TIMES
               DEPENDING ON WIRE-SIZE
               ASCENDING KEY IS WIRE-NAME
               INDEXED BY WIRE-IDX
               .
               10  WIRE-NAME                PIC X(3).
               10  WIRE-INPUT-1             PIC X(3).
               10  WIRE-INPUT-2             PIC X(3).
               10  WIRE-OUTPUT              PIC 9(1).
               10  WIRE-GATE                PIC 9(1).
                
