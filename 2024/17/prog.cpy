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
       01  PROG-GRP.
           05  PROG-REG-A            PIC 9(16) COMP.
           05  PROG-REG-B            PIC 9(16) COMP.
           05  PROG-REG-C            PIC 9(16) COMP.
           05  PROG-SIZE             PIC 9(2).
           05  PROG-INSTR-PTR        PIC 9(16).
           05  PROG-ITEMS OCCURS 1 TO 24 TIMES
               DEPENDING ON PROG-SIZE.
               10  PROG-ITEM         PIC 9(1).
