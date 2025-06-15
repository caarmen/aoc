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

       01  STACK-GRP.
           05  STACK-SIZE                PIC 9(5) USAGE COMP VALUE 0.
           05  STACK-ITEMS
               OCCURS 1 TO 20000 TIMES
               DEPENDING ON STACK-SIZE.
               10  STACK-ITEM-ROW        PIC 9(3).
               10  STACK-ITEM-COL        PIC 9(3).
               10  STACK-PREV-ITEM-ROW   PIC 9(3) VALUE 0.
               10  STACK-PREV-ITEM-COL   PIC 9(3) VALUE 0.
