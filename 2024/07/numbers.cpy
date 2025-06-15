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

       01  NUMS-GRP.
           05  CALC-VALUE                      PIC 9(18) COMP-3.
           05  NUMS-SIZE                       PIC 9(2) USAGE COMP.
           05  NUMS
               OCCURS 1 TO 30
               DEPENDING ON NUMS-SIZE
               INDEXED BY NUMS-INDEX.
               10  NUM                         PIC 9(4) USAGE COMP.
