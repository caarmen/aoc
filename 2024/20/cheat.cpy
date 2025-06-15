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
       01  CHEAT-GRP.
           05  CHEAT-SIZE                        PIC 9(8) VALUE 0.
           05  CHEATS OCCURS 1 TO 99999999 TIMES
               DEPENDING ON CHEAT-SIZE
               ASCENDING KEY IS CHEAT-ID
               INDEXED BY CHEAT-INDEX.
               10  CHEAT-ID                      PIC 9(12).
               10  CHEAT-DISTANCE-SAVED          PIC 9(5).
