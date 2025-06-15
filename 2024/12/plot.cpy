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
       01  PLOT-GRP.
           05  PLOT-SIZE                       PIC 9(3) COMP VALUE 0.
           05  PLOT-ROW OCCURS 1 TO 140 TIMES
               DEPENDING ON PLOT-SIZE
               INDEXED BY PLOT-ROW-INDEX.
               10  PLOT-COL OCCURS 140 TIMES
                   INDEXED BY PLOT-COL-INDEX.
                   15 PLOT-CELL                PIC X(1).
                   15 VISITED                  PIC 9(1) VALUE 0.
       01  REGION-GRP.
           05  REGION-COUNT                    PIC 9(3) COMP VALUE 0.
           05  REGIONS OCCURS 1 TO 26 TIMES  
               DEPENDING ON REGION-COUNT
                   ASCENDING KEY IS REGION
                   INDEXED BY REGION-INDEX.
               10  REGION                      PIC X(1). 
