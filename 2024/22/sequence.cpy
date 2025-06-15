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
       01  SEQUENCE-GRP.
           05  MAX-TOTAL-PRICE       PIC 9(6) VALUE 0.
           05  BEST-SEQUENCE-STR     PIC X(8).
           05  SEQUENCES-SIZE        PIC 9(6) VALUE 0.
           05  SEQUENCES OCCURS 1 TO 130000 TIMES
               DEPENDING ON SEQUENCES-SIZE
               ASCENDING KEY IS SEQUENCE-STR
               INDEXED BY SEQUENCE-IDX
               .
               10  SEQUENCE-STR      PIC X(8).
               10  TOTAL-PRICE       PIC 9(6) VALUE 0.
               10  PRICE-SIZE        PIC 9(4) VALUE 0.
               10  PRICES OCCURS 2200 TIMES
                   ASCENDING KEY IS BUYER-IDX
                   INDEXED BY PRICE-IDX.
                   15  BUYER-IDX     PIC 9(4).
                   15  BUYER-PRICE   PIC 9(1).


