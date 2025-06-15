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

      *> ===============================================================
      *> GET-FROM-CACHE
      *> Read an item from the cache.
      *> Return 0 if the item was found, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-FROM-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "21/src".
       01  IN-CACHE-KEY                    PIC X(4).
       01  OUT-CACHE-VALUE                 PIC 9(14).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           OUT-CACHE-VALUE
           OUT-RESULT.

           SEARCH ALL CACHE-CALCS
               AT END
                   MOVE 1 TO OUT-RESULT
               WHEN CACHE-KEY(CACHE-INDEX) = IN-CACHE-KEY
                   MOVE CACHE-VALUE(CACHE-INDEX) TO OUT-CACHE-VALUE
                   MOVE 0 TO OUT-RESULT

           GOBACK.
       END PROGRAM GET-FROM-CACHE.

      *> ===============================================================
      *> ADD-TO-CACHE
      *> Add an item to the cache
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-TO-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "21/src".
       01  IN-CACHE-KEY                    PIC X(4).
       01  IN-CACHE-VALUE                  PIC 9(14).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           IN-CACHE-VALUE
           OUT-RESULT.

           ADD 1 TO CACHE-SIZE
           SET CACHE-VALUE(CACHE-SIZE) TO IN-CACHE-VALUE
           SET CACHE-KEY(CACHE-SIZE) TO IN-CACHE-KEY

           SORT CACHE-CALCS
           MOVE 0 TO OUT-RESULT
           GOBACK.
       END PROGRAM ADD-TO-CACHE.

      *> ===============================================================
      *> UPDATE-CACHE
      *> Update an item in the cache
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE-CACHE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-CACHE-INDEX                  PIC 9(3).
       LINKAGE SECTION.
       COPY "cache" IN "21/src".
       01  IN-CACHE-KEY                    PIC X(4).
       01  IN-CACHE-VALUE                  PIC 9(14).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           IN-CACHE-VALUE.

           SEARCH ALL CACHE-CALCS
               AT END
                   ADD 1 TO CACHE-SIZE
                   SET LS-CACHE-INDEX TO CACHE-SIZE
                   SET CACHE-KEY(LS-CACHE-INDEX) TO IN-CACHE-KEY
               WHEN CACHE-KEY(CACHE-INDEX) = IN-CACHE-KEY
                   SET LS-CACHE-INDEX TO CACHE-INDEX

           SET CACHE-VALUE(LS-CACHE-INDEX) TO IN-CACHE-VALUE

           SORT CACHE-CALCS
           GOBACK.
       END PROGRAM UPDATE-CACHE.

      *> ===============================================================
      *> DISPLAY-CACHE
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "21/src".

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP.

           PERFORM VARYING CACHE-INDEX FROM 1 BY 1 UNTIL
               CACHE-INDEX > CACHE-SIZE
               DISPLAY CACHE-INDEX ": " CACHE-KEY(CACHE-INDEX) "="
               CACHE-VALUE(CACHE-INDEX)
           END-PERFORM
           GOBACK.
       END PROGRAM DISPLAY-CACHE.
