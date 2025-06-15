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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY08.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "antennas" IN "08".
       COPY "set" IN "08".
       01  LS-GRID-SIZE                          PIC 9(2) COMP.
       01  LS-ANTENNA-PAIR-INDEX                 PIC 9(2) COMP.

       PROCEDURE DIVISION.

           CALL "PARSE-ANTENNAS" USING
               BY REFERENCE ANTENNAS-GRP
               BY REFERENCE LS-GRID-SIZE

      *> Process each antenna symbol:
           PERFORM VARYING ANTENNAS-INDEX FROM 1 BY 1
               UNTIL ANTENNAS-INDEX > ANTENNAS-SIZE
      *> Go through each location of this symbol:
               PERFORM VARYING ANTENNA-COORDS-INDEX FROM 1 BY 1
                   UNTIL ANTENNA-COORDS-INDEX >
                       ANTENNA-COORDS-SIZE(ANTENNAS-INDEX)
      *> Go through the other locations of this symbol:
                   PERFORM VARYING LS-ANTENNA-PAIR-INDEX FROM
                       ANTENNA-COORDS-INDEX BY 1
                       UNTIL LS-ANTENNA-PAIR-INDEX >
                           ANTENNA-COORDS-SIZE(ANTENNAS-INDEX)
                       IF LS-ANTENNA-PAIR-INDEX NOT =
                           ANTENNA-COORDS-INDEX
                           CALL "CALCULATE-ANTINODES" USING
                               BY REFERENCE LS-GRID-SIZE
                               BY REFERENCE ANTENNA-ROW(
                                   ANTENNAS-INDEX,
                                   ANTENNA-COORDS-INDEX
                               )
                               BY REFERENCE ANTENNA-COL(
                                   ANTENNAS-INDEX,
                                   ANTENNA-COORDS-INDEX
                               )
                               BY REFERENCE ANTENNA-ROW(
                                   ANTENNAS-INDEX,
                                   LS-ANTENNA-PAIR-INDEX
                               )
                               BY REFERENCE ANTENNA-COL(
                                   ANTENNAS-INDEX,
                                   LS-ANTENNA-PAIR-INDEX
                               )
                               BY REFERENCE SET-GRP
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           DISPLAY "Found antinodes at " SET-SIZE " locations"

       .
       END PROGRAM DAY08.

      *> ===============================================================
      *> PARSE-ANTENNAS.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-ANTENNAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FD-DATA.
       01  F-DATA-RECORD                      PIC X(50).

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                       PIC X(30).
       01  LS-LINE                            PIC X(50).
       01  LS-ROW                             PIC 9(2) COMP VALUE 0.
       01  LS-COL                             PIC 9(2) COMP.
       01  LS-SYMBOL                          PIC X(1).

       LINKAGE SECTION.
       COPY "antennas" IN "08".
       01  OUT-GRID-SIZE                      PIC 9(2) COMP.

       PROCEDURE DIVISION USING
           BY REFERENCE ANTENNAS-GRP
           BY REFERENCE OUT-GRID-SIZE.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-DATA-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       ADD 1 TO LS-ROW
                       MOVE F-DATA-RECORD TO LS-LINE
                       COMPUTE OUT-GRID-SIZE = LENGTH OF
                           FUNCTION TRIM(LS-LINE)
                       PERFORM VARYING LS-COL FROM 1 BY 1
                           UNTIL LS-COL > OUT-GRID-SIZE
                           SET LS-SYMBOL TO LS-LINE(LS-COL:1)
                           IF LS-SYMBOL NOT = "."
                           THEN
                               CALL "ADD-ANTENNA" USING
                                   BY REFERENCE ANTENNAS-GRP
                                   BY REFERENCE LS-SYMBOL
                                   BY REFERENCE LS-ROW
                                   BY REFERENCE LS-COL
                           END-IF


                       END-PERFORM
           END-PERFORM
           CLOSE FD-DATA

           GOBACK.
       END PROGRAM PARSE-ANTENNAS.

      *> ===============================================================
      *> ADD-ANTENNA.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-ANTENNA.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "antennas" IN "08".
       01  IN-SYMBOL                          PIC X(1).
       01  IN-ROW                             PIC 9(2) COMP.
       01  IN-COL                             PIC 9(2) COMP.

       PROCEDURE DIVISION USING
           BY REFERENCE ANTENNAS-GRP
           BY REFERENCE IN-SYMBOL
           BY REFERENCE IN-ROW
           BY REFERENCE IN-COL.

           SET ANTENNAS-INDEX TO 1
           SEARCH ANTENNAS
               AT END
                   ADD 1 TO ANTENNAS-SIZE
                   SET ANTENNA-SYMBOL(ANTENNAS-INDEX) TO IN-SYMBOL
               WHEN ANTENNA-SYMBOL(ANTENNAS-INDEX) = IN-SYMBOL
                   CONTINUE
           END-SEARCH
           ADD 1 TO ANTENNA-COORDS-SIZE(ANTENNAS-INDEX)
           SET ANTENNA-ROW(
               ANTENNAS-INDEX,
               ANTENNA-COORDS-SIZE(ANTENNAS-INDEX)
           ) TO IN-ROW
           SET ANTENNA-COL(
               ANTENNAS-INDEX,
               ANTENNA-COORDS-SIZE(ANTENNAS-INDEX)
           ) TO IN-COL

           GOBACK.
       END PROGRAM ADD-ANTENNA.


      *> ===============================================================
      *> DISPLAY-ANTENNAS.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-ANTENNAS.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "antennas" IN "08".

       PROCEDURE DIVISION USING
           BY REFERENCE ANTENNAS-GRP.
           DISPLAY "read " ANTENNAS-SIZE " antennas"

           PERFORM VARYING ANTENNAS-INDEX FROM 1 BY 1
               UNTIL ANTENNAS-INDEX > ANTENNAS-SIZE
               DISPLAY ANTENNA-SYMBOL(ANTENNAS-INDEX) ": "
                   "("  ANTENNA-COORDS-SIZE(ANTENNAS-INDEX) ")"

               PERFORM VARYING ANTENNA-COORDS-INDEX FROM 1 BY 1
                   UNTIL ANTENNA-COORDS-INDEX >
                       ANTENNA-COORDS-SIZE(ANTENNAS-INDEX)
                       DISPLAY " "
                           ANTENNA-ROW(
                               ANTENNAS-INDEX,
                               ANTENNA-COORDS-INDEX
                           )
                           ","
                           ANTENNA-COL(
                               ANTENNAS-INDEX,
                               ANTENNA-COORDS-INDEX
                           )
               END-PERFORM
           END-PERFORM
           display "---"
           GOBACK.
       END PROGRAM DISPLAY-ANTENNAS.


      *> ===============================================================
      *> CALCULATE-ANTINODES
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-ANTINODES.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-DELTA-COLS                 PIC S9(2) COMP.
       01  LS-DELTA-ROWS                 PIC S9(2) COMP.
       01  LS-ANTINODE-ROW               PIC S9(2) COMP.
       01  LS-ANTINODE-COL               PIC S9(2) COMP.
       01  LS-ANTINODE-INDEX             PIC 9(2) COMP.
       01  LS-ANTINODE-VALUE             PIC 9(4) COMP.

       LINKAGE SECTION.
       01  IN-GRID-SIZE                  PIC 9(2) COMP.
       01  IN-ANTENNA-1-ROW              PIC 9(2) COMP.
       01  IN-ANTENNA-1-COL              PIC 9(2) COMP.
       01  IN-ANTENNA-2-ROW              PIC 9(2) COMP.
       01  IN-ANTENNA-2-COL              PIC 9(2) COMP.
       COPY "set" IN "08".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-GRID-SIZE
           BY REFERENCE IN-ANTENNA-1-ROW
           BY REFERENCE IN-ANTENNA-1-COL
           BY REFERENCE IN-ANTENNA-2-ROW
           BY REFERENCE IN-ANTENNA-2-COL
           BY REFERENCE SET-GRP.

           COMPUTE LS-DELTA-ROWS = IN-ANTENNA-2-ROW - IN-ANTENNA-1-ROW
           COMPUTE LS-DELTA-COLS = IN-ANTENNA-2-COL - IN-ANTENNA-1-COL
           COMPUTE LS-ANTINODE-ROW = IN-ANTENNA-1-ROW
           COMPUTE LS-ANTINODE-COL = IN-ANTENNA-1-COL
           PERFORM VARYING LS-ANTINODE-INDEX
               FROM 1 BY 1
               UNTIL LS-ANTINODE-ROW < 1
               OR LS-ANTINODE-ROW > IN-GRID-SIZE
               OR LS-ANTINODE-COL < 1
               OR LS-ANTINODE-COL > IN-GRID-SIZE

               COMPUTE LS-ANTINODE-VALUE = (100 * LS-ANTINODE-ROW)
                   + LS-ANTINODE-COL

               CALL "ADD-TO-SET" USING
                   BY REFERENCE LS-ANTINODE-VALUE
                   BY REFERENCE SET-GRP
               COMPUTE LS-ANTINODE-ROW = LS-ANTINODE-ROW - LS-DELTA-ROWS
               COMPUTE LS-ANTINODE-COL = LS-ANTINODE-COL - LS-DELTA-COLS
           END-PERFORM
           COMPUTE LS-ANTINODE-ROW = IN-ANTENNA-2-ROW
           COMPUTE LS-ANTINODE-COL = IN-ANTENNA-2-COL
           PERFORM VARYING LS-ANTINODE-INDEX
               FROM 1 BY 1
               UNTIL LS-ANTINODE-ROW < 1
               OR LS-ANTINODE-ROW > IN-GRID-SIZE
               OR LS-ANTINODE-COL < 1
               OR LS-ANTINODE-COL > IN-GRID-SIZE

               COMPUTE LS-ANTINODE-VALUE = (100 * LS-ANTINODE-ROW)
                   + LS-ANTINODE-COL

               CALL "ADD-TO-SET" USING
                   BY REFERENCE LS-ANTINODE-VALUE
                   BY REFERENCE SET-GRP
               COMPUTE LS-ANTINODE-ROW = LS-ANTINODE-ROW + LS-DELTA-ROWS
               COMPUTE LS-ANTINODE-COL = LS-ANTINODE-COL + LS-DELTA-COLS
           END-PERFORM

           GOBACK.

       END PROGRAM CALCULATE-ANTINODES.

      *> ===============================================================
      *> ADD-TO-SET.
      *> Returns 1 if we added the item to the set, 0 if the item
      *> existed already, so we didn't add it.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-TO-SET.
       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ITEM-VALUE                 PIC 9(4) USAGE COMP.
       COPY "set" IN "06".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ITEM-VALUE
           BY REFERENCE SET-GRP.
           SET SET-INDEX TO 0
           SEARCH SET-NODES
               VARYING SET-INDEX
               AT END
      *> First time seeing this item, add it.
                   ADD 1 TO SET-SIZE
                   SET SET-NODE-ITEM(SET-SIZE) TO IN-ITEM-VALUE
                   MOVE 1 TO RETURN-CODE

      *> We already have this item.
               WHEN SET-NODE-ITEM(SET-INDEX) = IN-ITEM-VALUE
                   MOVE 0 TO RETURN-CODE
           END-SEARCH
           GOBACK.
       END PROGRAM ADD-TO-SET.
