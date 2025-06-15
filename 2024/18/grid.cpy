       01  GRID-GRP.
           05  GRID-SIZE             PIC 9(2).
           05  GRID-ROW OCCURS 1 TO 71 TIMES
               DEPENDING ON GRID-SIZE
               INDEXED BY GRID-ROW-INDEX.
               10  GRID-COL OCCURS 71 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15  GRID-CELL     PIC X(1).
