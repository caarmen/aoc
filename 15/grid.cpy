       01  GRID-GRP.
           05  GRID-SIZE                               PIC 9(2) VALUE 0.
           05  ROBOT-ROW                               PIC 9(2).
           05  ROBOT-COL                               PIC 9(2).
           05  GRID-ROWS OCCURS 1 TO 50 TIMES
               DEPENDING ON GRID-SIZE
               INDEXED BY GRID-ROW-INDEX.
               10 GRID-COLS OCCURS 50 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15 GRID-CELL                        PIC X(1).
