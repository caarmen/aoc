       01  GRID-GRP.
           05  GRID-WIDTH                              PIC 9(3) VALUE 0.
           05  GRID-HEIGHT                             PIC 9(3) VALUE 0.
           05  ROBOT-ROW                               PIC 9(3).
           05  ROBOT-COL                               PIC 9(3).
           05  GRID-ROWS OCCURS 1 TO 100 TIMES
               DEPENDING ON GRID-HEIGHT
               INDEXED BY GRID-ROW-INDEX.
               10 GRID-COLS OCCURS 100 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15 GRID-CELL                        PIC X(1).
