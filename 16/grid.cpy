       01  GRID-GRP.
           05  GRID-SIZE                             PIC 9(3) VALUE 0.
           05  START-ROW                             PIC 9(3).
           05  START-COL                             PIC 9(3).
           05  END-ROW                               PIC 9(3).
           05  END-COL                               PIC 9(3).
           05  GRID-ROWS OCCURS 1 TO 141 TIMES
               DEPENDING ON GRID-SIZE
               INDEXED BY GRID-ROW-INDEX.
               10 GRID-COLS OCCURS 141 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15 GRID-CELL                      PIC X(1).
                   15 DIRECTIONS OCCURS 4 TIMES
                       INDEXED BY DIRECTION-INDEX.
                       20  PARENT-ROW                PIC 9(3) VALUE 0.
                       20  PARENT-COL                PIC 9(3) VALUE 0.
                       20  DIST-THRU-PARENT          PIC 9(6) VALUE
                                                         999999.
