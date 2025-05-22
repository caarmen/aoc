       01  GRID-GRP.
           05  GRID-SIZE                             PIC 9(3) VALUE 0.
           05  START-ROW                             PIC 9(3).
           05  START-COL                             PIC 9(3).
           05  CUR-ROW                               PIC 9(3).
           05  CUR-COL                               PIC 9(3).
           05  END-ROW                               PIC 9(3).
           05  END-COL                               PIC 9(3).
           05  GRID-ROWS OCCURS 1 TO 141 TIMES
               DEPENDING ON GRID-SIZE
               INDEXED BY GRID-ROW-INDEX.
               10 GRID-COLS OCCURS 141 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15 GRID-CELL                      PIC X(1).
      *>             15 DIST-FROM-START                PIC 9(5).
                   15 DIRECTION                  PIC X(1) VALUE SPACE.
