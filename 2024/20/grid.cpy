       01  GRID-GRP.
           05  GRID-SIZE                          PIC 9(3) VALUE 0.
           05  GRID-START-ROW                     PIC 9(3).
           05  GRID-START-COL                     PIC 9(3).
           05  GRID-END-ROW                       PIC 9(3).
           05  GRID-END-COL                       PIC 9(3).
           05  GRID-FULL-PATH-LENGTH              PIC 9(5).
           05  GRID-ROWS OCCURS 1 TO 141 TIMES
               DEPENDING ON GRID-SIZE
               INDEXED BY GRID-ROW-INDEX.
               10  GRID-COLS OCCURS 141 TIMES
                   INDEXED BY GRID-COL-INDEX.
               15  GRID-CELL                      PIC X(1).
               15  GRID-PATH-PARENT-ROW           PIC 9(3).
               15  GRID-PATH-PARENT-COL           PIC 9(3).
               15  GRID-DIST-TO-END               PIC 9(5).
