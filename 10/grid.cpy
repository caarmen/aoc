       01  GRID-GRP.
           05  GRID-SIZE                            PIC 9(2) USAGE COMP
                                                        VALUE 0.
           05  TRAIL-HEADS-SIZE                     PIC 9(3) VALUE 0.
           05  TRAIL-HEADS
                   OCCURS 500 TIMES
                   INDEXED BY TRAIL-HEADS-INDEX.
               10  TRAIL-HEAD-ROW                   PIC 9(2).
               10  TRAIL-HEAD-COL                   PIC 9(2).

           05  GRID-ROW
                   OCCURS 7 TO 47 TIMES
                   DEPENDING ON GRID-SIZE
                   INDEXED BY GRID-ROW-INDEX.
               10  GRID-COL
                   OCCURS 47 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15 GRID-CELL                     PIC X(1).
                   15 GRID-CELL-VISITED             PIC 9(1) VALUE 0.

