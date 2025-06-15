       01  GRID-GRP.
           05  GUARD-ROW                            PIC 9(3) USAGE COMP
                                                        VALUE 0.
           05  GUARD-COL                            PIC 9(3) USAGE COMP
                                                        VALUE 0.
           05  GRID-SIZE                            PIC 9(3) USAGE COMP
                                                        VALUE 0.
           05  GRID-ROW
                   OCCURS 10 TO 130 TIMES
                   DEPENDING ON GRID-SIZE
                   INDEXED BY GRID-ROW-INDEX.
               10  GRID-COL
                   OCCURS 130 TIMES
                   INDEXED BY GRID-COL-INDEX.
                   15 GRID-CELL                     PIC X(1).
