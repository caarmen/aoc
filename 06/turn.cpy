       01  TURN-GRP.
           05  TURN-SIZE              PIC 9(5) USAGE COMP VALUE 0.
           05  TURN-NODES OCCURS 1 TO 16900 TIMES
               DEPENDING ON TURN-SIZE
               INDEXED BY TURN-INDEX.
               10  TURN-NODE-ROW      PIC 9(3) USAGE COMP.
               10  TURN-NODE-COL      PIC 9(3) USAGE COMP.
               10  TURN-NODE-DIR      PIC 9(1) USAGE COMP.
