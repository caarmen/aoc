       01  SET-GRP.
           05  SET-SIZE              PIC 9(5) USAGE COMP VALUE 0.
           05  SET-NODES OCCURS 1 TO 16900 TIMES
               DEPENDING ON SET-SIZE
               INDEXED BY SET-INDEX.
               10  SET-NODE-ITEM     PIC 9(7) USAGE COMP.
