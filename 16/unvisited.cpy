       01  UNVISITED-GRP.
           05  UNVISITED-SIZE                PIC 9(6) VALUE 0.
           05  UNVISITED OCCURS 1 TO 999999
               DEPENDING ON UNVISITED-SIZE
               ASCENDING KEY IS UNVISITED-DIST-FROM-START
               INDEXED BY UNVISITED-INDEX.
               10  UNVISITED-ROW             PIC 9(3).
               10  UNVISITED-COL             PIC 9(3).
               10  UNVISITED-DIR             PIC 9(1).
               10  UNVISITED-DIST-FROM-START PIC 9(6) VALUE 999999.
