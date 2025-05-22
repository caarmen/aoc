       01  UNVISITED-GRP.
           05  UNVISITED-SIZE                PIC 9(5) VALUE 0.
           05  UNVISITED OCCURS 1 TO 20000
               DEPENDING ON UNVISITED-SIZE
               ASCENDING KEY IS UNVISITED-DIST-FROM-START
               INDEXED BY UNVISITED-INDEX.
               10  UNVISITED-ROW             PIC 9(3).
               10  UNVISITED-COL             PIC 9(3).
               10  UNVISITED-DIST-FROM-START PIC 9(5) VALUE 99999.
