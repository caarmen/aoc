       01  PLOT-GRP.
           05  PLOT-SIZE                       PIC 9(3) COMP VALUE 0.
           05  PLOT-ROW OCCURS 1 TO 140 TIMES
               DEPENDING ON PLOT-SIZE
               INDEXED BY PLOT-ROW-INDEX.
               10  PLOT-COL OCCURS 140 TIMES
                   INDEXED BY PLOT-COL-INDEX.
                   15 PLOT-CELL                PIC X(1).
                   15 VISITED                  PIC 9(1) VALUE 0.
       01  REGION-GRP.
           05  REGION-COUNT                    PIC 9(3) COMP VALUE 0.
           05  REGIONS OCCURS 1 TO 26 TIMES  
               DEPENDING ON REGION-COUNT
                   ASCENDING KEY IS REGION
                   INDEXED BY REGION-INDEX.
               10  REGION                      PIC X(1). 
