       01  ANTENNAS-GRP.
           05  ANTENNAS-SIZE                  PIC 9(3) COMP VALUE 0.
           05  ANTENNAS OCCURS 1 TO 99 TIMES
               DEPENDING ON ANTENNAS-SIZE
               INDEXED BY ANTENNAS-INDEX.
               10  ANTENNA-SYMBOL             PIC X(1).
               10  ANTENNA-COORDS-SIZE        PIC 9(1) COMP VALUE 0.
               10  ANTENNA-COORDS OCCURS 9 TIMES
                   INDEXED BY ANTENNA-COORDS-INDEX.
                   15  ANTENNA-ROW            PIC 9(2) COMP.
                   15  ANTENNA-COL            PIC 9(2) COMP.
