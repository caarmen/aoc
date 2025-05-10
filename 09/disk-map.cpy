       01  DISK-MAP-GRP.
           05  DISK-MAP-SIZE                  PIC 9(6) COMP VALUE 0.
           05  DISK-MAP OCCURS 1 TO 180000
               DEPENDING ON DISK-MAP-SIZE
               INDEXED BY DISK-INDEX.
           10  DISK-ENTRY                     PIC S9(6) COMP.
