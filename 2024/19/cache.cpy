       01  CACHE-GRP.
           05  CACHE-SIZE                   PIC 9(3) USAGE COMP VALUE 0.
           05  CACHE-CALCS OCCURS 1 TO 999 TIMES
               DEPENDING ON CACHE-SIZE
               ASCENDING KEY IS CACHE-KEY
               INDEXED BY CACHE-INDEX.
               10  CACHE-KEY                PIC X(100).
               10  CACHE-VALUE              PIC 9(16).
