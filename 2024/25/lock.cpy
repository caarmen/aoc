       01  LOCK-GRP.
           05  LOCKS-SIZE                             PIC 9(3) VALUE 0.
           05  LOCKS OCCURS 1 TO 999 TIMES
               DEPENDING ON LOCKS-SIZE
               INDEXED BY LOCK-IDX.
               10  LOCK-PINS OCCURS 5 TIMES
                   INDEXED BY LOCK-PIN-IDX.
                   15  LOCK-PIN-HEIGHT                PIC 9(1) VALUE 0.
