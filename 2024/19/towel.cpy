       01  TOWELS-GRP.
           05  TOWELS-SIZE           PIC 9(3) VALUE 0.
           05  TOWELS OCCURS 1 TO 500 TIMES
               DEPENDING ON TOWELS-SIZE
               ASCENDING KEY IS TOWEL
               INDEXED BY TOWEL-INDEX.
           10  TOWEL                 PIC X(10).
