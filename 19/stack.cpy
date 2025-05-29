
       01  STACK-GRP.
           05  STACK-SIZE                      PIC 9(3) USAGE COMP.
           05  STACK-ITEMS
               OCCURS 1 TO 999 TIMES
               DEPENDING ON STACK-SIZE.
               10  STACK-TOWELS-PATTERN        PIC X(100).
