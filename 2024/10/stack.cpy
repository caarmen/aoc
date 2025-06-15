
       01  STACK-GRP.
           05  STACK-SIZE                PIC 9(2) USAGE COMP.
           05  STACK-ITEMS
               OCCURS 1 TO 99 TIMES
               DEPENDING ON STACK-SIZE.
               10  STACK-ITEM-ROW        PIC 9(2).
               10  STACK-ITEM-COL        PIC 9(2).
