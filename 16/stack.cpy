
       01  STACK-GRP.
           05  STACK-SIZE                PIC 9(6) USAGE COMP.
           05  STACK-ITEMS
               OCCURS 1 TO 999999 TIMES
               DEPENDING ON STACK-SIZE.
               10  STACK-ITEM-ROW        PIC 9(3).
               10  STACK-ITEM-COL        PIC 9(3).
               10  STACK-ITEM-DIR        PIC 9(1).
               10  STACK-NEXT-ROW        PIC 9(3).
               10  STACK-NEXT-COL        PIC 9(3).
