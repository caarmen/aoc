
       01  STACK-GRP.
           05  STACK-SIZE                PIC 9(5) USAGE COMP VALUE 0.
           05  STACK-ITEMS
               OCCURS 1 TO 20000 TIMES
               DEPENDING ON STACK-SIZE.
               10  STACK-ITEM-ROW        PIC 9(3).
               10  STACK-ITEM-COL        PIC 9(3).
               10  STACK-PREV-ITEM-ROW   PIC 9(3) VALUE 0.
               10  STACK-PREV-ITEM-COL   PIC 9(3) VALUE 0.
