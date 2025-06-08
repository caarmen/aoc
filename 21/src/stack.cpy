
       01  STACK-GRP.
           05  STACK-SIZE                PIC 9(6) USAGE COMP.
           05  STACK-ITEMS
               OCCURS 1 TO 999999 TIMES
               DEPENDING ON STACK-SIZE.
               10  STACK-ITEM-LEVEL      PIC 9(2).
               10  STACK-ITEM-HISTORY    PIC X(100).
               10  STACK-ITEM-START-KEY  PIC X(1).
               10  STACK-ITEM-END-KEY    PIC X(1).
