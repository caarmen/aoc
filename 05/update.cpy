       01  UPDATES-GRP.
           05  UPDATE-SIZE            PIC 9(2) USAGE COMP VALUE 0.
           05  UPDATES OCCURS 1 TO 100 TIMES
               DEPENDING ON UPDATE-SIZE
               INDEXED BY UPDATE-INDEX.
               10  UPDATE-ITEM        PIC 9(2) USAGE COMP.
