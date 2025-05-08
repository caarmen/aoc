
       01  STACK-GRP.
           05  STACK-SIZE                       PIC 9(2) USAGE COMP.
           05  STACK-ITEMS
               OCCURS 1 TO 30
               DEPENDING ON STACK-SIZE
               INDEXED BY STACK-INDEX.
               COPY "stack-item" IN "07".
