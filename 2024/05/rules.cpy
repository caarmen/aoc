       01  RULES-GRP.
           05  RULES-SIZE                 PIC 9(2) USAGE COMP VALUE 0.
           05  RULES OCCURS 1 TO 100 TIMES
               DEPENDING ON RULES-SIZE
               ASCENDING KEY IS RULE-KEY
               INDEXED BY RULE-INDEX.
               COPY "rule" IN "05".
