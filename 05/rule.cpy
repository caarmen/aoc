               10  RULE-KEY           PIC 9(2) USAGE COMP.
               10  RULE-AFTER-SIZE    PIC 9(2) USAGE COMP VALUE 0.
               10  RULE-AFTER-LIST OCCURS 100 TIMES
                   INDEXED BY RULE-AFTER-INDEX.
                   15 RULE-AFTER-ITEM PIC 9(2) USAGE COMP.
