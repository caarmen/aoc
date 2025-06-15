
       01  NUMS-GRP.
           05  CALC-VALUE                      PIC 9(18) COMP-3.
           05  NUMS-SIZE                       PIC 9(2) USAGE COMP.
           05  NUMS
               OCCURS 1 TO 30
               DEPENDING ON NUMS-SIZE
               INDEXED BY NUMS-INDEX.
               10  NUM                         PIC 9(4) USAGE COMP.
