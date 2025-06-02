       01  VISITED-GRP.
           05  VISITED-SIZE                      PIC 9(7) VALUE 0.
           05  VISITED OCCURS 1 TO 9999999 TIMES
               DEPENDING ON VISITED-SIZE
               INDEXED BY VISITED-INDEX.
               10  VISITED-ROW                   PIC 9(1).
               10  VISITED-COL                   PIC 9(1).
               10  VISITED-KEYPRESS-HIST         PIC X(100).
               10  VISITED-MOV-HIST              PIC X(100).
