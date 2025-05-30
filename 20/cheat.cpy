       01  CHEAT-GRP.
           05  CHEAT-SIZE                        PIC 9(5) VALUE 0.
           05  CHEATS OCCURS 1 TO 99999 TIMES
               DEPENDING ON CHEAT-SIZE
               INDEXED BY CHEAT-INDEX.
               10  CHEAT-START-ROW               PIC 9(3).
               10  CHEAT-START-COL               PIC 9(3).
               10  CHEAT-END-ROW                 PIC 9(3).
               10  CHEAT-END-COL                 PIC 9(3).
               10  CHEAT-DISTANCE-SAVED          PIC 9(5).
