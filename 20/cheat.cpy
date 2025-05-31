       01  CHEAT-GRP.
           05  CHEAT-SIZE                        PIC 9(8) VALUE 0.
           05  CHEATS OCCURS 1 TO 99999999 TIMES
               DEPENDING ON CHEAT-SIZE
               ASCENDING KEY IS CHEAT-ID
               INDEXED BY CHEAT-INDEX.
               10  CHEAT-ID                      PIC 9(12).
               10  CHEAT-DISTANCE-SAVED          PIC 9(5).
