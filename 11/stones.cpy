
       01  STONE-GRP.
           05  STONES-SIZE          PIC 9(8) VALUE 0.
           05  STONES OCCURS 1 TO 99999999 TIMES
               DEPENDING ON STONES-SIZE
               INDEXED BY STONE-INDEX.
               10 STONE             PIC 9(18).
