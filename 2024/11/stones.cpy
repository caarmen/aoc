
       01  STONE-GRP.
           05  STONES-SIZE          PIC 9(18) VALUE 0.
           05  STONES OCCURS 1 TO 9999999 TIMES
               DEPENDING ON STONES-SIZE
               INDEXED BY STONE-INDEX.
               10 STONE             PIC 9(18).
               10 STONE-COUNT       PIC 9(18) VALUE 1.
               10 STONE-NEW-COUNT   PIC 9(18) VALUE 0.
