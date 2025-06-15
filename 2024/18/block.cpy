       01  BLOCKS-GRP.
           05  BLOCKS-SIZE            PIC 9(4).
           05  BLOCKS OCCURS 1 TO 9999 TIMES
               DEPENDING ON BLOCKS-SIZE
               INDEXED BY BLOCK-INDEX.
               10  BLOCK-ROW          PIC 9(2).
               10  BLOCK-COL          PIC 9(2).
