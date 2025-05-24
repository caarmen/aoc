       01  OUTPUT-GRP.
           05  OUTPUT-SIZE                PIC 9(9) VALUE 0.
           05  OUTPUT-ITEMS OCCURS 1 TO 999999999 TIMES
               DEPENDING ON OUTPUT-SIZE
               INDEXED BY OUTPUT-INDEX.
               10  OUTPUT-ITEM            PIC 9(1).
