       01  KEY-GRP.
           05  KEYS-SIZE                             PIC 9(3) VALUE 0.
           05  KEYS OCCURS 1 TO 999 TIMES
               DEPENDING ON KEYS-SIZE
               INDEXED BY KEY-IDX.
               10  KEY-PEAKS OCCURS 5 TIMES
                   INDEXED BY KEY-PEAK-IDX.
                   15  KEY-PEAK-HEIGHT               PIC 9(1) VALUE 0.
