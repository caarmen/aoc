       01  SEQUENCE-GRP.
           05  MAX-TOTAL-PRICE       PIC 9(6) VALUE 0.
           05  BEST-SEQUENCE-STR     PIC X(8).
           05  SEQUENCES-SIZE        PIC 9(6) VALUE 0.
           05  SEQUENCES OCCURS 1 TO 130000 TIMES
               DEPENDING ON SEQUENCES-SIZE
               ASCENDING KEY IS SEQUENCE-STR
               INDEXED BY SEQUENCE-IDX
               .
               10  SEQUENCE-STR      PIC X(8).
               10  TOTAL-PRICE       PIC 9(6) VALUE 0.
               10  PRICE-SIZE        PIC 9(4) VALUE 0.
               10  PRICES OCCURS 2200 TIMES
                   ASCENDING KEY IS BUYER-IDX
                   INDEXED BY PRICE-IDX.
                   15  BUYER-IDX     PIC 9(4).
                   15  BUYER-PRICE   PIC 9(1).


