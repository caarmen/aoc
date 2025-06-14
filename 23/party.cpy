
       01  PARTIES-GRP.
           05  PARTIES-SIZE                        PIC 9(5) VALUE 0.
           05  PARTIES OCCURS 1 TO 99999 TIMES
               DEPENDING ON PARTIES-SIZE
               ASCENDING KEY IS PARTY
               INDEXED BY PARTIES-IDX.
               10 PARTY                            PIC X(6).


