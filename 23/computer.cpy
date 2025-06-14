
       01  COMPUTER-GRP.
           05  COMPUTERS-SIZE                          PIC 9(3) VALUE 0.
           05  COMPUTERS OCCURS 1 TO 999 TIMES
               DEPENDING ON COMPUTERS-SIZE
               ASCENDING KEY IS COMPUTER-NAME
               INDEXED BY COMPUTER-IDX.
               10  COMPUTER-NAME                       PIC X(2).
               10  COMPUtER-LINKS-SIZE                 PIC 9(3) VALUE 0.
               10  COMPUTER-LINKS OCCURS 999 TIMES
                   INDEXED BY COMPUTER-LINKS-IDX.
                   15 COMPUTER-LINK-NAME               PIC X(2).

