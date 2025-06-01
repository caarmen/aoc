       01  KP-GRP.
           05  KP-TYPE                         PIC 9(1).
           05  KP-HEIGHT                       PIC 9(1) VALUE 4.
           05  KP-ROWS OCCURS 4 TIMES
                   INDEXED BY KP-ROW-INDEX.
               10  KP-COLS OCCURS 3 TIMES
                   INDEXED BY KP-COL-INDEX.
                   15  KP-KEY                  PIC X(1).
           05  KP-CUR-ROW                      PIC 9(1) VALUE 4.
           05  KP-CUR-COL                      PIC 9(1) VALUE 3.
           05  KP-KEY-SEQUENCE.
               10  KP-KEY-SEQUENCE-LENGTH      PIC 9(3) VALUE 0.
               10  KP-KEY-SEQUENCE-KEYS OCCURS 1 TO 200 TIMES
                   DEPENDING ON KP-KEY-SEQUENCE-LENGTH
                   INDEXED BY KP-KEY-SEQUENCE-IDX.
                   15  KP-KEY-SEQUENCE-KEY     PIC X(1).
