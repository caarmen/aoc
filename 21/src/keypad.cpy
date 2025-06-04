       01  KP-GRP.
           05  KP OCCURS 3 TIMES.
               10  KP-TYPE                         PIC 9(1).
               10  KP-HEIGHT                       PIC 9(1) VALUE 4.
               10  KP-ROWS OCCURS 4 TIMES.
                   15  KP-COLS OCCURS 3 TIMES.
                       20  KP-KEY                  PIC X(1).
               10  KP-CUR-ROW                      PIC 9(1) VALUE 4.
               10  KP-CUR-COL                      PIC 9(1) VALUE 3.
               10  KP-KEY-SEQUENCE.
                   15  KP-KEY-SEQUENCE-LENGTH      PIC 9(3) VALUE 0.
                   15  KP-KEY-SEQUENCE-KEYS OCCURS 100 TIMES.
                       20  KP-KEY-SEQUENCE-KEY     PIC X(1).
