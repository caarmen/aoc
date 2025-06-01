       01  NK-GRP.
           05  NK-ROWS OCCURS 4 TIMES
                   INDEXED BY NK-ROW-INDEX.
               10  NK-COLS OCCURS 3 TIMES
                   INDEXED BY NK-COL-INDEX.
                   15  NK-KEY                  PIC X(1).
           05  NK-CUR-ROW                      PIC 9(1) VALUE 4.
           05  NK-CUR-COL                      PIC 9(1) VALUE 3.
           05  NK-KEY-SEQUENCE.
               10  NK-KEY-SEQUENCE-LENGTH      PIC 9(3) VALUE 0.
               10  NK-KEY-SEQUENCE-KEYS OCCURS 1 TO 200 TIMES
                   DEPENDING ON NK-KEY-SEQUENCE-LENGTH
                   INDEXED BY NK-KEY-SEQUENCE-IDX.
                   15  NK-KEY-SEQUENCE-KEY     PIC X(1).
