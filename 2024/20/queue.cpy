       01  QUEUE-MAX-SIZE                         CONSTANT 9999.
       01  QUEUE-GRP.
           05  QUEUE-SIZE                         PIC 9(5) VALUE 0.
           05  QUEUE-HEAD                         PIC 9(4) VALUE 1.
           05  QUEUE-TAIL                         PIC 9(4) VALUE 0.
           05  QUEUE-ARR OCCURS 9999 TIMES.
               10  QUEUE-VALUE-ROW                PIC 9(3).
               10  QUEUE-VALUE-COL                PIC 9(3).
               10  QUEUE-VALUE-DIST               PIC 9(5).
