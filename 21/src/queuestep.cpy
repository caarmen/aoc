       01  QUEUE-MAX-SIZE                         CONSTANT 9999999.
       01  QUEUE-GRP.
           05  QUEUE-SIZE                         PIC 9(7) VALUE 0.
           05  QUEUE-HEAD                         PIC 9(7) VALUE 1.
           05  QUEUE-TAIL                         PIC 9(7) VALUE 0.
           05  QUEUE-ARR OCCURS 9999999 TIMES.
               10  QUEUE-VALUE-ROW                PIC 9(1).
               10  QUEUE-VALUE-COL                PIC 9(1).
               10  QUEUE-VALUE-MOV-HIST           PIC X(100).
