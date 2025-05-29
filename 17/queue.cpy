       01  QUEUE-MAX-SIZE                         CONSTANT 999.
       01  QUEUE-GRP.
           05  QUEUE-SIZE                         PIC 9(5) VALUE 0.
           05  QUEUE-HEAD                         PIC 9(4) VALUE 1.
           05  QUEUE-TAIL                         PIC 9(4) VALUE 0.
           05  QUEUE-ARR OCCURS 999 TIMES.
               10  QUEUE-VALUE                    PIC X(50).
