      *> ===============================================================
      *> ENQUEUE.
      *>
      *> Return 0 if the item was enqueued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENQUEUE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queue" IN "21".
       01 IN-QUEUE-VALUE-ROW                PIC 9(1).
       01 IN-QUEUE-VALUE-COL                PIC 9(1).
       01 IN-QUEUE-VALUE-ACTION             PIC X(1).
       01 IN-QUEUE-VALUE-SEQUENCE           PIC X(100).
       01 IN-QUEUE-VALUE-ACTION-HIST        PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           IN-QUEUE-VALUE-ROW
           IN-QUEUE-VALUE-COL
           IN-QUEUE-VALUE-ACTION
           IN-QUEUE-VALUE-SEQUENCE
           IN-QUEUE-VALUE-ACTION-HIST.

           IF QUEUE-SIZE = QUEUE-MAX-SIZE
               DISPLAY "Queue full"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           ADD 1 TO QUEUE-SIZE
           COMPUTE QUEUE-TAIL = FUNCTION MOD(QUEUE-TAIL + 1,
               QUEUE-MAX-SIZE)

           SET QUEUE-VALUE-ROW(QUEUE-TAIL) TO IN-QUEUE-VALUE-ROW
           SET QUEUE-VALUE-COL(QUEUE-TAIL) TO IN-QUEUE-VALUE-COL
           SET QUEUE-VALUE-ACTION(QUEUE-TAIL) TO IN-QUEUE-VALUE-ACTION
           SET QUEUE-VALUE-SEQUENCE(QUEUE-TAIL)
               TO IN-QUEUE-VALUE-SEQUENCE
           SET QUEUE-VALUE-ACTION-HIST(QUEUE-TAIL)
               TO IN-QUEUE-VALUE-ACTION-HIST

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM ENQUEUE.

      *> ===============================================================
      *> DEQUEUE.
      *>
      *> Return 0 if the item was dequeued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEQUEUE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queue" IN "21".
       01 OUT-QUEUE-VALUE-ROW                PIC 9(1).
       01 OUT-QUEUE-VALUE-COL                PIC 9(1).
       01 OUT-QUEUE-VALUE-ACTION             PIC X(1).
       01 OUT-QUEUE-VALUE-SEQUENCE           PIC X(100).
       01 OUT-QUEUE-VALUE-ACTION-HIST        PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           OUT-QUEUE-VALUE-ROW
           OUT-QUEUE-VALUE-COL
           OUT-QUEUE-VALUE-ACTION
           OUT-QUEUE-VALUE-SEQUENCE
           OUT-QUEUE-VALUE-ACTION-HIST.

           IF QUEUE-SIZE = 0
               DISPLAY "Queue empty"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           SET OUT-QUEUE-VALUE-ROW TO QUEUE-VALUE-ROW(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-COL TO QUEUE-VALUE-COL(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-ACTION TO QUEUE-VALUE-ACTION(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-SEQUENCE
               TO QUEUE-VALUE-SEQUENCE(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-ACTION-HIST
               TO QUEUE-VALUE-ACTION-HIST(QUEUE-HEAD)

           SUBTRACT 1 FROM QUEUE-SIZE
           COMPUTE QUEUE-HEAD = FUNCTION MOD(QUEUE-HEAD + 1,
               QUEUE-MAX-SIZE)

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM DEQUEUE.
