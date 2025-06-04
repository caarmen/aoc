      *> ===============================================================
      *> ENQUEUE.
      *>
      *> Return 0 if the item was enqueued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENQUEUE-STEP.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queuestep" IN "21/src".
       01 IN-QUEUE-VALUE-ROW                PIC 9(1).
       01 IN-QUEUE-VALUE-COL                PIC 9(1).
       01 IN-QUEUE-VALUE-MOV                PIC X(1).
       01 IN-QUEUE-VALUE-MOV-HIST           PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           IN-QUEUE-VALUE-ROW
           IN-QUEUE-VALUE-COL
           IN-QUEUE-VALUE-MOV
           IN-QUEUE-VALUE-MOV-HIST.

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
           SET QUEUE-VALUE-MOV(QUEUE-TAIL) TO IN-QUEUE-VALUE-MOV
           SET QUEUE-VALUE-MOV-HIST(QUEUE-TAIL)
               TO IN-QUEUE-VALUE-MOV-HIST

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM ENQUEUE-STEP.

      *> ===============================================================
      *> DEQUEUE.
      *>
      *> Return 0 if the item was dequeued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEQUEUE-STEP.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queuestep" IN "21/src".
       01 OUT-QUEUE-VALUE-ROW                PIC 9(1).
       01 OUT-QUEUE-VALUE-COL                PIC 9(1).
       01 OUT-QUEUE-VALUE-MOV                PIC X(1).
       01 OUT-QUEUE-VALUE-MOV-HIST           PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           OUT-QUEUE-VALUE-ROW
           OUT-QUEUE-VALUE-COL
           OUT-QUEUE-VALUE-MOV
           OUT-QUEUE-VALUE-MOV-HIST.

           IF QUEUE-SIZE = 0
               DISPLAY "Queue empty"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           SET OUT-QUEUE-VALUE-ROW TO QUEUE-VALUE-ROW(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-COL TO QUEUE-VALUE-COL(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-MOV TO QUEUE-VALUE-MOV(QUEUE-HEAD)
           SET OUT-QUEUE-VALUE-MOV-HIST
               TO QUEUE-VALUE-MOV-HIST(QUEUE-HEAD)

           SUBTRACT 1 FROM QUEUE-SIZE
           COMPUTE QUEUE-HEAD = FUNCTION MOD(QUEUE-HEAD + 1,
               QUEUE-MAX-SIZE)

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM DEQUEUE-STEP.
