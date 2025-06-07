      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "21/src".
       01  OUT-ITEM-LEVEL                   PIC 9(2).
       01  OUT-ITEM-START-KEY               PIC X(1).
       01  OUT-ITEM-END-KEY                 PIC X(1).
       01  OUT-RESULT                       PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           STACK-GRP
           OUT-ITEM-LEVEL
           OUT-ITEM-START-KEY
           OUT-ITEM-END-KEY
           RETURNING OUT-RESULT
           .

           IF STACK-SIZE > 0
               MOVE STACK-ITEM-LEVEL(STACK-SIZE) TO OUT-ITEM-LEVEL
               MOVE STACK-ITEM-START-KEY(STACK-SIZE)
                   TO OUT-ITEM-START-KEY
               MOVE STACK-ITEM-END-KEY(STACK-SIZE) TO OUT-ITEM-END-KEY
               COMPUTE STACK-SIZE = STACK-SIZE - 1
               MOVE 0 TO OUT-RESULT
           ELSE
               MOVE 1 TO OUT-RESULT
           END-IF
           GOBACK.

       END FUNCTION POP-STACK.

      *> ===============================================================
      *> PUSH-TO-STACK.
      *> Add an item to the end of the stack
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. PUSH-TO-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ITEM-LEVEL                   PIC 9(2).
       01  IN-ITEM-START-KEY               PIC X(1).
       01  IN-ITEM-END-KEY                 PIC X(1).
       COPY "stack" IN "21/src".
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           IN-ITEM-LEVEL
           IN-ITEM-START-KEY
           IN-ITEM-END-KEY
           RETURNING OUT-RESULT.

           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-LEVEL(STACK-SIZE) TO IN-ITEM-LEVEL
           SET STACK-ITEM-START-KEY(STACK-SIZE) TO IN-ITEM-START-KEY
           SET STACK-ITEM-END-KEY(STACK-SIZE) TO IN-ITEM-END-KEY

           MOVE 0 TO OUT-RESULT
           GOBACK.
       END FUNCTION PUSH-TO-STACK.

