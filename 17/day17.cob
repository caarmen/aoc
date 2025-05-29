       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY17.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                   PIC X(30).
       01  LS-INIT-REG-A                  PIC 9(18) COMP VALUE 0.
       01  LS-INIT-REG-B                  PIC 9(16) COMP.
       01  LS-INIT-REG-C                  PIC 9(16) COMP.
       01  LS-ITERATION                   PIC 9(5) VALUE 0.
       01  LS-PROGRAM-RESULT              PIC 9(1).
       01  LS-OCTAL-STRING                PIC X(50).
       COPY "prog" IN "17".
       COPY "output" IN "17".
       COPY "queue" IN "17".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               PROG-GRP
           SET LS-OCTAL-STRING TO SPACE
           CALL "ENQUEUE" USING
               QUEUE-GRP
               LS-OCTAL-STRING

      *> Inspiration: https://www.youtube.com/watch?v=QpvAyg1RIYI

           PERFORM UNTIL QUEUE-SIZE = 0
               CALL "DEQUEUE" USING
                   QUEUE-GRP
                   LS-OCTAL-STRING
               display "dequeued " ls-octal-string

               STRING FUNCTION TRIM(LS-OCTAL-STRING) "0"
                   INTO LS-OCTAL-STRING
               END-STRING
               PERFORM 8 TIMES

                   CALL "FROM-OCTAL-STRING" USING
                       LS-OCTAL-STRING
                       LS-INIT-REG-A

      *> Display the a register we'll try now

                   SET PROG-REG-A TO LS-INIT-REG-A
                   CALL "TO-OCTAL-STRING" USING
                       LS-INIT-REG-A
                       LS-OCTAL-STRING
                   DISPLAY "[" FUNCTION TRIM(LS-OCTAL-STRING)
                       "|" LS-INIT-REG-A "]" NO ADVANCING 
                   SET OUTPUT-SIZE TO 0

                   CALL "RUN-PROGRAM" USING
                       BY REFERENCE
                       PROG-GRP
                       OUTPUT-GRP
                       RETURNING LS-PROGRAM-RESULT
      *> Display the program
                   DISPLAY "[" NO ADVANCING
                   PERFORM VARYING PROG-INSTR-PTR FROM 1 BY 1
                       UNTIL PROG-INSTR-PTR > PROG-SIZE
                       DISPLAY PROG-ITEM(PROG-INSTR-PTR)
                           NO ADVANCING
                   END-PERFORM
      *> Display the output
                   DISPLAY "][" NO ADVANCING
                   PERFORM VARYING OUTPUT-INDEX FROM 1 BY 1
                       UNTIL OUTPUT-INDEX > OUTPUT-SIZE
                       DISPLAY OUTPUT-ITEM(OUTPUT-INDEX)
                           NO ADVANCING
                   END-PERFORM
                   DISPLAY "]"

                   IF LS-PROGRAM-RESULT = 0
                       DISPLAY "Self-generating program with "
                       LS-INIT-REG-A "(" FUNCTION TRIM(LS-OCTAL-STRING)
                           ")"
                       DISPLAY "Register A: " PROG-REG-A
                       DISPLAY "Register B: " PROG-REG-B
                       DISPLAY "Register C: " PROG-REG-C
                   END-IF
                   IF OUTPUT-ITEM(1) =
                       PROG-ITEMS(PROG-SIZE - OUTPUT-SIZE + 1)
                       CALL "ENQUEUE" USING 
                           QUEUE-GRP
                           LS-OCTAL-STRING
                   END-IF
                   ADD 1 TO LS-INIT-REG-A
                   CALL "TO-OCTAL-STRING" USING
                       LS-INIT-REG-A
                       LS-OCTAL-STRING

               END-PERFORM
           END-PERFORM
           .
       END PROGRAM DAY17.

      *> ===============================================================
      *> PARSE-FILE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO IN-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD             PIC X(50).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(50).
       01  LS-TOKEN-LEFT             PIC X(50).
       01  LS-TOKEN-RIGHT            PIC X(50).
       01  LS-LINE-PTR               PIC 9(2).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "prog" IN "17".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           PROG-GRP.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       EVALUATE TRUE
                           WHEN LS-LINE(1:10) = "Register A"
                               UNSTRING LS-LINE
                                   DELIMITED BY ": "
                                   INTO LS-TOKEN-LEFT LS-TOKEN-RIGHT
                               END-UNSTRING
                               SET PROG-REG-A TO LS-TOKEN-RIGHT
                           WHEN LS-LINE(1:10) = "Register B"
                               UNSTRING LS-LINE
                                   DELIMITED BY ": "
                                   INTO LS-TOKEN-LEFT LS-TOKEN-RIGHT
                               END-UNSTRING
                               SET PROG-REG-B TO LS-TOKEN-RIGHT
                           WHEN LS-LINE(1:10) = "Register C"
                               UNSTRING LS-LINE
                                   DELIMITED BY ": "
                                   INTO LS-TOKEN-LEFT LS-TOKEN-RIGHT
                               END-UNSTRING
                               SET PROG-REG-C TO LS-TOKEN-RIGHT
                           WHEN LS-LINE(1:7) = "Program"
                               UNSTRING LS-LINE
                                   DELIMITED BY ": "
                                   INTO LS-TOKEN-LEFT LS-TOKEN-RIGHT
                               END-UNSTRING
                               SET LS-LINE-PTR TO 1
                               PERFORM UNTIL LS-LINE-PTR > 
                                   LENGTH OF FUNCTION
                                       TRIM(LS-TOKEN-RIGHT)
                                   ADD 2 TO PROG-SIZE
                                   
                                   UNSTRING LS-TOKEN-RIGHT
                                       DELIMITED BY ","
                                       INTO
                                           PROG-ITEM(PROG-SIZE - 1)
                                           PROG-ITEM(PROG-SIZE)
                                       WITH POINTER LS-LINE-PTR
                                   END-UNSTRING
                               END-PERFORM
                       END-EVALUATE
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> RUN-PROGRAM.
      *> Return 0 if the output is identical to the input.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUN-PROGRAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION GET-COMBO-OPERAND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-ADV                          CONSTANT 0.
       01  C-BXL                          CONSTANT 1.
       01  C-BST                          CONSTANT 2.
       01  C-JNZ                          CONSTANT 3.
       01  C-BXC                          CONSTANT 4.
       01  C-OUT                          CONSTANT 5.
       01  C-BDV                          CONSTANT 6.
       01  C-CDV                          CONSTANT 7.
       LOCAL-STORAGE SECTION.
       01  LS-TEMP-RESULT                 PIC 9(16) COMP.
       01  LS-OPCODE                      PIC 9(1).
       01  LS-OPERAND                     PIC 9(1) COMP.
       01  LS-COMBO-OPERAND               PIC 9(16) COMP.
       LINKAGE SECTION.
       COPY "prog" IN "17".
       COPY "output" IN "17".

           PROCEDURE DIVISION USING BY REFERENCE
               PROG-GRP
               OUTPUT-GRP.
           SET PROG-INSTR-PTR TO 1
           PERFORM UNTIL PROG-INSTR-PTR > PROG-SIZE
               SET LS-OPCODE TO PROG-ITEM(PROG-INSTR-PTR)
               SET LS-OPERAND TO PROG-ITEM(PROG-INSTR-PTR + 1)
               SET LS-COMBO-OPERAND TO GET-COMBO-OPERAND(
                   PROG-GRP,
                   LS-OPERAND)

               EVALUATE LS-OPCODE
                   WHEN C-ADV
                       COMPUTE LS-TEMP-RESULT = PROG-REG-A / 
                           (2**LS-COMBO-OPERAND)
                       SET PROG-REG-A TO LS-TEMP-RESULT
                   WHEN C-BXL
                       CALL "xor" USING
                           BY VALUE PROG-REG-B
                           BY VALUE LS-OPERAND
                           RETURNING LS-TEMP-RESULT
                       SET PROG-REG-B TO LS-TEMP-RESULT
                   WHEN C-BST
                       COMPUTE LS-TEMP-RESULT = FUNCTION MOD(
                           LS-COMBO-OPERAND, 8
                       )
                       SET PROG-REG-B TO LS-TEMP-RESULT
                   WHEN C-JNZ
                       IF PROG-REG-A NOT = 0
                           COMPUTE PROG-INSTR-PTR = LS-OPERAND + 1
                       END-IF
                   WHEN C-BXC
                       CALL "xor" USING
                           BY VALUE PROG-REG-B
                           BY VALUE PROG-REG-C
                           RETURNING LS-TEMP-RESULT
                       SET PROG-REG-B TO LS-TEMP-RESULT
                   WHEN C-OUT
                       COMPUTE LS-TEMP-RESULT = FUNCTION MOD(
                           LS-COMBO-OPERAND, 8
                       )
                       ADD 1 TO OUTPUT-SIZE
                       SET OUTPUT-ITEM(OUTPUT-SIZE) TO LS-TEMP-RESULT
                   WHEN C-BDV
                       COMPUTE LS-TEMP-RESULT = PROG-REG-A / 
                           (2**LS-COMBO-OPERAND)
                       SET PROG-REG-B TO LS-TEMP-RESULT
                   WHEN C-CDV
                       COMPUTE LS-TEMP-RESULT = PROG-REG-A / 
                           (2**LS-COMBO-OPERAND)
                       SET PROG-REG-C TO LS-TEMP-RESULT

               END-EVALUATE
               IF NOT (LS-OPCODE = C-JNZ AND PROG-REG-A NOT = 0)
                   ADD 2 TO PROG-INSTR-PTR
               END-IF
                
           END-PERFORM

      *> Check if the output is identical to the input.
           IF OUTPUT-SIZE NOT = PROG-SIZE
               SET RETURN-CODE TO 1
               GOBACK
           END-IF
           PERFORM VARYING OUTPUT-INDEX FROM 1 BY 1
               UNTIL OUTPUT-INDEX > OUTPUT-SIZE
               IF OUTPUT-ITEM(OUTPUT-INDEX) NOT =
                   PROG-ITEM(OUTPUT-INDEX)
                   SET RETURN-CODE TO 1
                   GOBACK
               END-IF
           END-PERFORM
           SET RETURN-CODE TO 0
           GOBACK.
       END PROGRAM RUN-PROGRAM.

      *> ===============================================================
      *> GET-COMBO-OPERAND
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. GET-COMBO-OPERAND.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "prog" IN "17".
       01  IN-OPERAND                      PIC 9(1).
       01  OUT-RESULT                      PIC 9(16) COMP.

       PROCEDURE DIVISION USING BY REFERENCE
           PROG-GRP
           IN-OPERAND
           RETURNING OUT-RESULT.

           MOVE 0 TO OUT-RESULT
           
           EVALUATE IN-OPERAND
               WHEN GREATER OR EQUAL 0 AND LESS OR EQUAL 3
                   MOVE IN-OPERAND TO OUT-RESULT
               WHEN 4
                   MOVE PROG-REG-A TO OUT-RESULT
               WHEN 5
                   MOVE PROG-REG-B TO OUT-RESULT
               WHEN 6
                   MOVE PROG-REG-C TO OUT-RESULT
               WHEN OTHER
                   DISPLAY "Unexpected operand " IN-OPERAND
           END-EVALUATE

           GOBACK.
       END FUNCTION GET-COMBO-OPERAND.


      *> ===============================================================
      *> TO-OCTAL-STRING
      *> ===============================================================
     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TO-OCTAL-STRING.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-NUMBER                            PIC 9(18) COMP.
       01  LS-QUOTIENT                          PIC 9(18) COMP.
       01  LS-REMAINDER                         PIC 9(1).
       01  LS-TEMP-STRING                       PIC X(50).
       LINKAGE SECTION.
       01  IN-NUMBER                            PIC 9(18) COMP.
       01  OUT-OCTAL-STRING                     PIC x(50).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-NUMBER
           OUT-OCTAL-STRING.

           SET OUT-OCTAL-STRING TO SPACE

           SET LS-NUMBER TO IN-NUMBER
           PERFORM UNTIL LS-NUMBER = 0
               DIVIDE 8 INTO LS-NUMBER
                   GIVING LS-QUOTIENT REMAINDER LS-REMAINDER

               STRING
                   LS-REMAINDER
                   FUNCTION TRIM(OUT-OCTAL-STRING)
                   INTO LS-TEMP-STRING
               END-STRING
               MOVE LS-TEMP-STRING TO OUT-OCTAL-STRING

               COMPUTE LS-NUMBER = LS-QUOTIENT

           END-PERFORM
           GOBACK.
       END PROGRAM TO-OCTAL-STRING.

      *> ===============================================================
      *> FROM-OCTAL-STRING
      *> ===============================================================
     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FROM-OCTAL-STRING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-END-PTR                            CONSTANT 0.
       01  WS-BASE                               CONSTANT 8.
       LOCAL-STORAGE SECTION.
       01  LS-INDEX                              PIC 9(18).
       01  LS-LENGTH                             PIC 9(2).
       01  LS-DIGIT                              PIC 9(1).
       01  LS-POWER                              PIC 9(2).
       LINKAGE SECTION.
       01  IN-OCTAL-STRING                       PIC X(50).
       01  OUT-NUMBER                            PIC 9(18) COMP.

       PROCEDURE DIVISION USING BY REFERENCE
           IN-OCTAL-STRING
           OUT-NUMBER.

           SET OUT-NUMBER TO 0
           SET LS-POWER TO 0
           SET LS-LENGTH TO LENGTH OF FUNCTION TRIM(IN-OCTAL-STRING)
           PERFORM VARYING LS-INDEX FROM LS-LENGTH BY -1
               UNTIL LS-INDEX = 0
               SET LS-DIGIT TO IN-OCTAL-STRING(LS-INDEX:1)
               COMPUTE OUT-NUMBER = OUT-NUMBER + LS-DIGIT *(8**LS-POWER)
               ADD 1 TO LS-POWER
           END-PERFORM
           GOBACK.
       END PROGRAM FROM-OCTAL-STRING.

      *> ===============================================================
      *> ENQUEUE.
      *>
      *> Return 0 if the item was enqueued, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENQUEUE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "queue" IN "17".
       01 IN-QUEUE-VALUE                     PIC X(50).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           IN-QUEUE-VALUE
           .

           IF QUEUE-SIZE = QUEUE-MAX-SIZE
               DISPLAY "Queue full"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           ADD 1 TO QUEUE-SIZE
           COMPUTE QUEUE-TAIL = FUNCTION MOD(QUEUE-TAIL + 1,
               QUEUE-MAX-SIZE)

           SET QUEUE-VALUE(QUEUE-TAIL) TO IN-QUEUE-VALUE

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
       COPY "queue" IN "17".
       01 OUT-QUEUE-VALUE                      PIC X(50).

       PROCEDURE DIVISION USING BY REFERENCE
           QUEUE-GRP
           OUT-QUEUE-VALUE.

           IF QUEUE-SIZE = 0
               DISPLAY "Queue empty"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           SET OUT-QUEUE-VALUE TO QUEUE-VALUE(QUEUE-HEAD)

           SUBTRACT 1 FROM QUEUE-SIZE
           COMPUTE QUEUE-HEAD = FUNCTION MOD(QUEUE-HEAD + 1,
               QUEUE-MAX-SIZE)

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM DEQUEUE.
