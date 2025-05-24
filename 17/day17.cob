       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY17.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                   PIC X(30).
       01  LS-INIT-REG-A                  PIC 9(18) COMP.
       01  LS-INIT-REG-B                  PIC 9(16) COMP.
       01  LS-INIT-REG-C                  PIC 9(16) COMP.
       01  LS-ITERATION                   PIC 9(5) VALUE 0.
       01  LS-PROGRAM-RESULT              PIC 9(1).
       01  LS-FACTORS OCCURS 16 TIMES.
           05  LS-FACTOR                  PIC 9(1).
       COPY "prog" IN "17".
       COPY "output" IN "17".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               PROG-GRP

      *> 0
           SET LS-FACTOR(1) TO 0
      *> 3
           SET LS-FACTOR(2) TO 0
      *> 5
           SET LS-FACTOR(3) TO 0
      *> 5
           SET LS-FACTOR(4) TO 0
      *> 5
           SET LS-FACTOR(5) TO 0
      *> 4
           SET LS-FACTOR(6) TO 0
      *> 3
           SET LS-FACTOR(7) TO 0
      *> 0
           SET LS-FACTOR(8) TO 0
      *> 4
           SET LS-FACTOR(9) TO 0
      *> 1
           SET LS-FACTOR(10) TO 0
      *> 5
           SET LS-FACTOR(11) TO 0
      *> 7
           SET LS-FACTOR(12) TO 0
      *> 1
           SET LS-FACTOR(13) TO 0
      *> 1
           SET LS-FACTOR(14) TO 0
      *> 4
           SET LS-FACTOR(15) TO 0
      *> 2
           SET LS-FACTOR(16) TO 5

           PERFORM 20000 TIMES

               DISPLAY SPACE
               DISPLAY "Iteration " LS-ITERATION
               ADD 1 TO LS-ITERATION

               SET LS-INIT-REG-A TO 0
               PERFORM VARYING PROG-INSTR-PTR FROM 1 BY 1 UNTIL
                   PROG-INSTR-PTR > PROG-SIZE
                   COMPUTE LS-INIT-REG-A = LS-INIT-REG-A +
                       LS-FACTOR(PROG-INSTR-PTR) *
                           (8**(PROG-INSTR-PTR - 1))
               END-PERFORM

      *> Display the a register we'll try now

               SET PROG-REG-A TO LS-INIT-REG-A
               DISPLAY "Trying " PROG-REG-A ": (" NO ADVANCING
               PERFORM VARYING PROG-INSTR-PTR FROM PROG-SIZE BY -1
                   UNTIL PROG-INSTR-PTR = 0
                   DISPLAY LS-FACTOR(PROG-INSTR-PTR) NO ADVANCING
               END-PERFORM
               DISPLAY ")"
               SET OUTPUT-SIZE TO 0

               CALL "RUN-PROGRAM" USING
                   BY REFERENCE
                   PROG-GRP
                   OUTPUT-GRP
                   RETURNING LS-PROGRAM-RESULT
      *> Display the output, with x next to the ones that don't match
      *> the program
               DISPLAY "Program:" NO ADVANCING
               PERFORM VARYING OUTPUT-INDEX FROM 1 BY 1
                   UNTIL OUTPUT-INDEX > OUTPUT-SIZE
                   DISPLAY PROG-ITEM(OUTPUT-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
               DISPLAY "Output :" NO ADVANCING
               PERFORM VARYING OUTPUT-INDEX FROM 1 BY 1
                   UNTIL OUTPUT-INDEX > OUTPUT-SIZE
                   DISPLAY OUTPUT-ITEM(OUTPUT-INDEX)
                       NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
      *> Find the first (rightmost in the output) number which
      *> doesn't match the program.
               PERFORM VARYING PROG-INSTR-PTR FROM PROG-SIZE BY -1
                   UNTIL PROG-INSTR-PTR = 0
                   IF OUTPUT-ITEM(PROG-INSTR-PTR) NOT =
                       PROG-ITEM(PROG-INSTR-PTR)
                       IF LS-FACTOR(PROG-INSTR-PTR) = 9
                           SET LS-FACTOR(PROG-INSTR-PTR) TO 0
                           ADD 1 TO PROG-INSTR-PTR
                           ADD 1 TO LS-FACTOR(PROG-INSTR-PTR)
                       ELSE
                           ADD 1 TO LS-FACTOR(PROG-INSTR-PTR) 
                       END-IF
                       EXIT PERFORM
                   END-IF
               END-PERFORM



               IF LS-PROGRAM-RESULT = 0
                   DISPLAY "Self-generating program with "
                   LS-INIT-REG-A
                   DISPLAY "Register A: " PROG-REG-A
                   DISPLAY "Register B: " PROG-REG-B
                   DISPLAY "Register C: " PROG-REG-C
                   EXIT PERFORM
               END-IF

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
