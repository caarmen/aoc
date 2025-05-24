       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY17.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       COPY "prog" IN "17".
       COPY "output" IN "17".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               PROG-GRP

           CALL "RUN-PROGRAM" USING
               BY REFERENCE
               PROG-GRP
               OUTPUT-GRP

           DISPLAY "Register A: " PROG-REG-A
           DISPLAY "Register B: " PROG-REG-B
           DISPLAY "Register C: " PROG-REG-C
           DISPLAY OUTPUT-SIZE " output items"
           PERFORM VARYING OUTPUT-INDEX FROM 1 BY 1
               UNTIL OUTPUT-INDEX > OUTPUT-SIZE
               DISPLAY OUTPUT-ITEM(OUTPUT-INDEX) "," NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
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
               IF FUNCTION MOD(OUTPUT-SIZE, 10) = 0
                   display OUTPUT-SIZE
               END-IF
                
           END-PERFORM
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
