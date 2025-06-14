       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY24.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-TOTAL-OUTPUT           PIC 9(18) COMP VALUE 0.
       COPY "wire" IN "24".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               WIRE-GRP
               
           CALL "DISPLAY-WIRES" USING
               WIRE-GRP
           CALL "EVALUATE-ALL" USING
               WIRE-GRP
           CALL "DISPLAY-WIRES" USING
               WIRE-GRP
           CALL "CALCULATE-TOTAL-OUTPUT" USING
               WIRE-GRP
               LS-TOTAL-OUTPUT

           DISPLAY "Result: " LS-TOTAL-OUTPUT

               .
       END PROGRAM DAY24.

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
       01  F-FILE-RECORD             PIC X(47).

       WORKING-STORAGE SECTION.
       COPY "constants" IN "24".

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(47).
       01  LS-WIRE-NAME-1            PIC X(3).
       01  LS-WIRE-NAME-2            PIC X(3).
       01  LS-WIRE-NAME-3            PIC X(3).
       01  LS-GATE-STR               PIC X(3).
       01  LS-ARROW                  PIC X(3).
       01  LS-STR-PTR                PIC 9(2).


       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "wire" IN "24".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           WIRE-GRP.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       EVALUATE TRUE
      *> First part of the file input: 
                           WHEN LS-LINE(4:1) = ":"
                               ADD 1 TO WIRE-SIZE
                               SET WIRE-NAME(WIRE-SIZE) TO LS-LINE(1:3)
                               SET WIRE-GATE(WIRE-SIZE) TO C-INIT
                               SET WIRE-OUTPUT(WIRE-SIZE) TO
                                   FUNCTION NUMVAL(LS-LINE(6:1))
                           WHEN LS-LINE = SPACES
                               CONTINUE
      *> Second part of the file input: 
                           WHEN OTHER
                               UNSTRING LS-LINE DELIMITED BY " "
                                   INTO
                                   LS-WIRE-NAME-1
                                   LS-GATE-STR
                                   LS-WIRE-NAME-2
                                   LS-ARROW
                                   LS-WIRE-NAME-3
                               END-UNSTRING
                               ADD 1 TO WIRE-SIZE
                               SET WIRE-NAME(WIRE-SIZE) TO
                                   LS-WIRE-NAME-3
                               SET WIRE-INPUT-1(WIRE-SIZE) TO
                                   LS-WIRE-NAME-1
                               SET WIRE-INPUT-2(WIRE-SIZE) TO
                                   LS-WIRE-NAME-2
                               SET WIRE-OUTPUT(WIRE-SIZE) TO
                                   C-OUTPUT-UNKNOWN
                               EVALUATE LS-GATE-STR
                                   WHEN "AND"
                                       SET WIRE-GATE(WIRE-SIZE) TO C-AND
                                   WHEN "OR"
                                       SET WIRE-GATE(WIRE-SIZE) TO C-OR
                                   WHEN "XOR"
                                       SET WIRE-GATE(WIRE-SIZE) TO C-XOR
                               END-EVALUATE
                       END-EVALUATE
           END-PERFORM
           SORT WIRES
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> EVALUATE-ALL.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALUATE-ALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "24".
       01  C-BYTE-LENGTH                               CONSTANT 2.
       LOCAL-STORAGE SECTION.
       01  LS-WIRE-IDX                                 PIC 9(3).
       01  LS-VALUE-INPUT-1                            PIC 9(1) COMP.
       01  LS-VALUE-INPUT-2                            PIC 9(1) COMP.
       01  LS-TEMP-OUTPUT                              PIC 9(1) COMP.
      *> Set LS-COMPLETE TO 0 when do a full pass on the table
      *> without needing to do any more calculations.
       01  LS-COMPLETE                                 PIC 9(1) VALUE 1.
       LINKAGE SECTION.
       COPY "wire" IN "24".
       PROCEDURE DIVISION USING BY REFERENCE
           WIRE-GRP.

           PERFORM UNTIL LS-COMPLETE = 0
               SET LS-COMPLETE TO 0
               PERFORM VARYING LS-WIRE-IDX FROM 1 BY 1
                   UNTIL LS-WIRE-IDX > WIRE-SIZE
      *> We found a wire without an output calculated.
                   IF WIRE-OUTPUT(LS-WIRE-IDX) = C-OUTPUT-UNKNOWN
      *> We'll need another pass.
                       SET LS-COMPLETE TO 1
                       SET WIRE-IDX TO 1
      *> Look for the values of the two inputs.
                       SEARCH ALL WIRES
                           WHEN WIRE-NAME(WIRE-IDX) =
                               WIRE-INPUT-1(LS-WIRE-IDX)
                               SET LS-VALUE-INPUT-1
                                   TO WIRE-OUTPUT(WIRE-IDX)
                       END-SEARCH
                       SET WIRE-IDX TO 1
                       SEARCH ALL WIRES
                           WHEN WIRE-NAME(WIRE-IDX) =
                               WIRE-INPUT-2(LS-WIRE-IDX)
                               SET LS-VALUE-INPUT-2
                                   TO WIRE-OUTPUT(WIRE-IDX)
                       END-SEARCH
      *> If the values of both inputs are known, we can calculate
      *> the output of our wire.
                       IF LS-VALUE-INPUT-1 NOT = C-OUTPUT-UNKNOWN
                           AND LS-VALUE-INPUT-2 NOT = C-OUTPUT-UNKNOWN
                           SET LS-TEMP-OUTPUT TO LS-VALUE-INPUT-2
                           EVALUATE WIRE-GATE(LS-WIRE-IDX)
                               WHEN C-AND
                                   CALL "CBL_AND" USING
                                       LS-VALUE-INPUT-1
                                       LS-TEMP-OUTPUT
                                       BY VALUE C-BYTE-LENGTH
                               WHEN C-OR
                                   CALL "CBL_OR" USING
                                       LS-VALUE-INPUT-1
                                       LS-TEMP-OUTPUT
                                       BY VALUE C-BYTE-LENGTH
                               WHEN C-XOR
                                   CALL "CBL_XOR" USING
                                       LS-VALUE-INPUT-1
                                       LS-TEMP-OUTPUT
                                       BY VALUE C-BYTE-LENGTH
                           END-EVALUATE
                           SET WIRE-OUTPUT(LS-WIRE-IDX)
                               TO LS-TEMP-OUTPUT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       END PROGRAM EVALUATE-ALL.

      *> ===============================================================
      *> CALCULATE-TOTAL-OUTPUT.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-TOTAL-OUTPUT.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-POSITION                           PIC 9(2).
     
       LINKAGE SECTION.
       COPY "wire" IN "24".
       01  OUT-RESULT                            PIC 9(18) COMP VALUE 0.
       PROCEDURE DIVISION USING BY REFERENCE
           WIRE-GRP
           OUT-RESULT.

           PERFORM VARYING WIRE-IDX FROM WIRE-SIZE BY -1
               UNTIL WIRE-IDX = 0
               IF WIRE-NAME(WIRE-IDX)(1:1) NOT = "z"
                   EXIT PERFORM
               END-IF
               IF WIRE-OUTPUT(WIRE-IDX) = 1
                   SET LS-POSITION TO WIRE-NAME(WIRE-IDX)(2:2)

                   COMPUTE OUT-RESULT = OUT-RESULT + 2**LS-POSITION
               END-IF
               
           END-PERFORM
           .
       END PROGRAM CALCULATE-TOTAL-OUTPUT.

      *> ===============================================================
      *> DISPLAY-WIRES.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-WIRES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "24".
       LOCAL-STORAGE SECTION.
       01  LS-GATE-STR                                    PIC X(3).
       LINKAGE SECTION.
       COPY "wire" IN "24".
       PROCEDURE DIVISION USING BY REFERENCE
           WIRE-GRP.

           PERFORM VARYING WIRE-IDX FROM 1 BY 1
               UNTIL WIRE-IDX > WIRE-SIZE
               EVALUATE WIRE-GATE(WIRE-IDX)
                   WHEN C-AND
                       SET LS-GATE-STR TO "and"
                   WHEN C-OR
                       SET LS-GATE-STR TO "or"
                   WHEN C-XOR
                       SET LS-GATE-STR TO "xor"
               END-EVALUATE
               IF WIRE-GATE(WIRE-IDX) = C-INIT
                   DISPLAY WIRE-NAME(WIRE-IDX) ": "
                       WIRE-OUTPUT(WIRE-IDX)
               ELSE
                   DISPLAY WIRE-INPUT-1(WIRE-IDX)
                       " " LS-GATE-STR " "
                       WIRE-INPUT-2(WIRE-IDX)
                       " -> "
                       WIRE-NAME(WIRE-IDX)
                       NO ADVANCING
                   IF WIRE-OUTPUT(WIRE-IDX) NOT = C-OUTPUT-UNKNOWN
                       DISPLAY " (" WIRE-OUTPUT(WIRE-IDX) ")"
                           NO ADVANCING
                   END-IF
                   DISPLAY SPACE
               END-IF
           END-PERFORM

           .
       END PROGRAM DISPLAY-WIRES.

