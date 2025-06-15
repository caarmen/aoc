       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY24.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(100).
       01  LS-INPUT-1                PIC 9(18) COMP.
       01  LS-INPUT-2                PIC 9(18) COMP.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-TOTAL-OUTPUT           PIC 9(18) COMP VALUE 0.
       COPY "wire" IN "24".

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE

           UNSTRING LS-COMMAND-LINE DELIMITED BY " "
               INTO LS-FILE-PATH LS-INPUT-1 LS-INPUT-2
           END-UNSTRING

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               WIRE-GRP

           CALL "DISPLAY-WIRES" USING
               WIRE-GRP
           CALL "FIND-INVALID-Z-WIRES" USING
               WIRE-GRP
           CALL "DISPLAY-WIRES" USING
               WIRE-GRP
           CALL "SETUP-INPUT" USING
               WIRE-GRP
               LS-INPUT-1
               LS-INPUT-2
           CALL "EVALUATE-ALL" USING
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
                               IF WIRE-NAME(WIRE-SIZE) = "y00"
                                   COMPUTE WIRE-INPUT-BIT-SIZE =
                                       FUNCTION NUMVAL(
                                           WIRE-NAME(WIRE-SIZE - 1)(2:2)
                                       ) + 1
                               END-IF
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
      *> SWAP-WIRES.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWAP-WIRES.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-WIRE-IDX-1                                 PIC 9(3).
       01  LS-WIRE-IDX-2                                 PIC 9(3).
       LINKAGE SECTION.
       COPY "wire" IN "24".
       01  IN-WIRE-NAME-1                                PIC X(3).
       01  IN-WIRE-NAME-2                                PIC X(3).
       PROCEDURE DIVISION USING BY REFERENCE
           WIRE-GRP
           IN-WIRE-NAME-1
           IN-WIRE-NAME-2.

           DISPLAY "Swap " IN-WIRE-NAME-1 "," IN-WIRE-NAME-2
           SET WIRE-IDX TO 1
           SEARCH ALL WIRES
               WHEN WIRE-NAME(WIRE-IDX) = IN-WIRE-NAME-1
                   SET LS-WIRE-IDX-1 TO WIRE-IDX
           END-SEARCH

           SEARCH ALL WIRES
               WHEN WIRE-NAME(WIRE-IDX) = IN-WIRE-NAME-2
                   SET LS-WIRE-IDX-2 TO WIRE-IDX
           END-SEARCH

           SET WIRE-NAME(LS-WIRE-IDX-2) TO IN-WIRE-NAME-1
           SET WIRE-NAME(LS-WIRE-IDX-1) TO IN-WIRE-NAME-2
           SORT WIRES
           .

       END PROGRAM SWAP-WIRES.

      *> ===============================================================
      *> FIND-INVALID-Z-WIRES
      *> Return the names of the wires whose name starts with z and
      *> whose gate isn't the expected XOR gate.
      *> xNN xor yNN should make a wire aaa.
      *> This aaa should be xor'd with a bbb, to make zNN.
      *> Look for xNN xor yNN that doesn't make an aaa which makes
      *> a zNN.
      *> Return the list of found zNNs.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-INVALID-Z-WIRES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "24".
       LOCAL-STORAGE SECTION.
       01  LS-WIRE-IDX                            PIC 9(3).
       01  LS-EXPECTED-Z-WIRE                     PIC X(3).
       01  LS-FOUND-OUTPUT-WIRE                   PIC X(3).
       01  LS-SWAP-LIST-GRP.
           05  LS-SWAP-LIST-SIZE                  PIC 9(1) VALUE 0.
           05  LS-SWAP-LIST-ITEMS OCCURS 1 TO 8 TIMES
               DEPENDING ON LS-SWAP-LIST-SIZE
               ASCENDING KEY IS LS-SWAP-LIST-ITEM
               INDEXED BY LS-SWAP-LIST-IDX.
               10 LS-SWAP-LIST-ITEM               PIC X(3).
       01  LS-SWAP-GRP.
           05  LS-SWAP-SIZE                       PIC 9(1) VALUE 0.
           05  LS-SWAPS OCCURS 1 TO 4 TIMES 
               DEPENDING ON LS-SWAP-SIZE
               INDEXED BY LS-SWAP-IDX.
               10 LS-SWAP-NAME-1                  PIC X(3).
               10 LS-SWAP-NAME-2                  PIC X(3).
       LINKAGE SECTION.
       COPY "wire" IN "24".

       PROCEDURE DIVISION USING BY REFERENCE
           WIRE-GRP.

           PERFORM VARYING LS-WIRE-IDX FROM 1 BY 1 UNTIL LS-WIRE-IDX >
               WIRE-SIZE
               IF (
                   WIRE-INPUT-1(LS-WIRE-IDX)(1:1) = "x"
                       OR WIRE-INPUT-1(LS-WIRE-IDX)(1:1) = "y"
                   )
                   AND WIRE-GATE(LS-WIRE-IDX) = C-XOR
                   SET WIRE-IDX TO 1
                   SEARCH WIRES
                       VARYING WIRE-IDX
                       WHEN (
                               WIRE-INPUT-1(WIRE-IDX) =
                                   WIRE-NAME(LS-WIRE-IDX)
                               OR WIRE-INPUT-2(WIRE-IDX) =
                                   WIRE-NAME(LS-WIRE-IDX)
                           )
                           AND WIRE-GATE(WIRE-IDX) = C-XOR
                           AND WIRE-NAME(WIRE-IDX)(1:1) NOT = "z"

                           SET LS-FOUND-OUTPUT-WIRE TO
                               WIRE-NAME(WIRE-IDX)

                           STRING "z"
                               WIRE-INPUT-1(LS-WIRE-IDX)(2:2)
                               INTO 
                               LS-EXPECTED-Z-WIRE
                           END-STRING

                           ADD 1 TO LS-SWAP-SIZE
                           SET LS-SWAP-NAME-1(LS-SWAP-SIZE) TO
                               LS-FOUND-OUTPUT-WIRE
                           SET LS-SWAP-NAME-2(LS-SWAP-SIZE) TO
                               LS-EXPECTED-Z-WIRE
                   END-SEARCH
               END-IF
           END-PERFORM
      *> This last one (khg and tvb) I found by hand!
      *> How? I tried different additions using this program, providng
      *> numbers to add onthe command line.
      *> I provided 0 and an increasiningly large number, until the result was
      *> incorrect.
      *> I noticed the incorrect numbers started as soon as the answer
      *> had a 1 in the 25th bit, so I took a look at z25.
      *> I noticed its grandparent was (x25 and y25) instead of the
      *> expected (x25 xor y25). I swapped its parent (tvb) to get the
      *> right parent (khg) which comes from (x25 xor y25).
           ADD 1 TO LS-SWAP-SIZE
           SET LS-SWAP-NAME-1(LS-SWAP-SIZE) TO "khg"
           SET LS-SWAP-NAME-2(LS-SWAP-SIZE) TO "tvb"
           PERFORM VARYING LS-SWAP-IDX FROM 1 BY 1 UNTIL LS-SWAP-IDX >
               LS-SWAP-SIZE
               CALL "SWAP-WIRES" USING
                   WIRE-GRP
                   LS-SWAP-NAME-1(LS-SWAP-IDX)
                   LS-SWAP-NAME-2(LS-SWAP-IDX)
               ADD 1 TO LS-SWAP-LIST-SIZE
               SET LS-SWAP-LIST-ITEM(LS-SWAP-LIST-SIZE) TO
                   LS-SWAP-NAME-1(LS-SWAP-IDX)
               ADD 1 TO LS-SWAP-LIST-SIZE
               SET LS-SWAP-LIST-ITEM(LS-SWAP-LIST-SIZE) TO
                   LS-SWAP-NAME-2(LS-SWAP-IDX)
               SORT LS-SWAP-LIST-ITEMS

           END-PERFORM

           PERFORM VARYING LS-SWAP-LIST-IDX FROM 1 BY 1 UNTIL 
               LS-SWAP-LIST-IDX > LS-SWAP-LIST-SIZE
               DISPLAY LS-SWAP-LIST-ITEM(LS-SWAP-LIST-IDX) "," NO
                   ADVANCING
           END-PERFORM
           DISPLAY SPACE
           .
       END PROGRAM FIND-INVALID-Z-WIRES.

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
      *> SETUP-INPUT.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SETUP-INPUT.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-INPUT-BINARY-STR-1                PIC X(45).
       01  LS-INPUT-BINARY-STR-2                PIC X(45).
       01  LS-INPUT-1-NAME                      PIC X(3).
       01  LS-INPUT-1-VALUE                     PIC 9(1).
       01  LS-INPUT-2-NAME                      PIC X(3).
       01  LS-INPUT-2-VALUE                     PIC 9(1).
       01  LS-IDX                               PIC 9(2).
       LINKAGE SECTION.
       COPY "wire" IN "24".
       01  IN-INPUT-1                           PIC 9(18) COMP.
       01  IN-INPUT-2                           PIC 9(18) COMP.
       PROCEDURE DIVISION USING BY REFERENCE
           WIRE-GRP
           IN-INPUT-1
           IN-INPUT-2.

           CALL "TO-BINARY-STRING" USING
               IN-INPUT-1
               WIRE-INPUT-BIT-SIZE
               LS-INPUT-BINARY-STR-1

           CALL "TO-BINARY-STRING" USING
               IN-INPUT-2
               WIRE-INPUT-BIT-SIZE
               LS-INPUT-BINARY-STR-2

           PERFORM VARYING LS-IDX FROM 0 BY 1 UNTIL
               LS-IDX = WIRE-INPUT-BIT-SIZE
               STRING "x" LS-IDX
                   INTO LS-INPUT-1-NAME
               END-STRING
               SET LS-INPUT-1-VALUE TO LS-INPUT-BINARY-STR-1(
                   WIRE-INPUT-BIT-SIZE - LS-IDX:1
               )
               STRING "y" LS-IDX
                   INTO LS-INPUT-2-NAME
               END-STRING
               SET LS-INPUT-2-VALUE TO LS-INPUT-BINARY-STR-2(
                   WIRE-INPUT-BIT-SIZE - LS-IDX:1
               )
               SET WIRE-IDX TO 1
               SEARCH ALL WIRES
                   WHEN WIRE-NAME(WIRE-IDX) = LS-INPUT-1-NAME
                       SET WIRE-OUTPUT(WIRE-IDX) TO LS-INPUT-1-VALUE
               END-SEARCH
               SET WIRE-IDX TO 1
               SEARCH ALL WIRES
                   WHEN WIRE-NAME(WIRE-IDX) = LS-INPUT-2-NAME
                       SET WIRE-OUTPUT(WIRE-IDX) TO LS-INPUT-2-VALUE
               END-SEARCH
           END-PERFORM

           display "will add " in-input-1 " + " in-input-2
               " which is " function trim(ls-input-binary-str-1) " + "
               function trim(ls-input-binary-str-2)

           .
       END PROGRAM SETUP-INPUT.

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

