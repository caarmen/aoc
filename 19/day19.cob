       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY19.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PROCESS-FILE" USING
               BY REFERENCE LS-FILE-PATH.
       END PROGRAM DAY19.

      *> ===============================================================
      *> PROCESS-FILE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO IN-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD             PIC X(3000).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(3000).
       01  LS-TOWEL                  PIC X(10).
       01  LS-TOWEL-PTR              PIC 9(4).
       01  LS-ITER-POSSIBLE-COUNT    PIC 9(6) VALUE 0.
       01  LS-TOTAL-POSSIBLE-COUNT   PIC 9(6) VALUE 0.
       COPY "towel" IN "19".

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       EVALUATE TRUE
                           WHEN TOWELS-SIZE = 0
                               SET LS-TOWEL-PTR TO 1
                               PERFORM UNTIL LS-TOWEL-PTR >
                                   LENGTH FUNCTION TRIM(LS-LINE)
                                   UNSTRING LS-LINE
                                       DELIMITED BY ", "
                                       INTO LS-TOWEL
                                       WITH POINTER LS-TOWEL-PTR
                                   END-UNSTRING
                                   ADD 1 TO TOWELS-SIZE
                                   SET TOWEL(TOWELS-SIZE) TO LS-TOWEL
                               END-PERFORM
                               display "parsed " towels-size " towels"
                           WHEN LS-LINE NOT = SPACE
                               CALL "PROCESS-STRING" USING
                                   TOWELS-GRP
                                   LS-LINE
                                   RETURNING LS-ITER-POSSIBLE-COUNT
                               ADD LS-ITER-POSSIBLE-COUNT TO
                                   LS-TOTAL-POSSIBLE-COUNT
                               display LS-ITER-POSSIBLE-COUNT ": "
                                   FUNCTION TRIM(LS-LINE)
                       END-EVALUATE
           END-PERFORM
           CLOSE FD-DATA

           DISPLAY LS-TOTAL-POSSIBLE-COUNT " patterns are possible."
           .
       END PROGRAM PROCESS-FILE.

      *> ===============================================================
      *> PROCESS-STRING
      *> Return the number of towel combinations possible to make this
      *> pattern.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-STRING.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "cache" IN "19".
       01  LS-INPUT-INDEX-RIGHT             PIC 9(3).
       01  LS-INPUT-INDEX-LEFT              PIC 9(3).
       01  LS-INPUT-LENGTH                  PIC 9(3).
       01  LS-SUBSTRING                     PIC X(100).
       01  LS-SUBSTRING-LEFT                PIC X(100).
       01  LS-SUBSTRING-RIGHT               PIC X(100).
       01  LS-SUBSTRING-LENGTH              PIC 9(3).
       01  LS-SUBSTRING-COUNT               PIC 9(6).
       01  LS-CACHE-RESULT                  PIC 9(1).
       01  LS-TOTAL-POSSIBLE-COUNT          PIC 9(6) VALUE 0.

       LINKAGE SECTION.
       COPY "towel" IN "19".
       01  IN-PATTERN                        PIC X(100).
       PROCEDURE DIVISION USING BY REFERENCE
           TOWELS-GRP
           IN-PATTERN.
           display "pattern " in-pattern

           SET CACHE-SIZE TO 0
           SET LS-INPUT-LENGTH TO LENGTH OF FUNCTION TRIM(IN-PATTERN)
           PERFORM VARYING LS-INPUT-INDEX-RIGHT
               FROM LS-INPUT-LENGTH BY -1
               UNTIL LS-INPUT-INDEX-RIGHT = 0
               SET LS-SUBSTRING TO
                   IN-PATTERN(LS-INPUT-INDEX-RIGHT:LS-INPUT-LENGTH -
                   LS-INPUT-INDEX-RIGHT + 1
               )
               SET LS-SUBSTRING-LENGTH TO LENGTH OF FUNCTION
                   TRIM(LS-SUBSTRING
               )
               display "substring " ls-substring
               PERFORM VARYING LS-INPUT-INDEX-LEFT FROM 1 BY 1 UNTIL
                   LS-INPUT-INDEX-LEFT > LS-SUBSTRING-LENGTH
                   PERFORM VARYING TOWEL-INDEX FROM 1 BY 1
                       UNTIL TOWEL-INDEX > TOWELS-SIZE
                       SET LS-SUBSTRING-LEFT TO LS-SUBSTRING(
                           1:LS-INPUT-INDEX-LEFT
                       )
                       SET LS-SUBSTRING-RIGHT TO LS-SUBSTRING(
                           LS-INPUT-INDEX-LEFT + 1:
                           LS-SUBSTRING-LENGTH - LS-INPUT-INDEX-LEFT + 1
                       )
                       IF LS-SUBSTRING-LEFT = TOWEL(TOWEL-INDEX)
                           AND LS-SUBSTRING-RIGHT NOT = SPACE
                           display " split " function
                           trim(LS-SUBSTRING-LEFT)
                               ": " function trim(LS-SUBSTRING-RIGHT)

                           CALL "GET-FROM-CACHE" USING
                               CACHE-GRP
                               LS-SUBSTRING-RIGHT
                               LS-SUBSTRING-COUNT
                               LS-CACHE-RESULT
                           IF LS-CACHE-RESULT NOT = 0
                               CALL "GET-POSSIBLE-PATTERN-COUNT" USING
                                   TOWELS-GRP
                                   LS-SUBSTRING-RIGHT
                                   RETURNING LS-SUBSTRING-COUNT
                               CALL "ADD-TO-CACHE" USING
                                   CACHE-GRP
                                   LS-SUBSTRING-RIGHT
                                   LS-SUBSTRING-COUNT
                                   LS-CACHE-RESULT
                           END-IF
                           display "  counted " ls-substring-count
                           IF LS-SUBSTRING = IN-PATTERN
                               ADD LS-SUBSTRING-COUNT
                               TO LS-TOTAL-POSSIBLE-COUNT
                           END-IF

                       END-IF
                   END-PERFORM
               END-PERFORM



           END-PERFORM

           MOVE LS-TOTAL-POSSIBLE-COUNT TO RETURN-CODE
           GOBACK.

       END PROGRAM PROCESS-STRING.

      *> ===============================================================
      *> GET-POSSIBLE-PATTERN-COUNT.
      *> Return the number of towel combinations possible to make this
      *> pattern.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-POSSIBLE-PATTERN-COUNT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PUSH-TO-STACK
           FUNCTION POP-STACK.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-PATTERN-LENGTH                 PIC 9(3).
       01  LS-PUSH-RESULT                    PIC 9(1).
       01  LS-POP-RESULT                     PIC 9(1).
       01  LS-TOWEL-PATTERN                  PIC X(100).
       01  LS-NEXT-TOWEL-PATTERN             PIC X(100).
       01  LS-NEXT-PATTERN-LENGTH            PIC 9(3).
       01  LS-TOWEL-COMBO-MATCH-COUNT        PIC 9(6) VALUE 0.
       COPY "stack" IN "19".

       LINKAGE SECTION.
       COPY "towel" IN "19".
       01  IN-PATTERN                        PIC X(100).

       PROCEDURE DIVISION USING BY REFERENCE
           TOWELS-GRP
           IN-PATTERN.

           SET STACK-SIZE TO 0
           SET LS-PATTERN-LENGTH TO LENGTH OF FUNCTION TRIM(IN-PATTERN)
           SET LS-TOWEL-PATTERN TO SPACE
           SET LS-PUSH-RESULT TO PUSH-TO-STACK(
               LS-TOWEL-PATTERN,
               STACK-GRP
           )

           PERFORM UNTIL STACK-SIZE = 0
      *>         display space
      *>         display "iteration"
               SET LS-POP-RESULT TO POP-STACK(
                   STACK-GRP,
                   LS-TOWEL-PATTERN,
               )
      *>         display "popped " function trim(ls-towel-pattern)
               PERFORM VARYING TOWEL-INDEX FROM 1 BY 1
                   UNTIL TOWEL-INDEX > TOWELS-SIZE
                   SET LS-NEXT-TOWEL-PATTERN TO SPACE
                   STRING
                       FUNCTION TRIM(LS-TOWEL-PATTERN)
                       FUNCTION TRIM(TOWEL(TOWEL-INDEX))
                       INTO LS-NEXT-TOWEL-PATTERN
                   END-STRING
                   SET LS-NEXT-PATTERN-LENGTH TO LENGTH OF
                       FUNCTION TRIM(LS-NEXT-TOWEL-PATTERN)
                   IF LS-NEXT-TOWEL-PATTERN = IN-PATTERN
                       ADD 1 TO LS-TOWEL-COMBO-MATCH-COUNT
                       EXIT PERFORM
                   END-IF

      *>             display function trim(ls-next-towel-pattern) " in "
      *>                 function trim(in-pattern) " ? "
      *>                 no advancing
                   IF LS-NEXT-PATTERN-LENGTH <= LS-PATTERN-LENGTH
                       AND IN-PATTERN(1:LS-NEXT-PATTERN-LENGTH) =
                       LS-NEXT-TOWEL-PATTERN
      *>                 display "yes"
                       SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                           LS-NEXT-TOWEL-PATTERN,
                           STACK-GRP
                       )
      *>             ELSE
      *>                 display "no"
                   END-IF
               END-PERFORM
           END-PERFORM

           MOVE LS-TOWEL-COMBO-MATCH-COUNT TO RETURN-CODE
           GOBACK.
           PUSH-NEIGHBORS.

       END PROGRAM GET-POSSIBLE-PATTERN-COUNT.

      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "19".
       01  OUT-TOWELS-PATTERN              PIC X(100).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           STACK-GRP
           OUT-TOWELS-PATTERN
           RETURNING OUT-RESULT
           .

           IF STACK-SIZE > 0
               MOVE STACK-TOWELS-PATTERN(STACK-SIZE)
                   TO OUT-TOWELS-PATTERN
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
       01  IN-TOWELS-PATTERN               PIC X(100).
       COPY "stack" IN "19".
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           IN-TOWELs-PATTERN
           STACK-GRP
           RETURNING OUT-RESULT.

           ADD 1 TO STACK-SIZE
           SET STACK-TOWELS-PATTERN(STACK-SIZE) TO IN-TOWELS-PATTERN

           MOVE 0 TO OUT-RESULT
           GOBACK.
       END FUNCTION PUSH-TO-STACK.

      *> ===============================================================
      *> GET-FROM-CACHE
      *> Read an item from the cache.
      *> Return 0 if the item was found, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-FROM-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "19".
       01  IN-CACHE-KEY                    PIC X(100).
       01  OUT-CACHE-VALUE                 PIC 9(6).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           OUT-CACHE-VALUE
           OUT-RESULT.

           SEARCH ALL CACHE-CALCS
               AT END
                   MOVE 1 TO OUT-RESULT
               WHEN CACHE-KEY(CACHE-INDEX) = IN-CACHE-KEY
                   MOVE CACHE-VALUE(CACHE-INDEX) TO OUT-CACHE-VALUE
                   MOVE 0 TO OUT-RESULT

           GOBACK.
       END PROGRAM GET-FROM-CACHE.
      *> ===============================================================
      *> ADD-TO-CACHE
      *> Add an item to the cache
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-TO-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "19".
       01  IN-CACHE-KEY                    PIC X(100).
       01  IN-CACHE-VALUE                  PIC 9(6).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           IN-CACHE-VALUE
           OUT-RESULT.

           ADD 1 TO CACHE-SIZE
           SET CACHE-VALUE(CACHE-SIZE) TO IN-CACHE-VALUE
           SET CACHE-KEY(CACHE-SIZE) TO IN-CACHE-KEY

           SORT CACHE-CALCS
           MOVE 0 TO OUT-RESULT
           GOBACK.
       END PROGRAM ADD-TO-CACHE.
