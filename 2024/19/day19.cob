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
       01  LS-ITER-POSSIBLE-COUNT    PIC 9(16) VALUE 0.
       01  LS-PART-1-COUNT           PIC 9(4) VALUE 0.
       01  LS-PART-2-COUNT           PIC 9(16) VALUE 0.
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
                               SORT TOWELS
                               DISPLAY "Parsed " TOWELS-SIZE " towels"
                           WHEN LS-LINE NOT = SPACE
                               CALL "PROCESS-STRING" USING
                                   TOWELS-GRP
                                   LS-LINE
                                   LS-ITER-POSSIBLE-COUNT
                               ADD LS-ITER-POSSIBLE-COUNT TO
                                   LS-PART-2-COUNT
                               IF LS-ITER-POSSIBLE-COUNT > 0
                                   ADD 1 TO LS-PART-1-COUNT
                               END-IF
      *>                         DISPLAY LS-ITER-POSSIBLE-COUNT ": "
      *>                             FUNCTION TRIM(LS-LINE)
                       END-EVALUATE
           END-PERFORM
           CLOSE FD-DATA

           DISPLAY LS-PART-1-COUNT " patterns are possible."
           DISPLAY LS-PART-2-COUNT " pattern combos are possible."
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
       01  LS-SUBSTRING-SPLIT-COUNT         PIC 9(16).
       01  LS-SUBSTRING-COUNT               PIC 9(16).
       01  LS-CACHE-RESULT                  PIC 9(1).

       LINKAGE SECTION.
       COPY "towel" IN "19".
       01  IN-PATTERN                       PIC X(100).
       01  OUT-RESULT                       PIC 9(16) VALUE 0.
       PROCEDURE DIVISION USING BY REFERENCE
           TOWELS-GRP
           IN-PATTERN
           OUT-RESULT.

           SET CACHE-SIZE TO 0
           SET LS-INPUT-LENGTH TO LENGTH OF FUNCTION TRIM(IN-PATTERN)
           SET LS-SUBSTRING TO SPACE
           SET LS-SUBSTRING-COUNT TO 1

      *> Add a special empty string substring to the cache, with a
      *> cache value of 1. There is exactly 1 "combination" of towels
      *> that can be mixed to form an empty string (no towels at all).
           CALL "ADD-TO-CACHE" USING
               CACHE-GRP
               LS-SUBSTRING
               LS-SUBSTRING-COUNT
               LS-CACHE-RESULT

      *> Process our input string with progressively larger substrings
      *> starting from the right.
      *> For an IN-PATTERN of "rrbgbr", we'll process:
      *> r
      *> br
      *> gbr
      *> bgbr
      *> rbgbr
      *> rrbgbr <-- the whole IN-PATTERN
           PERFORM VARYING LS-INPUT-INDEX-RIGHT
               FROM LS-INPUT-LENGTH BY -1
               UNTIL LS-INPUT-INDEX-RIGHT = 0
               SET LS-SUBSTRING-COUNT TO 0
               SET LS-SUBSTRING-SPLIT-COUNT TO 0

      *> Example: LS-SUBSTRING = gbr, for our 3rd iteration.
               SET LS-SUBSTRING TO
                   IN-PATTERN(LS-INPUT-INDEX-RIGHT:LS-INPUT-LENGTH -
                   LS-INPUT-INDEX-RIGHT + 1
               )
      *> Example: LS-SUBSTRING-LENGTH = 3, for gbr
               SET LS-SUBSTRING-LENGTH TO LENGTH OF FUNCTION
                   TRIM(LS-SUBSTRING)
      *> Now process our substring with progressively larger substrings
      *> starting from the left.
      *> For a LS-SUBSTRING of "gbr", we'll process:
      *> g
      *> gb
      *> gbr <-- the whole substring
               PERFORM VARYING LS-INPUT-INDEX-LEFT FROM 1 BY 1 UNTIL
                   LS-INPUT-INDEX-LEFT > LS-SUBSTRING-LENGTH
      *> Example: for our sub iteration number 2:
      *>   LS-SUBSTRING-LEFT = gb
      *>   LS-SUBSTRING-RIGHT = r
                   SET LS-SUBSTRING-LEFT TO LS-SUBSTRING(
                       1:LS-INPUT-INDEX-LEFT
                   )
                   SET LS-SUBSTRING-RIGHT TO LS-SUBSTRING(
                       LS-INPUT-INDEX-LEFT + 1:
                       LS-SUBSTRING-LENGTH - LS-INPUT-INDEX-LEFT + 1
                   )

      *> Check if "gb" matches one of the towels exactly, if so:
      *>
      *> The number of combos of our substring "gbr", which start with
      *> the left part "gb" is equal to the number of combos 
      *> which start with the right part, "r".
      *> We should have the number of combos of the right part "r"
      *> already in the cache, from previous iterations.
                   SEARCH ALL TOWELS
                       WHEN LS-SUBSTRING-LEFT(1:10) = TOWEL(TOWEL-INDEX)
                           CALL "GET-FROM-CACHE" USING
                               CACHE-GRP
                               LS-SUBSTRING-RIGHT
                               LS-SUBSTRING-SPLIT-COUNT
                               LS-CACHE-RESULT
                           ADD LS-SUBSTRING-SPLIT-COUNT TO
                               LS-SUBSTRING-COUNT
                   END-SEARCH
               END-PERFORM
               CALL "ADD-TO-CACHE" USING
                   CACHE-GRP
                   LS-SUBSTRING
                   LS-SUBSTRING-COUNT
                   LS-CACHE-RESULT

           END-PERFORM

           MOVE LS-SUBSTRING-COUNT TO OUT-RESULT
           GOBACK.

       END PROGRAM PROCESS-STRING.

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
       01  OUT-CACHE-VALUE                 PIC 9(16).
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
       01  IN-CACHE-VALUE                  PIC 9(16).
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
