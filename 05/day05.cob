       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY05.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *> Make the file external so we can access it from other programs
      *> https://stackoverflow.com/questions/67910111/is-it-possible-to-pass-a-cobol-file-descriptor-to-another-program
       FD FD-DATA EXTERNAL.
       01  F-DATA-RECORD                 PIC X(100).
       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                  PIC X(20).
       01  LS-LINE                       PIC X(100).
       01  LS-UPDATE-RESULT              USAGE BINARY-LONG VALUE 0.
       01  LS-TOTAL-RESULT               USAGE BINARY-LONG VALUE 0.
       01  LS-CORRECTED-RESULT           USAGE BINARY-LONG VALUE 0.
       01  LS-TOTAL-CORRECTED-RESULT     USAGE BINARY-LONG VALUE 0.
       01  LS-UPDATE-MIDDLE-INDEX        PIC 9(2) USAGE COMP.
       COPY "rules" IN "05".
       COPY "update" IN "05".

       PROCEDURE DIVISION.
           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           OPEN INPUT FD-DATA
           CALL "PARSE-RULES" USING
               BY REFERENCE RULES-GRP
           SORT RULES
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-DATA-RECORD
               AT END
                   EXIT PERFORM
               NOT AT END
                   MOVE F-DATA-RECORD TO LS-LINE
                   CALL "PARSE-UPDATE" USING
                       LS-LINE
                       UPDATES-GRP

                   CALL "CHECK-UPDATE" USING
                       BY REFERENCE RULES-GRP
                       BY REFERENCE UPDATES-GRP
                       RETURNING LS-UPDATE-RESULT
                   COMPUTE LS-TOTAL-RESULT = LS-TOTAL-RESULT +
                       LS-UPDATE-RESULT
                       IF LS-UPDATE-RESULT = 0
                       THEN
                           CALL "SORT-ITEMS" USING
                               BY REFERENCE RULES-GRP
                               BY REFERENCE UPDATES-GRP
                           COMPUTE LS-UPDATE-MIDDLE-INDEX =
                                   (UPDATE-SIZE / 2) + 1
                           COMPUTE LS-CORRECTED-RESULT =
                               UPDATE-ITEM(LS-UPDATE-MIDDLE-INDEX)
                           ADD LS-CORRECTED-RESULT TO
                               LS-TOTAL-CORRECTED-RESULT

                       END-IF
           END-PERFORM
           CLOSE FD-DATA

           DISPLAY "RESULT: " LS-TOTAL-RESULT
           DISPLAY "CORRECTED RESULT: " LS-TOTAL-CORRECTED-RESULT
           GOBACK.
       END PROGRAM DAY05.

      *> ===============================================================
      *> PARSE-RULES.
      *>
      *> Parse all the rules in the file to table of rules.
      *>
      *> Each rule entry has a key (a number) and table of numbers
      *> which must come after the key, in updates.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-RULES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA EXTERNAL.
       01  F-DATA-RECORD                 PIC X(100).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                       PIC X(100).
       01  LS-LINE-LENGTH                PIC 9(3) USAGE COMP.

       01  RULE-BEFORE-TOKEN             PIC 9(2) USAGE COMP.
       01  RULE-AFTER-TOKEN              PIC 9(2) USAGE COMP.

       LINKAGE SECTION.
       COPY "rules" IN "05".
      *> Passing a file:
      *> https://www.microfocus.com/documentation/extend-acucobol/925/BKTRTRHPCBS071.html

       PROCEDURE DIVISION USING
           BY REFERENCE RULES-GRP.
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-DATA-RECORD
               MOVE F-DATA-RECORD TO LS-LINE
      *> Read up until the blank line
               IF LS-LINE = SPACES
               THEN
                   EXIT PERFORM
               END-IF
      *> This is a rule line
               SET LS-LINE-LENGTH TO LENGTH OF FUNCTION TRIM(LS-LINE)
               UNSTRING LS-LINE
                   DELIMITED BY "|"
                   INTO RULE-BEFORE-TOKEN, RULE-AFTER-TOKEN

               SET RULE-INDEX TO 1
               SEARCH RULES
                   AT END
                       ADD 1 TO RULES-SIZE
                       SET RULE-KEY(RULES-SIZE) TO RULE-BEFORE-TOKEN
                       SET RULE-AFTER-SIZE(RULES-SIZE) TO 1
                       MOVE RULE-AFTER-TOKEN TO
                       RULE-AFTER-ITEM(
                           RULES-SIZE
                           RULE-AFTER-SIZE(RULES-SIZE)
                       )
                   WHEN RULE-KEY(RULE-INDEX)
                       = RULE-BEFORE-TOKEN
                       ADD 1 TO
                           RULE-AFTER-SIZE(RULE-INDEX)
                       MOVE RULE-AFTER-TOKEN TO
                       RULE-AFTER-ITEM(
                           RULE-INDEX,
                           RULE-AFTER-SIZE(RULE-INDEX)
                       )
               END-SEARCH
           END-PERFORM
           GOBACK.
       END PROGRAM PARSE-RULES.


      *> ===============================================================
      *> PARSE-UPDATE.
      *>
      *> Parse a single update line.
      *> Return an update group which contains a table of numbers
      *> (update items) as well as the the size (the number of items).
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-UPDATE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-LINE-LENGTH                PIC 9(3) USAGE COMP.
       01  LS-UPDATES-TOKEN-PTR          PIC 9(9).
       01  LS-UPDATE-TOKEN               PIC 9(2) USAGE COMP.

       LINKAGE SECTION.
       01  IN-LINE                       PIC X(100).
       COPY "update" IN "05".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LINE
           BY REFERENCE UPDATES-GRP.

           SET LS-LINE-LENGTH TO LENGTH OF FUNCTION TRIM(IN-LINE)
           SET LS-UPDATES-TOKEN-PTR TO 1
           SET UPDATE-SIZE TO 0
           PERFORM UNTIL LS-UPDATES-TOKEN-PTR > LS-LINE-LENGTH
               UNSTRING FUNCTION TRIM(IN-LINE)
                   DELIMITED BY ","
                   INTO LS-UPDATE-TOKEN
                   WITH POINTER LS-UPDATES-TOKEN-PTR
               END-UNSTRING
               ADD 1 TO UPDATE-SIZE
               MOVE LS-UPDATE-TOKEN TO UPDATE-ITEM(UPDATE-SIZE)
           END-PERFORM

           GOBACK.
       END PROGRAM PARSE-UPDATE.

      *> ===============================================================
      *> CHECK-UPDATE.
      *>
      *> Checks that, for each number X in the update, that any and all
      *> rules for this number X are respected.
      *> For each rule, if the "after" numbers appear in the update,
      *> they must be after the number X.
      *>
      *> Returns the middle number of the update, if the rules are
      *> respected, otherwise returns 0.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-UPDATE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-UPDATE-ITEM-INDEX           PIC 9(2) USAGE COMP.
       01  LS-UPDATE-MIDDLE-INDEX         PIC 9(2) USAGE COMP.
       01  LS-UPDATE-CHECK-ITEM-RESULT    USAGE BINARY-LONG VALUE 0.
       01  LS-UPDATE-CHECK-TOTAL-RESULT   USAGE BINARY-LONG VALUE 0.
       LINKAGE SECTION.
       COPY "rules" IN "05".
       COPY "update" IN "05".

       PROCEDURE DIVISION USING
           BY REFERENCE RULES-GRP
           BY REFERENCE UPDATES-GRP.

           PERFORM VARYING LS-UPDATE-ITEM-INDEX
               FROM 1 BY 1 UNTIL LS-UPDATE-ITEM-INDEX > UPDATE-SIZE
                   SEARCH ALL RULES
                       WHEN RULE-KEY(RULE-INDEX)
                           = UPDATE-ITEM(LS-UPDATE-ITEM-INDEX)
                           CALL "CHECK-UPDATE-ITEM" USING
                               BY REFERENCE LS-UPDATE-ITEM-INDEX
                               BY REFERENCE RULES(RULE-INDEX)
                               BY REFERENCE UPDATES-GRP
                               RETURNING LS-UPDATE-CHECK-ITEM-RESULT
                           IF LS-UPDATE-CHECK-ITEM-RESULT NOT = 0
                           THEN
                               EXIT PERFORM
                           END-IF
           END-PERFORM
           IF LS-UPDATE-CHECK-ITEM-RESULT = 0
           THEN
               COMPUTE LS-UPDATE-MIDDLE-INDEX = (UPDATE-SIZE / 2) + 1
               COMPUTE LS-UPDATE-CHECK-TOTAL-RESULT =
                   UPDATE-ITEM(LS-UPDATE-MIDDLE-INDEX)
           END-IF
           MOVE LS-UPDATE-CHECK-TOTAL-RESULT TO RETURN-CODE
           GOBACK.
       END PROGRAM CHECK-UPDATE.

      *> ===============================================================
      *> CHECK-UPDATE-ITEM.
      *>
      *> Checks that the given rule is respected.
      *> If the rule "after" numbers appear in the update,
      *> they must be after the number X.
      *>
      *> Returns 0 if the rule is respected, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-UPDATE-ITEM.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01  IN-UPDATE-ITEM-INDEX      PIC 9(2) USAGE COMP.
       01  RULE-GRP-01.
           05  FILLER.
       COPY "rule" IN "05".
       COPY "update" IN "05".
       PROCEDURE DIVISION USING
           BY REFERENCE IN-UPDATE-ITEM-INDEX
           BY REFERENCE RULE-GRP-01
           BY REFERENCE UPDATES-GRP.

           MOVE 0 TO RETURN-CODE

           PERFORM VARYING RULE-AFTER-INDEX FROM 1 BY 1
               UNTIL RULE-AFTER-INDEX > RULE-AFTER-SIZE
               SET UPDATE-INDEX TO 1
               SEARCH UPDATES VARYING UPDATE-INDEX
                   WHEN UPDATE-ITEM(UPDATE-INDEX) =
                       RULE-AFTER-ITEM(RULE-AFTER-INDEX)

                       IF UPDATE-INDEX < IN-UPDATE-ITEM-INDEX
                       THEN
                           MOVE 1 TO RETURN-CODE
                           EXIT PERFORM
                       END-IF



           END-PERFORM

           GOBACK.
       END PROGRAM CHECK-UPDATE-ITEM.
      *> ===============================================================
      *> SORT-ITEMS.
      *>
      *> Not only did I search for the algo on wikipedia, but I also
      *> picked the easest algo (worst performance):
      *> https://en.wikipedia.org/wiki/Bubble_sort
      *>
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-ITEMS.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-SORT-INDEX         PIC 9(2) USAGE COMP.
       01  LS-COMPARISON         USAGE BINARY-LONG.
       01  LS-SWAPPED            PIC 9(1) VALUE 0.
       01  LS-TEMP-ITEM          PIC 9(2) USAGE COMP.
       LINKAGE SECTION.
       COPY "rules" IN "05".
       COPY "update" IN "05".

       PROCEDURE DIVISION USING
           BY REFERENCE RULES-GRP
           BY REFERENCE UPDATES-GRP.

           SET LS-SWAPPED TO 1
           PERFORM UNTIL LS-SWAPPED = 0
               SET LS-SWAPPED TO 0
               PERFORM VARYING LS-SORT-INDEX FROM 2 BY 1 UNTIL
                   LS-SORT-INDEX > UPDATE-SIZE
                   CALL "COMPARE-TWO-ITEMS" USING
                       BY REFERENCE UPDATE-ITEM(LS-SORT-INDEX - 1)
                       BY REFERENCE UPDATE-ITEM(LS-SORT-INDEX)
                       BY REFERENCE RULES-GRP
                       RETURNING LS-COMPARISON

                       IF LS-COMPARISON = 1
                       THEN
                           SET LS-SWAPPED TO 1
                           SET LS-TEMP-ITEM TO
                               UPDATE-ITEM(LS-SORT-INDEX - 1)
                           SET UPDATE-ITEM(LS-SORT-INDEX - 1)
                               TO UPDATE-ITEM(LS-SORT-INDEX)
                           SET UPDATE-ITEM(LS-SORT-INDEX)
                               TO LS-TEMP-ITEM
                       END-IF

               END-PERFORM
           END-PERFORM
           GOBACK.
       END PROGRAM SORT-ITEMS.

      *> ===============================================================
      *> COMPARE-TWO-ITEMS.
      *>
      *> Compare two distinct items
      *>
      *> Return -1 if the first item should appear before the second
      *> item, 1 if the second item should appear first, or 0 if
      *> there's no rules for these two items.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPARE-TWO-ITEMS.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

       LINkAGE SECTION.
       01  IN-ITEM-1               PIC 9(2) USAGE COMP.
       01  IN-ITEM-2               PIC 9(2) USAGE COMP.
       COPY "rules" IN "05".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ITEM-1
           BY REFERENCE IN-ITEM-2
           BY REFERENCE RULES-GRP.

      *> Find the rule for item 1
           SEARCH ALL RULES
               WHEN RULE-KEY(RULE-INDEX) = IN-ITEM-1

                   PERFORM VARYING RULE-AFTER-INDEX
                       FROM 1 BY 1 UNTIL
                       RULE-AFTER-INDEX > RULE-AFTER-SIZE(RULE-INDEX)

                       IF RULE-AFTER-ITEM(RULE-INDEX,RULE-AFTER-INDEX)
                           = IN-ITEM-2
      *> Found item 2 in this list, return -1
      *> (item 1 should be before item2)
                           MOVE -1 TO RETURN-CODE
                           GOBACK
                   END-PERFORM
           END-SEARCH

      *> Find the rule for item 2
           SEARCH ALL RULES
               WHEN RULE-KEY(RULE-INDEX) = IN-ITEM-2
                   PERFORM VARYING RULE-AFTER-INDEX
                       FROM 1 BY 1 UNTIL
                       RULE-AFTER-INDEX > RULE-AFTER-SIZE(RULE-INDEX)

                       IF RULE-AFTER-ITEM(RULE-INDEX,RULE-AFTER-INDEX)
                           = IN-ITEM-1
      *> Found item 1 in this list, return 1
      *> (item 2 should be before item 1)
                           MOVE 1 TO RETURN-CODE
                           GOBACK
                   END-PERFORM
           END-SEARCH
           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM COMPARE-TWO-ITEMS.

      *> ===============================================================
      *> DISPLAY-RULE
      *>
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-RULE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

       LINkAGE SECTION.
       01  RULE-GRP-01.
           05  FILLER.
       COPY "rule" IN "05".

       PROCEDURE DIVISION USING
           BY REFERENCE RULE-GRP-01.

           DISPLAY RULE-KEY ": " WITH NO ADVANCING
           PERFORM VARYING RULE-AFTER-INDEX FROM 1 BY 1
               UNTIL RULE-AFTER-INDEX > RULE-AFTER-SIZE
               DISPLAY " " RULE-AFTER-ITEM(RULE-AFTER-INDEX)
                   WITH NO ADVANCING
           END-PERFORM
           DISPLAY " "

           GOBACK.
       END PROGRAM DISPLAY-RULE.
