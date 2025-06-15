      *> =================================================================
      *> Copyright 2025 - Present, Carmen Alvarez
      *>
      *> This file is part of Advent of code - @caarmen.
      *>
      *> Advent of code - @caarmen is free software: you can redistribute
      *> it and/or modify it under the terms of the GNU General Public
      *> License as published by the Free Software Foundation, either
      *> version 3 of the License, or (at your option) any later version.
      *>
      *> Advent of code - @caarmen is distributed in the hope that it will
      *> be useful, but WITHOUT ANY WARRANTY; without even the implied
      *> warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
      *> See the GNU General Public License for more details.
      *>
      *> You should have received a copy of the GNU General Public License
      *> along with Advent of code - @caarmen. If not, see
      *> <https://www.gnu.org/licenses/>.
      *> =================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION Is LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FD-DATA.
      *> Example data:
      *> 7 6 4 2 1
      *> 1 2 7 8 9
      *> 9 7 6 2 1
      *> 1 3 2 4 5
      *> 8 6 4 4 1
      *> 1 3 6 7 9
       01  F-DATA-RECORD                   PIC X(100).
      
       LOCAL-STORAGE SECTION.
      *> Command-line arguments:
      *> LS-PART: whether to apply the logic for part 1 or 2 of aoc.
       01  LS-COMMAND-LINE                 PIC X(103).
       01  LS-PART                         PIC 9(1).
       01  LS-FILE-PATH                    PIC X(100).

      *> Data items for parsing a line
       01  LS-LINE                         PIC X(100).
       01  LS-TOKEN-COUNT                  USAGE BINARY-LONG.
       01  LS-SOURCE-TOKEN-TABLE-GROUP.
           05  LS-SOURCE-TOKEN-TABLE       OCCURS 16 TIMES.
               10  LS-SOURCE-TOKEN         PIC X(4).
      *> Local data items for keeping track of safe lines.
       01  LS-IS-LINE-SAFE                 USAGE BINARY-LONG.
       01  LS-SAFE-COUNT                   PIC 9(4) VALUE 0.
      *> Local data items for the problem dampener.
       01  LS-CHEAT-TOKEN-COUNT            USAGE BINARY-LONG.
       01  LS-ATTEMPT-IDX                  USAGE BINARY-LONG.
       01  LS-TEMP-TOKEN-TABLE-GROUP.
           05  LS-TEMP-TOKEN-TABLE         OCCURS 16 TIMES.
               10  LS-TEMP-TOKEN           PIC X(4).


       PROCEDURE DIVISION.

      *> Read the file path from the command line arguments.
       ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
       UNSTRING LS-COMMAND-LINE DELIMITED BY " "
           INTO LS-PART LS-FILE-PATH

       OPEN INPUT FD-DATA
       PERFORM UNTIL EXIT
           READ FD-DATA INTO F-DATA-RECORD
           AT END
               EXIT PERFORM
           NOT AT END
               MOVE F-DATA-RECORD TO LS-LINE
      *> Parse the levels from the line into our table.
               CALL "TOKENIZE-STRING" USING
                   BY REFERENCE LS-LINE
                   BY REFERENCE LS-SOURCE-TOKEN-TABLE-GROUP
                   RETURNING LS-TOKEN-COUNT
      *> Check if the line is safe as-is.
               CALL "IS-LINE-SAFE" USING
                   BY REFERENCE LS-TOKEN-COUNT
                   BY REFERENCE LS-SOURCE-TOKEN-TABLE-GROUP
                   RETURNING LS-IS-LINE-SAFE
               IF LS-IS-LINE-SAFE = 1
               THEN
                   ADD 1 TO LS-SAFE-COUNT
               ELSE
                   IF LS-PART = "2"
                   THEN
      *> The line wasn't safe.
      *> See if we can "cheat" by removing one of the levels.
                       COMPUTE LS-CHEAT-TOKEN-COUNT = LS-TOKEN-COUNT - 1
                       PERFORM VARYING LS-ATTEMPT-IDX
                           FROM 1 BY 1
                           UNTIL LS-ATTEMPT-IDX > LS-TOKEN-COUNT
                               OR LS-IS-LINE-SAFE = 1
      *> Get a copy of the table with one item removed.
                           CALL "REMOVE-ITEM-FROM-TABLE" USING
                               BY REFERENCE LS-TOKEN-COUNT
                               BY REFERENCE LS-ATTEMPT-IDX
                               BY REFERENCE LS-SOURCE-TOKEN-TABLE-GROUP
                               BY REFERENCE LS-TEMP-TOKEN-TABLE-GROUP
      *> See if this reduced table is safe.
                           CALL "IS-LINE-SAFE" USING
                               BY REFERENCE LS-CHEAT-TOKEN-COUNT
                               BY REFERENCE LS-TEMP-TOKEN-TABLE-GROUP
                               RETURNING LS-IS-LINE-SAFE
                           IF LS-IS-LINE-SAFE = 1
                           THEN
                               ADD 1 TO LS-SAFE-COUNT
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
       END-PERFORM
       CLOSE FD-DATA
       DISPLAY "Safe count: " LS-SAFE-COUNT
       GOBACK.
       END PROGRAM DAY02.

      *> ===============================================================
      *> IS-LINE-SAFE.
      *>
      *> Returns 1 if the line is safe, 0 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-LINE-SAFE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY constants IN "02".
       LOCAL-STORAGE SECTION.
       01  LS-LINE-STATE           USAGE BINARY-LONG.
       LINKAGE SECTION.
       01  IN-TOKEN-COUNT          USAGE BINARY-LONG.
       01  IN-TOKEN-TABLE-GROUP.
           05  IN-TOKEN-TABLE      OCCURS 16 TIMES INDEXED BY TOKEN-IDX.
               10  IN-TOKEN        PIC X(4).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-TOKEN-COUNT
           BY REFERENCE IN-TOKEN-TABLE-GROUP.
           SET LS-LINE-STATE TO C-LINE-STATE-INITIAL
           SET RETURN-CODE TO 1
           PERFORM VARYING TOKEN-IDX FROM 1 BY 1
               UNTIL TOKEN-IDX > IN-TOKEN-COUNT

               CALL "GET-NEXT-STATE" USING
                   BY REFERENCE FUNCTION 
                       NUMVAL(IN-TOKEN-TABLE(TOKEN-IDX))
                   BY REFERENCE FUNCTION 
                       NUMVAL(IN-TOKEN-TABLE(
                           FUNCTION MAX(TOKEN-IDX - 1, 1)))
                   BY REFERENCE LS-LINE-STATE
                   RETURNING LS-LINE-STATE

               IF LS-LINE-STATE = C-LINE-STATE-UNSAFE
                   SET RETURN-CODE TO 0
                   EXIT PERFORM
           END-PERFORM
           GOBACK.
       END PROGRAM IS-LINE-SAFE.

      *> ===============================================================
      *> GET-NEXT-STATE.
      *>
      *> Returns the next state.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-NEXT-STATE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY constants IN "02".

       LOCAL-STORAGE SECTION.
       01  LS-DIFF-LEVEL            USAGE BINARY-INT.

       LINKAGE SECTION.
       01  IN-LEVEL                 USAGE BINARY-INT.
       01  IN-PREV-LEVEL            USAGE BINARY-INT.
       01  IN-LINE-STATE            USAGE BINARY-LONG.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LEVEL
           BY REFERENCE IN-PREV-LEVEL
           BY REFERENCE IN-LINE-STATE.
      *> Start out safe
           SET RETURN-CODE TO IN-LINE-STATE
           COMPUTE LS-DIFF-LEVEL = IN-LEVEL - IN-PREV-LEVEL
           EVALUATE TRUE
               WHEN IN-LINE-STATE = C-LINE-STATE-INITIAL
                   SET RETURN-CODE TO C-LINE-STATE-ONE-LEVEL
      *> Ensure that the diff is within the safe bounds:
               WHEN LS-DIFF-LEVEL < -3
                   OR LS-DIFF-LEVEL > 3
                   OR LS-DIFF-LEVEL = 0
                   SET RETURN-CODE TO C-LINE-STATE-UNSAFE
      *> If this is our second token, store whether
      *> we're increasing or decreasing.
               WHEN IN-LINE-STATE = C-LINE-STATE-ONE-LEVEL
                   EVALUATE TRUE
                       WHEN LS-DIFF-LEVEL > 0
                           SET RETURN-CODE
                               TO C-LINE-STATE-INCREASING
                       WHEN LS-DIFF-LEVEL < 0
                           SET RETURN-CODE
                               TO C-LINE-STATE-DECREASING
                       WHEN OTHER
                           SET RETURN-CODE
                               TO C-LINE-STATE-UNSAFE
                   END-EVALUATE
      *> If this is the third or later token, make sure
      *> we're continuing to increase, or decrease, consistently.
               WHEN IN-LINE-STATE = C-LINE-STATE-INCREASING
                   AND LS-DIFF-LEVEL < 0
                   SET RETURN-CODE TO C-LINE-STATE-UNSAFE
               WHEN IN-LINE-STATE = C-LINE-STATE-DECREASING
                   AND LS-DIFF-LEVEL > 0
                   SET RETURN-CODE TO C-LINE-STATE-UNSAFE
               END-EVALUATE
           GOBACK.
       END PROGRAM GET-NEXT-STATE.

      *> ===============================================================
      *> TOKENIZE-STRING.
      *>
      *> Parse a string and fill out the table of tokens.
      *> Returns the number of tokens parsed.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOKENIZE-STRING.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-TOKEN-POINTER        PIC 9(3).
       01  LS-INDEX                PIC 9(2) VALUE 1.

       LINKAGE SECTION.
       01  IN-LINE                 PIC X(100).
       01  OUT-TOKEN-TABLE-GROUP.
           05  OUT-TOKEN-TABLE     OCCURS 16 TIMES INDEXED BY TOKEN-IDX.
               10  OUT-TOKEN       PIC X(4).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LINE
           BY REFERENCE OUT-TOKEN-TABLE-GROUP.

           INITIALIZE OUT-TOKEN-TABLE-GROUP

           SET LS-TOKEN-POINTER TO 1
           PERFORM UNTIL LS-TOKEN-POINTER
               > LENGTH OF FUNCTION TRIM(IN-LINE)
               UNSTRING IN-LINE
                   DELIMITED BY " "
                   INTO OUT-TOKEN(LS-INDEX)
                   WITH POINTER LS-TOKEN-POINTER
               END-UNSTRING
               ADD 1 TO LS-INDEX
           END-PERFORM

           COMPUTE RETURN-CODE = LS-INDEX - 1

           GOBACK.
       END PROGRAM TOKENIZE-STRING.


     
      *> ===============================================================
      *> REMOVE-ITEM-FROM-TABLE
      *>
      *> Copies the contents from IN-TOKEN-TABLE-GROUP into
      *> OUT-TOKEN-TABLE-GROUP, omitting the token at
      *> IN-INDEX-TO-REMOVE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMOVE-ITEM-FROM-TABLE.
       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  TOKEN-IDX               USAGE BINARY-LONG.

       LINKAGE SECTION.
       01  IN-TOKEN-COUNT          USAGE BINARY-LONG.
       01  IN-INDEX-TO-REMOVE      USAGE BINARY-LONG.
       01  IN-TOKEN-TABLE-GROUP.
           05  IN-TOKEN-TABLE      OCCURS 16 TIMES.
               10  IN-TOKEN        PIC X(4).
       01  OUT-TOKEN-TABLE-GROUP.
           05  OUT-TOKEN-TABLE     OCCURS 16 TIMES.
               10  OUT-TOKEN       PIC X(4).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-TOKEN-COUNT
           BY REFERENCE IN-INDEX-TO-REMOVE
           BY REFERENCE IN-TOKEN-TABLE-GROUP
           BY REFERENCE OUT-TOKEN-TABLE-GROUP
           .
           PERFORM VARYING TOKEN-IDX 
               FROM 1 BY 1
               UNTIL TOKEN-IDX = IN-TOKEN-COUNT
               IF TOKEN-IDX < IN-INDEX-TO-REMOVE
               THEN
                   MOVE IN-TOKEN(TOKEN-IDX) TO OUT-TOKEN(TOKEN-IDX)
               ELSE
                   MOVE IN-TOKEN(TOKEN-IDX + 1) TO OUT-TOKEN(TOKEN-IDX)
               END-IF
           END-PERFORM.
           GOBACK.  
       END PROGRAM REMOVE-ITEM-FROM-TABLE.
