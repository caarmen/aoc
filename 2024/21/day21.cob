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
       PROGRAM-ID. DAY21.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD             PIC X(47).

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(100).
       01  LS-MODE                   PIC X(1).
       01  LS-LINE                   PIC X(100).
       01  LS-INPUT                  PIC X(100).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-KP-IDX                 PIC 9(1) VALUE 1.
       01  LS-KEYPAD-COUNT           PIC 9(2).
       01  LS-COMPLEXITY             PIC 9(16).
       01  LS-TOTAL-COMPLEXITY       PIC 9(16) VALUE 0.
       01  LS-SHORTEST-SEQUENCE      PIC X(100).
       COPY "keypad" IN "21/src".

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY SPACE
               INTO LS-MODE LS-KEYPAD-COUNT LS-INPUT
           END-UNSTRING


           EVALUATE LS-MODE
               WHEN ">"

      *> Init the first directional keypads
                   PERFORM VARYING LS-KP-IDX FROM 1 BY 1 UNTIL
                       LS-KP-IDX = LS-KEYPAD-COUNT
                       CALL "INIT-DIRECTIONAL-KEYPAD" USING BY REFERENCE
                           KP-GRP
                           LS-KP-IDX
                   END-PERFORM

      *> Init the last numeric keypad
                   SET LS-KP-IDX TO LS-KEYPAD-COUNT
                   CALL "INIT-NUMERIC-KEYPAD" USING BY REFERENCE
                       KP-GRP
                       LS-KP-IDX

                   SET LS-KP-IDX TO 1
                   CALL "USE-KEYPAD-SEQUENCE" USING
                       KP-GRP
                       LS-KP-IDX
                       LS-INPUT
                   PERFORM VARYING LS-KP-IDX FROM 1 BY 1
                       UNTIL LS-KP-IDX > LS-KEYPAD-COUNT
                       CALL "DISPLAY-KEYPAD" USING BY REFERENCE
                           KP-GRP
                           LS-KP-IDX
                   END-PERFORm

               WHEN "<"

                   SET LS-FILE-PATH TO FUNCTION TRIM(LS-INPUT)

                   OPEN INPUT FD-DATA
                   PERFORM UNTIL EXIT
                       READ FD-DATA INTO F-FILE-RECORD
                           AT END
                               EXIT PERFORM
                           NOT AT END
                               MOVE F-FILE-RECORD TO LS-LINE
                               CALL "CALCULATE-COMPLEXITY" USING
                                   LS-KEYPAD-COUNT
                                   LS-LINE
                                   LS-COMPLEXITY
                               ADD LS-COMPLEXITY TO LS-TOTAL-COMPLEXITY
                   END-PERFORM
                   DISPLAY "Total complexity: " LS-TOTAL-COMPLEXITY
                   CLOSE FD-DATA

           END-EVALUATE

           .
       END PROGRAM DAY21.

      *> ===============================================================
      *> CALCULATE-COMPLEXITY
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-COMPLEXITY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-KP-IDX                             PIC 9(1) VALUE 3.
       01  LS-KP-TYPE                            PIC 9(1).
       01  LS-TARGET-SEQUENCE                    PIC X(100).
       01  LS-SOURCE-SEQUENCE-LENGTH             PIC 9(14).
       LINKAGE SECTION.
       01  IN-KEYPAD-COUNT                       PIC 9(2).
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-COMPLEXITY                        PIC 9(16).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KEYPAD-COUNT
           IN-TARGET-SEQUENCE
           OUT-COMPLEXITY.

           display "calculate complexity '"
           function trim(in-target-sequence) "'"
           SET LS-TARGET-SEQUENCE TO IN-TARGET-SEQUENCE
           SET LS-KP-TYPE TO C-TYPE-NUMERIC

           CALL "GET-SHORTEST-INPUT-SEQUENCE-LEN" USING
               BY REFERENCE
               IN-KEYPAD-COUNT
               LS-TARGET-SEQUENCE
               LS-SOURCE-SEQUENCE-LENGTH

           DISPLAY "Shortest sequence length for "
               function trim(ls-target-sequence) ": "
               LS-SOURCE-SEQUENCE-LENGTH

      *> Find the shortest input sequence now
           COMPUTE OUT-COMPLEXITY =
               LS-SOURCE-SEQUENCE-LENGTH *
               FUNCTION NUMVAL(IN-TARGET-SEQUENCE)
           DISPLAY "Complexity "  LS-SOURCE-SEQUENCE-LENGTH
               " * " FUNCTION NUMVAL(IN-TARGET-SEQUENCE)
               " = " OUT-COMPLEXITY

           GOBACK.
       END PROGRAM CALCULATE-COMPLEXITY.

      *> ===============================================================
      *> GET-SHORTEST-INPUT-SEQUENCE-LEN
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-SHORTEST-INPUT-SEQUENCE-LEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PUSH-TO-STACK
           FUNCTION POP-STACK.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       COPY "stack" IN "21/src".
       COPY "cache" IN "21/src".
       01  LS-POP-RESULT                         PIC 9(1).
       01  LS-PUSH-RESULT                        PIC 9(1).

      *> The data we push to the stack:
       01  LS-STACK-ITEM-LEVEL                   PIC 9(2).
       01  LS-STACK-ITEM-HISTORY                 PIC X(100).
       01  LS-STACK-ITEM-START-KEY               PIC X(1).
       01  LS-STACK-ITEM-END-KEY                 PIC X(1).

      *> Data needed to backtrack up to parents:
       01  LS-NEXT-HISTORY                       PIC X(100).
       01  LS-HISTORY-IDX                        PIC 9(2).
       01  LS-HISTORY-DEPTH                      PIC 9(2).
       01  LS-PARENT-LEVEL                       PIC 9(2).

      *> Data for cache lookups:
       01  LS-CACHE-KEY                          PIC X(4).
       01  LS-CACHE-VALUE                        PIC 9(14).
       01  LS-CACHE-RESULT                       PIC 9(1).

      *> Data for sequences of keys at
      *> a given iteration:
       01  LS-TARGET-IDX                         PIC 9(3).
       01  LS-TARGET-SEQUENCE                    PIC X(100).
       01  LS-TARGET-SEQUENCE-LENGTH             PIC 9(6).
       01  LS-SOURCE-SEQUENCE                    PIC X(100).
       01  LS-SOURCE-SEQUENCE-LENGTH             PIC 9(14).

       01  LS-KP-TYPE                            PIC 9(1).

       LINKAGE SECTION.
       01  IN-KEYPAD-COUNT                       PIC 9(2).
       01  IN-TARGET-SEQUENCE                    PIC X(100).
       01  OUT-ORIGIN-SEQUENCE-LENGTH            PIC 9(14) VALUE 0.

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KEYPAD-COUNT
           IN-TARGET-SEQUENCE
           OUT-ORIGIN-SEQUENCE-LENGTH.

      *> Start at the top with the target sequence given as input.
           SET LS-TARGET-SEQUENCE TO IN-TARGET-SEQUENCE
           SET LS-STACK-ITEM-LEVEL TO IN-KEYPAD-COUNT

           PERFORM PUSH-TARGET-SEQUENCE

           PERFORM UNTIL STACK-SIZE = 0

               SET LS-POP-RESULT TO POP-STACK(
                   STACK-GRP,
                   LS-STACK-ITEM-LEVEL,
                   LS-STACK-ITEM-HISTORY,
                   LS-STACK-ITEM-START-KEY,
                   LS-STACK-ITEM-END-KEY
               )

      *> If we've already seen a subtree with this given
      *> start key & end key as the root of the subtree, with this root
      *> at the same level we're at now, we don't need to retraverse
      *> this tree. Get the total source sequence for this subtree, and add it
      *> to this node's parents, in the cache.

               STRING LS-STACK-ITEM-LEVEL
                   LS-STACK-ITEM-START-KEY
                   LS-STACK-ITEM-END-KEY
                   INTO LS-CACHE-KEY
               END-STRING

               CALL "GET-FROM-CACHE" USING
                   CACHE-GRP
                   LS-CACHE-KEY
                   LS-SOURCE-SEQUENCE-LENGTH
                   LS-CACHE-RESULT
               IF LS-CACHE-RESULT = 0
                   COMPUTE LS-HISTORY-DEPTH =
                       LENGTH OF FUNCTION TRIM(
                           LS-STACK-ITEM-HISTORY
                       ) / 2
                   COMPUTE OUT-ORIGIN-SEQUENCE-LENGTH =
                       OUT-ORIGIN-SEQUENCE-LENGTH + 
                       LS-SOURCE-SEQUENCE-LENGTH
                   PERFORM BUBBLE-UP-PARENT-CACHE
               ELSE

      *> This is a node we haven't processed yet.
      *> Calculate the source sequence to produce its target sequence.
                   IF LS-STACK-ITEM-LEVEL = IN-KEYPAD-COUNT
                       SET LS-KP-TYPE TO C-TYPE-NUMERIC
                   ELSE
                       SET LS-KP-TYPE TO C-TYPE-DIRECTIONAL
                   END-IF
                   CALL "GET-STEP-PATH" USING
                       LS-KP-TYPE
                       LS-STACK-ITEM-START-KEY
                       LS-STACK-ITEM-END-KEY
                       LS-SOURCE-SEQUENCE

      *> We're not at the bottom yet.
      *> The source sequence we just calculated now becomes our next
      *> target sequence.
      *> Push all the keys in the this new target sequence to the stack.
                   IF LS-STACK-ITEM-LEVEL > 1
                       ADD -1 TO LS-STACK-ITEM-LEVEL
                       SET LS-TARGET-SEQUENCE TO LS-SOURCE-SEQUENCE
                       PERFORM PUSH-TARGET-SEQUENCE
                   ELSE
      *> We're at the bottom level. Add the size of the source sequence
      *> at this location to the cache, and bubble up the cumulative
      *> sequence sizes to the parent cache items
                       STRING LS-STACK-ITEM-LEVEL
                           LS-STACK-ITEM-START-KEY
                           LS-STACK-ITEM-END-KEY
                           INTO LS-CACHE-KEY
                       END-STRING
                       SET LS-SOURCE-SEQUENCE-LENGTH TO LENGTH OF
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE)
                       CALL "ADD-TO-CACHE" USING
                           CACHE-GRP
                           LS-CACHE-KEY
                           LS-SOURCE-SEQUENCE-LENGTH
                           LS-CACHE-RESULT
      *> Create/update the cache entries of the ancestor nodes
                       PERFORM BUBBLE-UP-PARENT-CACHE
                       COMPUTE OUT-ORIGIN-SEQUENCE-LENGTH =
                           OUT-ORIGIN-SEQUENCE-LENGTH + 
                           LENGTH OF FUNCTION TRIM(
                               LS-SOURCE-SEQUENCE)
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ===============================================================
      *> BUBBLE-UP-PARENT-CACHE.
      *> Add the length of the source sequence, for our current node,
      *> to the already calculated sequence length of the parent nodes.
      *> ===============================================================
       BUBBLE-UP-PARENT-CACHE.
      *> The history in the stack is like A3^A<A:
      *> For the case of 26 total levels, this example indicates a node
      *> at level 24.
      *> Level 26: 3 is the first numeric input (we start from A).
      *> Level 25: On the last directional keypad, You need to do ^A to reach 3.
      *> Level 24: On the second-to-last directional keypad,
      *>     you need to do <A to reach ^.

      *> The cache-key is like 25^A:
      *> This example indicates a node for a directional sequence from 
      *> '^' to 'A', at the 25th level.

           COMPUTE LS-HISTORY-DEPTH =
               LENGTH OF FUNCTION TRIM(
                   LS-STACK-ITEM-HISTORY
               ) / 2
      *> In this example of a history of A3^A<A: LS-HISTORY-DEPTH is 6/2 = 3
           PERFORM VARYING LS-HISTORY-IDX FROM 1 BY 1
               UNTIL LS-HISTORY-IDX > LS-HISTORY-DEPTH
      *> The parent levels will go from 26 to 24.
               COMPUTE LS-PARENT-LEVEL = IN-KEYPAD-COUNT -
                   LS-HISTORY-IDX + 1
      *> The cache key at level 25 would be 25^A
               STRING 
                   LS-PARENT-LEVEL
                   LS-STACK-ITEM-HISTORY(
                       ((LS-HISTORY-IDX - 1) * 2 + 1):2
                   )
                   INTO LS-CACHE-KEY
               END-STRING
               CALL "GET-FROM-CACHE" USING
                   CACHE-GRP
                   LS-CACHE-KEY
                   LS-CACHE-VALUE
                   LS-CACHE-RESULT
      *> If this parent is already in the cache, our calculated sequence
      *> length to whatever was already stored there.
               IF LS-CACHE-RESULT = 0
                   ADD LS-SOURCE-SEQUENCE-LENGTH TO
                       LS-CACHE-VALUE
                   CALL "UPDATE-CACHE" USING
                       CACHE-GRP
                       LS-CACHE-KEY
                       LS-CACHE-VALUE
               ELSE
      *> The parent isn't in the cache yet: add it.
                   CALL "ADD-TO-CACHE" USING
                       CACHE-GRP
                       LS-CACHE-KEY
                       LS-SOURCE-SEQUENCE-LENGTH
                       LS-CACHE-RESULT
               END-IF
           END-PERFORM
           .


      *> ===============================================================
      *> PUSH-TARGET-SEQUENCE.
      *> Push all segments of the given target sequence to the stack.
      *> These are segments for which we will need to calculate the
      *> source sequences.
      *> ===============================================================
       PUSH-TARGET-SEQUENCE.

      *> Say we have a puzzle input starting at 3 (we implicitly start at A first).
      *> The level 25 directional keypad needs ^A.
      *> The level 24 directional keypad needs <A>A.
      *> The level 23 directional keypad needs v<<A >>^A vA ^A.

      *> Say we're currently at level 24, with the <A segment.
      *> The history so far is A3^A:
      *>   Level 26: A3
      *>   Level 25: ^A
      *> We calculated that at level 23, we'd need v<<A to make this <A
      *> segment.

      *> In this case, the source sequence we calculated above was v<<A.
      *> This source sequence has now become our target sequence, to add
      *> to the stack.
           SET LS-TARGET-SEQUENCE-LENGTH TO
               LENGTH OF FUNCTION TRIM(LS-TARGET-SEQUENCE)
      *> LS-TARGET-SEQUENCE-LENGTH = 4 (v<<A)
           SET LS-NEXT-HISTORY TO SPACE
           STRING FUNCTION TRIM(LS-STACK-ITEM-HISTORY)
               LS-STACK-ITEM-START-KEY
               LS-STACK-ITEM-END-KEY
               INTO LS-NEXT-HISTORY
           END-STRING
      *> LS-NEXT-HISTORY = A3^A + < + A = A3^A<A
           PERFORM VARYING LS-TARGET-IDX FROM LS-TARGET-SEQUENCE-LENGTH
               BY -1 UNTIL LS-TARGET-IDX = 1
               SET LS-STACK-ITEM-END-KEY TO
                   LS-TARGET-SEQUENCE(LS-TARGET-IDX:1)
               SET LS-STACK-ITEM-START-KEY TO
                   LS-TARGET-SEQUENCE(LS-TARGET-IDX - 1 :1)
      *> Example, for the << segment in v<<A, we'll push:
      *>   LS-STACK-ITEM-LEVEL = 23
      *>   LS-NEXT-HISTORY = A3^A<A
      *>   LS-STACK-ITEM-START-KEY = <
      *>   LS-STACK-ITEM-END-KEY = <
               SET LS-PUSH-RESULT TO PUSH-TO-STACK( 
                   STACK-GRP,
                   LS-STACK-ITEM-LEVEL,
                   LS-NEXT-HISTORY,
                   LS-STACK-ITEM-START-KEY,
                   LS-STACK-ITEM-END-KEY
               )
           END-PERFORM
      *> Special case for the beginning of all of our sequences:
      *> Push a pair starting from "A".
      *> Example, for the v at the beginning  v<<A, we'll push:
      *>   LS-STACK-ITEM-LEVEL = 23
      *>   LS-NEXT-HISTORY = A3^A<A
      *>   LS-STACK-ITEM-START-KEY = A
      *>   LS-STACK-ITEM-END-KEY = v
           SET LS-STACK-ITEM-END-KEY TO LS-TARGET-SEQUENCE(1:1)
           SET LS-STACK-ITEM-START-KEY TO "A"
           SET LS-PUSH-RESULT TO PUSH-TO-STACK( 
               STACK-GRP,
               LS-STACK-ITEM-LEVEL,
               LS-NEXT-HISTORY,
               LS-STACK-ITEM-START-KEY,
               LS-STACK-ITEM-END-KEY
           )
           .
       END PROGRAM GET-SHORTEST-INPUT-SEQUENCE-LEN.

      *> ===============================================================
      *> GET-STEP-PATH.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-STEP-PATH.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-LEFT                               PIC X(3) VALUE "<<<".
       01  C-RIGHT                              PIC X(3) VALUE ">>>".
       01  C-TOP                                PIC X(3) VALUE "^^^".
       01  C-BOTTOM                             PIC X(3) VALUE "vvv".
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-TARGET-START-ROW                  PIC 9(1).
       01  LS-TARGET-START-COL                  PIC 9(1).
       01  LS-TARGET-END-ROW                    PIC 9(1).
       01  LS-TARGET-END-COL                    PIC 9(1).
       01  LS-TARGET-DELTA-ROW                  PIC S9(1).
       01  LS-TARGET-DELTA-COL                  PIC S9(1).
       01  LS-SOURCE-SEQUENCE-UP                PIC X(3) VALUE SPACES.
       01  LS-SOURCE-SEQUENCE-RIGHT             PIC X(3) VALUE SPACES.
       01  LS-SOURCE-SEQUENCE-DOWN              PIC X(3) VALUE SPACES.
       01  LS-SOURCE-SEQUENCE-LEFT              PIC X(3) VALUE SPACES.
       LINKAGE SECTION.
       01  IN-KP-TYPE                           PIC 9(1).
       01  IN-TARGET-START-KEY                  PIC X(1).
       01  IN-TARGET-END-KEY                    PIC X(1).
       01  OUT-SOURCE-SEQUENCE                  PIC x(100).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KP-TYPE
           IN-TARGET-START-KEY
           IN-TARGET-END-KEy
           OUT-SOURCE-SEQUENCE.

           SET OUT-SOURCE-SEQUENCE TO SPACES

           CALL "GET-POSITION" USING
               IN-KP-TYPE
               IN-TARGET-START-KEY
               LS-TARGET-START-ROW
               LS-TARGET-START-COL

           CALL "GET-POSITION" USING
               IN-KP-TYPE
               IN-TARGET-END-KEY
               LS-TARGET-END-ROW
               LS-TARGET-END-COL

           COMPUTE LS-TARGET-DELTA-ROW = 
               LS-TARGET-END-ROW - LS-TARGET-START-ROW
           COMPUTE LS-TARGET-DELTA-COL = 
               LS-TARGET-END-COL - LS-TARGET-START-COL

           EVALUATE LS-TARGET-DELTA-ROW
               WHEN < 0
                   SET LS-SOURCE-SEQUENCE-UP TO
                       C-TOP(1: - LS-TARGET-DELTA-ROW)
               WHEN > 0
                   SET LS-SOURCE-SEQUENCE-DOWN TO
                       C-BOTTOM(1: LS-TARGET-DELTA-ROW)
           END-EVALUATE
           EVALUATE LS-TARGET-DELTA-COL
               WHEN < 0
                   SET LS-SOURCE-SEQUENCE-LEFT TO
                       C-LEFT(1: - LS-TARGET-DELTA-COL)
               WHEN > 0
                   SET LS-SOURCE-SEQUENCE-RIGHT TO
                       C-RIGHT(1: LS-TARGET-DELTA-COL)
           END-EVALUATE

           EVALUATE IN-KP-TYPE
      *> For the numeric keypad, it generally requires fewer source
      *> directional sequences to go
      *> left->down, left->up, down->right, up->right.

      *> The exception is when this would make us go through
      *> the bottom left empty space. In this case, we must go
      *> up->left and down->right instead.
               WHEN C-TYPE-NUMERIC
                   IF (
                           LS-TARGET-START-COL = 1
                           AND LS-TARGET-END-COL > 1
                           AND LS-TARGET-END-ROW = 4
                       ) OR (
                           LS-TARGET-START-ROW = 4
                           AND LS-TARGET-END-COL = 1
                           AND LS-TARGET-END-ROW < 4
                       ) 
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   ELSE
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   END-IF
      *> For the directional keypad, we have a similar concept.
      *> It generally requires fewer source sequences to go
      *> left->up, left->down, down->right, up->right.

      *> The exception is when this would make us go through
      *> the top left empty space. In this case, we must go
      *> down->left and right->up instead.
               WHEN C-TYPE-DIRECTIONAL
                   IF (
                           LS-TARGET-START-ROW = 1
                           AND LS-TARGET-END-COL = 1
                       ) OR (
                           LS-TARGET-START-COL = 1
                           AND LS-TARGET-END-ROW = 1
                       ) 
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   ELSE
                       STRING
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-LEFT)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-DOWN)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-UP)
                           FUNCTION TRIM(LS-SOURCE-SEQUENCE-RIGHT)
                           "A"
                           INTO OUT-SOURCE-SEQUENCE
                       END-STRING
                   END-IF

           END-EVALUATE
           .
       END PROGRAM GET-STEP-PATH.


      *> ===============================================================
      *> GET-POSITION.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-POSITION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "21/src".
       LOCAL-STORAGE SECTION.
       01  LS-TEMP PIC 9(1).
       LINKAGE SECTION.
       01  IN-KP-TYPE                           PIC 9(1).
       01  IN-KEY                               PIC X(1).
       01  OUT-ROW                              PIC 9(1).
       01  OUT-COL                              PIC 9(1).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-KP-TYPE
           IN-KEY
           OUT-ROW
           OUT-COL.

           EVALUATE IN-KEY ALSO IN-KP-TYPE
      *> Directional
               WHEN "^" ALSO ANY
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 2
               WHEN "A" ALSO C-TYPE-DIRECTIONAL
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 3
               WHEN "<" ALSO ANY
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 1
               WHEN "v" ALSO ANY
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 2
               WHEN ">" ALSO ANY
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 3
      *> Numeric
      *> Note, I tried to do this with math instead,
      *> using division and modulo, but gave up after
      *> having too many bugs :)
               WHEN "7" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 1
               WHEN "8" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 2
               WHEN "9" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 1
                   SET OUT-COL TO 3
               WHEN "4" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 1
               WHEN "5" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 2
               WHEN "6" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 2
                   SET OUT-COL TO 3
               WHEN "1" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 3
                   SET OUT-COL TO 1
               WHEN "2" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 3
                   SET OUT-COL TO 2
               WHEN "3" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 3
                   SET OUT-COL TO 3
               WHEN "0" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 4
                   SET OUT-COL TO 2
               WHEN "A" ALSO C-TYPE-NUMERIC
                   SET OUT-ROW TO 4
                   SET OUT-COL TO 3
           END-EVALUATE
           .
       END PROGRAM GET-POSITION.
