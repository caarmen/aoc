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
       PROGRAM-ID. DAY22.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(30).
       01  LS-ITERATION-COUNT        PIC 9(4).
       01  LS-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY SPACE
               INTO LS-ITERATION-COUNT LS-FILE-PATH
           END-UNSTRING

           CALL "PARSE-FILE" USING
               BY REFERENCE
               LS-ITERATION-COUNT
               LS-FILE-PATH
               .
       END PROGRAM DAY22.

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

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(47).
       01  LS-BUYER-IDX              PIC 9(4) VALUE 0.
       01  LS-SECRET-NUMBER          PIC 9(16) COMP.
       01  LS-NEW-SECRET-NUMBER      PIC 9(16) COMP.
       01  LS-TOTAL                  PIC 9(16) COMP VALUE 0.
       COPY "sequence" IN "22".

       LINKAGE SECTION.
       01  IN-ITERATION-COUNT        PIC 9(4).
       01  IN-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION USING
           BY REFERENCE
           IN-ITERATION-COUNT
           IN-FILE-PATH
           .

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       ADD 1 TO LS-BUYER-IDX
                       display ls-line
                       SET LS-SECRET-NUMBER TO FUNCTION NUMVAL(
                           LS-LINE
                       )
                       CALL "GET-NEXT-SECRET-NUMBERS" USING
                           SEQUENCE-GRP
                           LS-BUYER-IDX
                           IN-ITERATION-COUNT
                           LS-SECRET-NUMBER
                           LS-NEW-SECRET-NUMBER
                       DISPLAY LS-SECRET-NUMBER ": "
                           LS-NEW-SECRET-NUMBER
                       ADD LS-NEW-SECRET-NUMBER TO LS-TOTAL

           END-PERFORM
           CLOSE FD-DATA

           DISPLAY "Total: " LS-TOTAL
           DISPLAY "Max total price: " MAX-TOTAL-PRICE
           DISPLAY "Best sequence: " BEST-SEQUENCE-STR

           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> MIX.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIX.
       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-OUT-SECRET-NUMBER                  PIC 9(16) COMP.
       01  IN-VALUE                              PIC 9(16) COMP.

       PROCEDURE DIVISION USING BY REFERENCE
           IN-OUT-SECRET-NUMBER
           IN-VALUE.

           CALL "CBL_XOR" USING
               IN-VALUE
               IN-OUT-SECRET-NUMBER
               BY VALUE 8

           GOBACK.
       END PROGRAM MIX.

      *> ===============================================================
      *> PRUNE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUNE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-OUT-SECRET-NUMBER                  PIC 9(16) COMP.

       PROCEDURE DIVISION USING BY REFERENCE
           IN-OUT-SECRET-NUMBER.

           COMPUTE IN-OUT-SECRET-NUMBER = FUNCTION MOD(
               IN-OUT-SECRET-NUMBER,
               16777216
           )

           GOBACK.
       END PROGRAM PRUNE.

      *> ===============================================================
      *> GET-NEXT-SECRET-NUMBERS.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-NEXT-SECRET-NUMBERS.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ITERATION                          PIC 9(4).
       01  LS-CUR-SECRET-NUMBER                  PIC 9(16) COMP.
       01  LS-NEXT-SECRET-NUMBER                 PIC 9(16) COMP.
       01  LS-PREV-PRICE                         PIC 9(1) VALUE 0.
       01  LS-CUR-PRICE                          PIC 9(1).
       01  LS-DELTA                              PIC +9.
       01  LS-SEQUENCE                           PIC X(8) VALUE SPACES.
       LINKAGE SECTION.
       COPY "sequence" IN "22".
       01  IN-BUYER-IDX                          PIC 9(4) VALUE 0.
       01  IN-ITERATION-COUNT                    PIC 9(4).
       01  IN-SECRET-NUMBER                      PIC 9(16) COMP.
       01  OUT-SECRET-NUMBER                     PIC 9(16) COMP.
       PROCEDURE DIVISION USING BY REFERENCE
           SEQUENCE-GRP
           IN-BUYER-IDX
           IN-ITERATION-COUNT
           IN-SECRET-NUMBER
           OUT-SECRET-NUMBER.

           SET LS-CUR-SECRET-NUMBER TO IN-SECRET-NUMBER
           SET LS-PREV-PRICE TO FUNCTION REM(IN-SECRET-NUMBER, 10)
           PERFORM VARYING LS-ITERATION FROM 1 BY 1 UNTIL
               LS-ITERATION > IN-ITERATION-COUNT

               SET LS-NEXT-SECRET-NUMBER TO 0

               CALL "GET-NEXT-SECRET-NUMBER" USING
                   LS-CUR-SECRET-NUMBER
                   LS-NEXT-SECRET-NUMBER
               SET LS-CUR-PRICE TO FUNCTION REM(LS-NEXT-SECRET-NUMBER,
                   10)


               COMPUTE LS-DELTA = LS-CUR-PRICE - LS-PREV-PRICE
               STRING
                   LS-SEQUENCE(3:6)
                   LS-DELTA
                   INTO LS-SEQUENCE
               END-STRING
               IF LS-ITERATION >= 4
                   CALL "LOG-SEQUENCE" USING
                       SEQUENCE-GRP
                       LS-SEQUENCE
                       IN-BUYER-IDX
                       LS-CUR-PRICE
               END-IF


               SET LS-CUR-SECRET-NUMBER TO LS-NEXT-SECRET-NUMBER
               SET LS-PREV-PRICE TO LS-CUR-PRICE

           END-PERFORM

           SET OUT-SECRET-NUMBER TO LS-NEXT-SECRET-NUMBER

           GOBACK.
       END PROGRAM GET-NEXT-SECRET-NUMBERS.

      *> ===============================================================
      *> LOG-SEQUENCE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOG-SEQUENCE.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "sequence" IN "22".
       01  IN-SEQUENCE                           PIC X(8).
       01  IN-BUYER-IDX                          PIC 9(4) VALUE 0.
       01  IN-PRICE                              PIC 9(1).

       PROCEDURE DIVISION USING BY REFERENCE
           SEQUENCE-GRP
           IN-SEQUENCE
           IN-BUYER-IDX
           IN-PRICE
           .


      *> Find or create an entry for this sequence.
           SET SEQUENCE-IDX TO 1
           SEARCH SEQUENCES
               VARYING SEQUENCE-IDX
               AT END
                   ADD 1 TO SEQUENCES-SIZE
                   SET SEQUENCE-IDX TO SEQUENCES-SIZE

                   SET SEQUENCE-STR(SEQUENCE-IDX) TO IN-SEQUENCE
                   SET TOTAL-PRICE(SEQUENCE-IDX) TO 0
                   SET PRICE-SIZE(SEQUENCE-IDX) TO 0
               WHEN SEQUENCE-STR(SEQUENCE-IDX) = IN-SEQUENCE
                   CONTINUE
           END-SEARCH

      *> See if we already processed this sequence for this buyer.
           CALL "DID-BUYER-HIT-SEQUENCE" USING
               IN-BUYER-IDX
               SEQUENCES(SEQUENCE-IDX).
           IF RETURN-CODE = 0
      *> This buyer already sold a banana after this sequence.
               GOBACK
           END-IF

      *> Save this buyer's price for this sequence.
           ADD 1 TO PRICE-SIZE(SEQUENCE-IDX)
           SET BUYER-PRICE(SEQUENCE-IDX, PRICE-IDX) TO IN-PRICE
           SET BUYER-IDX(SEQUENCE-IDX, PRICE-IDX) TO IN-BUYER-IDX

      *> Keep track of the largest prices.
      *> Add this price to any prices we already stored for this
      *>    sequence.
           ADD IN-PRICE TO TOTAL-PRICE(SEQUENCE-IDX)
           SORT PRICES(SEQUENCE-IDX)

      *> Check if this is the largest price for any sequence.
      *> If so, save this largest price, as well as the sequence string.
           IF TOTAL-PRICE(SEQUENCE-IDX) > MAX-TOTAL-PRICE
               SET MAX-TOTAL-PRICE TO TOTAL-PRICE(SEQUENCE-IDX)
               SET BEST-SEQUENCE-STR TO IN-SEQUENCE
           END-IF
           .

       END PROGRAM LOG-SEQUENCE.

      *> ===============================================================
      *> DID-BUYER-HIT-SEQUENCE.
      *> Return 0 if the buyer already hit this sequence.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DID-BUYER-HIT-SEQUENCE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-BUYER-IDX                          PIC 9(4) VALUE 0.
       01  PRICE-GRP.
           05  SEQUENCE-STR      PIC X(8).
           05  TOTAL-PRICE       PIC 9(6) VALUE 0.
           05  PRICE-SIZE        PIC 9(4) VALUE 0.
           05  PRICES OCCURS 2200 TIMES
               ASCENDING KEY IS BUYER-IDX
               INDEXED BY PRICE-IDX.
               10  BUYER-IDX     PIC 9(4).
               10  BUYER-PRICE   PIC 9(1).
       PROCEDURE DIVISION USING BY REFERENCE
           IN-BUYER-IDX
           PRICE-GRP.
      *> See if we already processed this sequence for this buyer.
           SET PRICE-IDX TO 1
           SEARCH ALL PRICES
               AT END
                   SET RETURN-CODE TO 1
                   GOBACK
               WHEN BUYER-IDX(PRICE-IDX) = IN-BUYER-IDX
      *> This buyer already sold a banana after this sequence.
                   SET RETURN-CODE TO 0
                   GOBACK
           END-SEARCH
           .
       END PROGRAM DID-BUYER-HIT-SEQUENCE.

      *> ===============================================================
      *> GET-NEXT-SECRET-NUMBER.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-NEXT-SECRET-NUMBER.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-TEMP-NUMBER                        PIC 9(16) COMP.
       LINKAGE SECTION.
       01  IN-SECRET-NUMBER                      PIC 9(16) COMP.
       01  OUT-SECRET-NUMBER                     PIC 9(16) COMP.

       PROCEDURE DIVISION USING BY REFERENCE
           IN-SECRET-NUMBER
           OUT-SECRET-NUMBER.

           SET OUT-SECRET-NUMBER TO IN-SECRET-NUMBER
      *> Step 1.
      *> Calculate the result of multiplying the secret number by 64.
           COMPUTE LS-TEMP-NUMBER = OUT-SECRET-NUMBER * 64
      *> Then, mix this result into the secret number.
           CALL "MIX" USING
               OUT-SECRET-NUMBER
               LS-TEMP-NUMBER
      *> Finally, prune the secret number.
           CALL "PRUNE" USING
               OUT-SECRET-NUMBER

      *> Step 2.
      *> Calculate the result of dividing the secret number by 32.
      *> Round the result down to the nearest integer.
           COMPUTE LS-TEMP-NUMBER = OUT-SECRET-NUMBER / 32
      *> Then, mix this result into the secret number.
           CALL "MIX" USING
               OUT-SECRET-NUMBER
               LS-TEMP-NUMBER
      *> Finally, prune the secret number.
           CALL "PRUNE" USING
               OUT-SECRET-NUMBER

      *> Step 3.
      *> Calculate the result of multiplying the secret number by 2048.
           COMPUTE LS-TEMP-NUMBER = OUT-SECRET-NUMBER * 2048
      *> Then, mix this result into the secret number.
           CALL "MIX" USING
               OUT-SECRET-NUMBER
               LS-TEMP-NUMBER
      *> Finally, prune the secret number.
           CALL "PRUNE" USING
               OUT-SECRET-NUMBER

           GOBACK.
       END PROGRAM GET-NEXT-SECRET-NUMBER.
