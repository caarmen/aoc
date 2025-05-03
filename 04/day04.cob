       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY04.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
      *> Command-line arguments:
       01  LS-COMMAND-LINE                  PIC X(103).
       01  LS-PART                          PIC 9(1).
       01  LS-FILE-PATH                     PIC X(20).
       COPY "grid" IN "04".

       PROCEDURE DIVISION
           .
           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " "
               INTO LS-PART LS-FILE-PATH

           CALL "PARSE-GRID" USING
               BY REFERENCE LS-FILE-PATH
               BY REFERENCE GRID
               BY REFERENCE GRID-SIZE

           IF LS-PART = 1
           THEN
               CALL "PART-01" USING
               BY REFERENCE GRID
               BY REFERENCE GRID-SIZE
           ELSE
               CALL "PART-02" USING
               BY REFERENCE GRID
               BY REFERENCE GRID-SIZE
           END-IF
           GOBACK.
       END PROGRAM DAY04.


      *> ===============================================================
      *> PART-01.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PART-01.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ROW-INDEX                     PIC 9(3) USAGE COMP VALUE 1.
       01  LS-COL-INDEX                     PIC 9(3) USAGE COMP VALUE 1.
       01  LS-GRID-COL                      PIC X(140) VALUE SPACES.
       01  LS-LINE-XMAS-COUNT               PIC 9(5) USAGE COMP VALUE 0.
       01  LS-TOTAL-XMAS-COUNT              PIC 9(9) USAGE COMP VALUE 0.
       01  LS-CORNER-OFFSET                 PIC 9(3) USAGE COMP VALUE 1.
       01  LS-DIAGONAL-TLBR-1               PIC X(140) VALUE SPACES.
       01  LS-DIAGONAL-TRBL-1               PIC X(140) VALUE SPACES.
       01  LS-DIAGONAL-TLBR-2               PIC X(140) VALUE SPACES.
       01  LS-DIAGONAL-TRBL-2               PIC X(140) VALUE SPACES.
       01  LS-DIAGONALS-COUNT               PIC 9(1) USAGE COMP VALUE 4.
       LINKAGE SECTION.
       COPY "grid" IN "04".

       PROCEDURE DIVISION USING
           BY REFERENCE GRID
           BY REFERENCE GRID-SIZE.

      *> Search rows for XMAS
           PERFORM VARYING LS-ROW-INDEX FROM 1 BY 1
               UNTIL LS-ROW-INDEX > GRID-SIZE
                   CALL "COUNT-XMAS" USING
                       BY REFERENCE GRID-ROW(LS-ROW-INDEX)
                       RETURNING LS-LINE-XMAS-COUNT
                   ADD LS-LINE-XMAS-COUNT TO LS-TOTAL-XMAS-COUNT
           END-PERFORM

      *> Search columns for XMAS
           PERFORM VARYING LS-COL-INDEX FROM 1 BY 1
               UNTIL LS-COL-INDEX > GRID-SIZE
                   CALL "EXTRACT-COLUMN" USING
                       BY REFERENCE LS-COL-INDEX
                       BY REFERENCE GRID
                       BY REFERENCE GRID-SIZE
                       BY REFERENCE LS-GRID-COL
                   CALL "COUNT-XMAS" USING
                       BY REFERENCE LS-GRID-COL
                       RETURNING LS-LINE-XMAS-COUNT
                   ADD LS-LINE-XMAS-COUNT TO LS-TOTAL-XMAS-COUNT
           END-PERFORM

      *> TODO Search diagonals for XMAS
           PERFORM VARYING LS-CORNER-OFFSET FROM 0 BY 1
               UNTIL LS-CORNER-OFFSET = GRID-SIZE
               CALL "EXTRACT-DIAGONALS" USING
                   BY REFERENCE LS-CORNER-OFFSET
                   BY REFERENCE GRID
                   BY REFERENCE GRID-SIZE
                   BY REFERENCE LS-DIAGONAL-TLBR-1
                   BY REFERENCE LS-DIAGONAL-TRBL-1
                   BY REFERENCE LS-DIAGONAL-TLBR-2
                   BY REFERENCE LS-DIAGONAL-TRBL-2
                   BY REFERENCE LS-DIAGONALS-COUNT
               CALL "COUNT-XMAS" USING
                   BY REFERENCE LS-DIAGONAL-TLBR-1
                       RETURNING LS-LINE-XMAS-COUNT
                   ADD LS-LINE-XMAS-COUNT TO LS-TOTAL-XMAS-COUNT
               CALL "COUNT-XMAS" USING
                   BY REFERENCE LS-DIAGONAL-TRBL-1
                       RETURNING LS-LINE-XMAS-COUNT
                   ADD LS-LINE-XMAS-COUNT TO LS-TOTAL-XMAS-COUNT
               IF LS-DIAGONALS-COUNT = 4
                   CALL "COUNT-XMAS" USING
                       BY REFERENCE LS-DIAGONAL-TLBR-2
                           RETURNING LS-LINE-XMAS-COUNT
                       ADD LS-LINE-XMAS-COUNT TO LS-TOTAL-XMAS-COUNT
                   CALL "COUNT-XMAS" USING
                       BY REFERENCE LS-DIAGONAL-TRBL-2
                           RETURNING LS-LINE-XMAS-COUNT
                       ADD LS-LINE-XMAS-COUNT TO LS-TOTAL-XMAS-COUNT
               END-IF
           END-PERFORM
           DISPLAY "Total XMAS count: " LS-TOTAL-XMAS-COUNT
           GOBACK.
       END PROGRAM PART-01.

      *> ===============================================================
      *> PARSE-GRID.
      *>
      *> Fill the given grid with the contents of the file path.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-GRID.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO IN-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FD-DATA.
       01  F-DATA-RECORD                    PIC X(140).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                          PIC X(140).
       01  LS-ROW-INDEX                     PIC 9(3) USAGE COMP VALUE 1.

       LINKAGE SECTION.
       01  IN-FILE-PATH                     PIC X(20).
       COPY "grid" in "04".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           BY REFERENCE GRID
           BY REFERENCE GRID-SIZE.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO LS-LINE
               AT END
                   EXIT PERFORM
               NOT AT END
                   IF GRID-SIZE = 0
                   THEN
                       COMPUTE GRID-SIZE = LENGTH OF
                           FUNCTION TRIM(LS-LINE)
                   END-IF
                   MOVE LS-LINE TO GRID-ROW(LS-ROW-INDEX)
                   ADD 1 TO LS-ROW-INDEX
           END-PERFORM
           CLOSE FD-DATA

           GOBACK.
       END PROGRAM PARSE-GRID.

      *> ===============================================================
      *> EXTRACT-COLUMN
      *>
      *> Return the given column of the grid.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACT-COLUMN.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ROW-INDEX                     PIC 9(3) USAGE COMP VALUE 1.
       LINKAGE SECTION.
       01  IN-COL-INDEX                     PIC 9(3) USAGE COMP VALUE 1.
       COPY "grid" IN "04".
       01  OUT-GRID-COL                     PIC X(140) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-COL-INDEX
           BY REFERENCE GRID
           BY REFERENCE GRID-SIZE
           BY REFERENCE OUT-GRID-COL.

           PERFORM VARYING LS-ROW-INDEX FROM 1 BY 1
               UNTIL LS-ROW-INDEX > GRID-SIZE
               MOVE GRID-CELL(LS-ROW-INDEX, IN-COL-INDEX)
                   TO OUT-GRID-COL(LS-ROW-INDEX:1)
           END-PERFORM


           GOBACK.
       END PROGRAM EXTRACT-COLUMN.

      *> ===============================================================
      *> EXTRACT-DIAGONALS
      *>
      *> Fill in diagonals with the offset from the corners.
      *>
      *> The following diagonals are returned.
      *> -TLBR indicates "from the top left to the bottom right"
      *> -TRBL indicates "from the top right to the bottom left"
      *>
      *>
      *>  Example: corner offset = 1:
      *>
      *>  TLBR1   TRBL1   TLBR2   TRBL2
      *>
      *>  .X...   ...X.   .....   .....
      *>  ..X..   ..X..   X....   ....X
      *>  ...X.   .X...   .X...   ...X.
      *>  ....X   X....   ..X..   ..X..
      *>  .....   .....   ...X.   .X...
      *>
      *>
      *>  Special case: corner offsset = 0:
      *>
      *>  TLBR1   TRBL1   TLBR2   TRBL2
      *>
      *>  X....   ....X
      *>  .X...   ...X.
      *>  ..X..   ..X..    N/A     N/A
      *>  ...X.   .X...
      *>  ....X   X....
      *>
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACT-DIAGONALS.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ROW-START-INDEX               PIC 9(3) USAGE COMP VALUE 1.
       01  LS-ROW-INDEX                     PIC 9(3) USAGE COMP VALUE 1.
       01  LS-DIAGONAL-INDEX                PIC 9(3) USAGE COMP VALUE 1.
       LINKAGE SECTION.
       01  IN-CORNER-OFFSET                 PIC 9(3) USAGE COMP VALUE 1.
       COPY "grid" IN "04".
       01  OUT-DIAGONAL-TLBR-1              PIC X(140) VALUE SPACES.
       01  OUT-DIAGONAL-TRBL-1              PIC X(140) VALUE SPACES.
       01  OUT-DIAGONAL-TLBR-2              PIC X(140) VALUE SPACES.
       01  OUT-DIAGONAL-TRBL-2              PIC X(140) VALUE SPACES.
       01  OUT-DIAGONALS-COUNT              PIC 9(1) USAGE COMP VALUE 4.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-CORNER-OFFSET
           BY REFERENCE GRID
           BY REFERENCE GRID-SIZE
           BY REFERENCE OUT-DIAGONAL-TLBR-1
           BY REFERENCE OUT-DIAGONAL-TRBL-1
           BY REFERENCE OUT-DIAGONAL-TLBR-2
           BY REFERENCE OUT-DIAGONAL-TRBL-2
           BY REFERENCE OUT-DIAGONALS-COUNT.


      *>    TLBR1
      *>
      *>    .X...
      *>    ..X..
      *>    ...X.
      *>    ....X
      *>    .....

           SET LS-ROW-START-INDEX TO 1
           SET LS-DIAGONAL-INDEX TO 1
           MOVE SPACES TO OUT-DIAGONAL-TLBR-1
           PERFORM VARYING LS-ROW-INDEX FROM LS-ROW-START-INDEX BY 1
               UNTIL LS-ROW-INDEX > GRID-SIZE - IN-CORNER-OFFSET
               MOVE GRID-CELL(
                   LS-ROW-INDEX, LS-ROW-INDEX + IN-CORNER-OFFSET
               ) TO OUT-DIAGONAL-TLBR-1(LS-DIAGONAL-INDEX:1)
               ADD 1 TO LS-DIAGONAL-INDEX
           END-PERFORM

      *>    TRBL1
      *>
      *>    ...X.
      *>    ..X..
      *>    .X...
      *>    X....
      *>    .....

           SET LS-ROW-START-INDEX TO 1
           SET LS-DIAGONAL-INDEX TO 1
           MOVE SPACES TO OUT-DIAGONAL-TRBL-1
           PERFORM VARYING LS-ROW-INDEX FROM LS-ROW-START-INDEX BY 1
               UNTIL LS-ROW-INDEX > GRID-SIZE - IN-CORNER-OFFSET
               MOVE GRID-CELL(
                   LS-ROW-INDEX,
                   GRID-SIZE - LS-ROW-INDEX - IN-CORNER-OFFSET + 1
               ) TO OUT-DIAGONAL-TRBL-1(LS-DIAGONAL-INDEX:1)
               ADD 1 TO LS-DIAGONAL-INDEX
           END-PERFORM

           IF IN-CORNER-OFFSET = 0
           THEN
               SET OUT-DIAGONALS-COUNT TO 2
               GOBACK
           END-IF

      *>    TLBR2
      *>
      *>    .....
      *>    X....
      *>    .X...
      *>    ..X..
      *>    ...X.

           COMPUTE LS-ROW-START-INDEX = 1 + IN-CORNER-OFFSET
           SET LS-DIAGONAL-INDEX TO 1
           MOVE SPACES TO OUT-DIAGONAL-TLBR-2
           PERFORM VARYING LS-ROW-INDEX FROM LS-ROW-START-INDEX BY 1
               UNTIL LS-ROW-INDEX > GRID-SIZE
               MOVE GRID-CELL(
                   LS-ROW-INDEX,
                   LS-ROW-INDEX - IN-CORNER-OFFSET
               ) TO OUT-DIAGONAL-TLBR-2(LS-DIAGONAL-INDEX:1)
               ADD 1 TO LS-DIAGONAL-INDEX
           END-PERFORM

      *>    TRBL2
      *>
      *>    .....
      *>    ....X
      *>    ...X.
      *>    ..X..
      *>    .X...
           COMPUTE LS-ROW-START-INDEX = 1 + IN-CORNER-OFFSET
           SET LS-DIAGONAL-INDEX TO 1
           MOVE SPACES TO OUT-DIAGONAL-TRBL-2
           PERFORM VARYING LS-ROW-INDEX FROM LS-ROW-START-INDEX BY 1
               UNTIL LS-ROW-INDEX > GRID-SIZE
               MOVE GRID-CELL(
                   LS-ROW-INDEX,
                   GRID-SIZE - LS-ROW-INDEX + IN-CORNER-OFFSET + 1
               ) TO OUT-DIAGONAL-TRBL-2(LS-DIAGONAL-INDEX:1)
               ADD 1 TO LS-DIAGONAL-INDEX
           END-PERFORM

           SET OUT-DIAGONALS-COUNT TO 4
           GOBACK.

       END PROGRAM EXTRACT-DIAGONALS.

      *> ===============================================================
      *> COUNT-XMAS.
      *>
      *> Return the number of occurrences of XMAS or SAMX in the given
      *> text.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-XMAS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-XMAS                           CONSTANT "XMAS".
       LOCAL-STORAGE SECTION.
       01  LS-INSPECT-TALLY                 PIC 9(3) USAGE COMP VALUE 0.
       01  LS-COUNT                         PIC 9(3) USAGE COMP VALUE 0.

       LINKAGE SECTION.
       01  IN-TEXT                          PIC X(140).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-TEXT.

      *> Look for the word forward in the text.
           INSPECT IN-TEXT TALLYING LS-INSPECT-TALLY FOR ALL C-XMAS

           ADD LS-INSPECT-TALLY TO LS-COUNT
           SET LS-INSPECT-TALLY TO 0

      *> Look for the word backward in the text.
           INSPECT FUNCTION REVERSE(IN-TEXT)
               TALLYING LS-INSPECT-TALLY FOR ALL C-XMAS
           ADD LS-INSPECT-TALLY TO LS-COUNT

           MOVE LS-COUNT TO RETURN-CODE

           GOBACK.
       END PROGRAM COUNT-XMAS.


      *> ===============================================================
      *> PART-02
      *>
      *> Return the number of occurrences of the X-MAS cross in the
      *> given text.
      *>
      *> We look for these patterns:
      *>
      *> M M  M S  S S  S M
      *>  A    A    A    A
      *> S S  M S  M M  S M
      *>
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PART-02.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-COUNT                         PIC 9(9) USAGE COMP VALUE 0.
       01  LS-ROW-INDEX                     PIC 9(3) USAGE COMP VALUE 1.
       01  LS-COL-INDEX                     PIC 9(3) USAGE COMP VALUE 1.

       LINKAGE SECTION.
       COPY "grid" IN "04".

       PROCEDURE DIVISION USING
           BY REFERENCE GRID
           BY REFERENCE GRID-SIZE.

           PERFORM VARYING
               LS-ROW-INDEX FROM 2 BY 1 UNTIL LS-ROW-INDEX = GRID-SIZE
               AFTER
               LS-COL-INDEX FROM 2 BY 1 UNTIL LS-COL-INDEX = GRID-SIZE

               IF GRID-CELL(LS-ROW-INDEX,LS-COL-INDEX) = "A"
               THEN
                   EVALUATE TRUE
      *> M M
      *>  A
      *> S S
                       WHEN
                           GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX - 1)
                           = "M"
                       AND GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX + 1)
                           = "M"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX - 1)
                           = "S"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX + 1)
                           = "S"
                           ADD 1 TO LS-COUNT
      *> M S
      *>  A
      *> M S
                       WHEN
                           GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX - 1)
                           = "M"
                       AND GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX + 1)
                           = "S"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX - 1)
                           = "M"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX + 1)
                           = "S"
                           ADD 1 TO LS-COUNT
      *> S S
      *>  A
      *> M M
                       WHEN
                           GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX - 1)
                           = "S"
                       AND GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX + 1)
                           = "S"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX - 1)
                           = "M"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX + 1)
                           = "M"
                           ADD 1 TO LS-COUNT
      *> S M
      *>  A
      *> S M
                       WHEN
                           GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX - 1)
                           = "S"
                       AND GRID-CELL(LS-ROW-INDEX - 1, LS-COL-INDEX + 1)
                           = "M"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX - 1)
                           = "S"
                       AND GRID-CELL(LS-ROW-INDEX + 1, LS-COL-INDEX + 1)
                           = "M"
                           ADD 1 TO LS-COUNT

               END-IF
           END-PERFORM
           DISPLAY "X-MAS count " LS-COUNT
           GOBACK.
       END PROGRAM PART-02.
