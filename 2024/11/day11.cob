       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY11.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH                PIC X(30).
       01  LS-BLINK                    PIC 9(2).
       01  LS-ITERATION                PIC 9(2).
       01  LS-STONE-COUNT              PIC 9(18).
       COPY "stones" IN "11".

       PROCEDURE DIVISION.
           ACCEPT LS-FILE-PATH FROM COMMAND-LINE
           CALL "PARSE-INPUT" USING
               LS-FILE-PATH
               STONE-GRP
           CALL "DISPLAY-STONES" USING
               STONE-GRP

           PERFORM VARYING LS-ITERATION FROM 1 BY 1
               UNTIL LS-ITERATION > 75
               CALL "TRANSFORM-LINE" USING
                   STONE-GRP
               SET LS-STONE-COUNT TO 0
               PERFORM VARYING STONE-INDEX FROM 1 BY 1 UNTIL
                   STONE-INDEX > STONES-SIZE
                   ADD STONE-COUNT(STONE-INDEX) TO LS-STONE-COUNT
               END-PERFORM
               DISPLAY LS-ITERATION ": " LS-STONE-COUNT " stones."
           END-PERFORM

           .
       END PROGRAM DAY11.

      *> ===============================================================
      *> TRANSFORM-LINE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSFORM-LINE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-NEW-STONE-1               PIC 9(18).
       01  LS-NEW-STONE-2               PIC S9(18) VALUE -1.
       01  LS-STONE-INDEX               PIC 9(18).
       01  LS-INSERT-RESULT             PIC 9(1).
       01  LS-SIZE-START                PIC 9(18).
       01  LS-STONE-COUNT               PIC 9(18).

       LINKAGE SECTION.
       COPY "stones" IN "11".

       PROCEDURE DIVISION
           USING STONE-GRP.

           SET LS-SIZE-START TO STONES-SIZE
           PERFORM VARYING LS-STONE-INDEX FROM 1 BY 1
               UNTIL LS-STONE-INDEX > LS-SIZE-START
               CALL "TRANSFORM-STONE" USING
                   BY REFERENCE STONE(LS-STONE-INDEX)
                   LS-NEW-STONE-1
                   LS-NEW-STONE-2

                   SET LS-STONE-COUNT TO STONE-COUNT(LS-STONE-INDEX)
                   SET STONE-COUNT(LS-STONE-INDEX) TO 0
                   CALL "INSERT-STONE" USING
                       BY REFERENCE STONE-GRP
                       LS-NEW-STONE-1
                       LS-STONE-COUNT
                   IF LS-NEW-STONE-2 >= 0
                   THEN
                       CALL "INSERT-STONE" USING
                           BY REFERENCE STONE-GRP
                           LS-NEW-STONE-2
                           LS-STONE-COUNT
                       SET LS-NEW-STONE-2 TO -1
                   END-IF
           END-PERFORM

           PERFORM VARYING LS-STONE-INDEX FROM 1 BY 1
               UNTIL LS-STONE-INDEX > STONES-SIZE
               ADD STONE-NEW-COUNT(LS-STONE-INDEX) TO
                   STONE-COUNT(LS-STONE-INDEX)
               SET STONE-NEW-COUNT(LS-STONE-INDEX) TO 0
           END-PERFORM
           GOBACK.
       END PROGRAM TRANSFORM-LINE.

      *> ===============================================================
      *> TRANSFORM-STONE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSFORM-STONE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-STONE-STR                 PIC X(18) VALUE SPACES.
       01  LS-STONE-STR-LENGTH          PIC 9(2).
       01  LS-STONE-Z                   PIC Z(18).

       LINKAGE SECTION.
       01  IN-STONE                     PIC 9(18).
       01  OUT-STONE-1                  PIC 9(18).
       01  OUT-STONE-2                  PIC S9(18).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-STONE
           OUT-STONE-1
           OUT-STONE-2.

           SET OUT-STONE-2 TO -1
           MOVE IN-STONE TO LS-STONE-Z
           MOVE LS-STONE-Z TO LS-STONE-STR
           COMPUTE LS-STONE-STR-LENGTH =
               LENGTH FUNCTION TRIM(LS-STONE-STR)

           EVALUATE TRUE
               WHEN IN-STONE = 0
                   SET OUT-STONE-1 TO 1
               WHEN FUNCTION MOD(LS-STONE-STR-LENGTH, 2) = 0
                   MOVE FUNCTION TRIM(LS-STONE-STR)(
                       1:LS-STONE-STR-LENGTH/2) TO OUT-STONE-1
                   MOVE
                       FUNCTION TRIM(LS-STONE-STR)(
                           LS-STONE-STR-LENGTH/2 + 1:
                           LS-STONE-STR-LENGTH/2) TO OUT-STONE-2
               WHEN OTHER
                   COMPUTE OUT-STONE-1 = IN-STONE * 2024
           END-EVALUATE
           GOBACK.
       END PROGRAM TRANSFORM-STONE.

      *> ===============================================================
      *> INSERT-STONE.
      *> Return 1 if a new stone was inserted, 0 otherwise
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-STONE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-STONE-INDEX                  PIC 9(18).
       LINKAGE SECTION.
       COPY "stones" IN "11".
       01  IN-STONE                        PIC 9(18).
       01  IN-STONE-COUNT                  PIC 9(18).

       PROCEDURE DIVISION USING
           BY REFERENCE STONE-GRP
           IN-STONE
           IN-STONE-COUNT.

      *> See if this stone exists, if so increase its count
           SET STONE-INDEX TO 0
           SEARCH STONES
               WHEN STONE(STONE-INDEX) = IN-STONE
                   ADD IN-STONE-COUNT TO STONE-NEW-COUNT(STONE-INDEX)
                   GOBACK RETURNING 0
           END-SEARCH

      *> Look for a slot with a count of 0 and replace it with this
      *> stone.
           SET STONE-INDEX TO 0
           SEARCH STONES
               WHEN STONE-COUNT(STONE-INDEX) = 0
                   AND STONE-NEW-COUNT(STONE-INDEX) = 0
                   SET STONE(STONE-INDEX) TO IN-STONE
                   SET STONE-NEW-COUNT(STONE-INDEX) TO IN-STONE-COUNT
                   GOBACK RETURNING 0
           END-SEARCH

      *> Add a new stone.
           ADD 1 TO STONES-SIZE.
           SET STONE(STONES-SIZE) TO IN-STONE
           SET STONE-COUNT(STONES-SIZE) TO 0
           SET STONE-NEW-COUNT(STONES-SIZE) TO IN-STONE-COUNT
           GOBACK RETURNING 1.
       END PROGRAM INSERT-STONE.

      *> ===============================================================
      *> DISPLAY-STONES.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-STONES.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stones" IN "11".

       PROCEDURE DIVISION USING
           BY REFERENCE STONE-GRP.
           DISPLAY "---"
           PERFORM VARYING STONE-INDEX FROM 1 BY 1
               UNTIL STONE-INDEX > STONES-SIZE
               DISPLAY STONE(STONE-INDEX) " (" STONE-COUNT(STONE-INDEX)
                   ") "
           END-PERFORM
           DISPLAY "---"

           GOBACK.
       END PROGRAM DISPLAY-STONES.

      *> ===============================================================
      *> PARSE-INPUT.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-INPUT.

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
       01  LS-LINE-PTR               PIC 9(2).
       01  LS-STONE                  PIC 9(18).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "stones" IN "11".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           STONE-GRP.


           OPEN INPUT FD-DATA
           READ FD-DATA INTO F-FILE-RECORD
           MOVE F-FILE-RECORD TO LS-LINE
           CLOSE FD-DATA

           SET LS-LINE-PTR TO 1
           PERFORM UNTIL LS-LINE-PTR > LENGTH OF FUNCTION TRIM(LS-LINE)
               UNSTRING LS-LINE
                   DELIMITED BY " "
                   INTO LS-STONE
                   WITH POINTER LS-LINE-PTR
               END-UNSTRING
               ADD 1 TO STONES-SIZE
               SET STONE(STONES-SIZE) TO LS-STONE
               SET STONE-COUNT(STONES-SIZE) TO 1
               SET STONE-NEW-COUNT(STONES-SIZE) TO 0
           END-PERFORM
           .

       END PROGRAM PARSE-INPUT.
