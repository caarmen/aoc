       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY09.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE                       PIC X(30).
       01  LS-FILE-PATH                          PIC X(20000).
       01  LS-PART                               PIC X(1).
       01  LS-CHECKSUM-TOTAL                     PIC 9(18) COMP VALUE 0.
       COPY "disk-map" IN "09".

       PROCEDURE DIVISION.
           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE
           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " "
               INTO LS-PART, LS-FILE-PATH
           END-UNSTRING

           CALL "PARSE-DISK-MAP" USING
               BY REFERENCE LS-FILE-PATH
               BY REFERENCE DISK-MAP-GRP

           EVALUATE LS-PART
               WHEN "1"
                   CALL "DEFRAGMENT-DISK-1" USING
                       BY REFERENCE DISK-MAP-GRP
               WHEN "2"
                   CALL "DEFRAGMENT-DISK-2" USING
                       BY REFERENCE DISK-MAP-GRP
           END-EVALUATE

           CALL "CALCULATE-CHECKSUM" USING
               BY REFERENCE DISK-MAP-GRP
               BY REFERENCE LS-CHECKSUM-TOTAL

           DISPLAY "checksum: " LS-CHECKSUM-TOTAL
           .
       END PROGRAM DAY09.

      *> ===============================================================
      *> PARSE-DISK-MAP.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-DISK-MAP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO IN-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FD-DATA.
       01  F-FILE-RECORD                         PIC X(20000).

       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-LINE                               PIC X(20000).
       01  LS-INPUT-INDEX                        PIC 9(6) COMP.
       01  LS-CURRENT-INPUT                      PIC 9(1) COMP.
       01  LS-LINE-LENGTH                        PIC 9(5) COMP.
       01  LS-CURRENT-DISK-ENTRY-ID              PIC S9(6) COMP.
       LINKAGE SECTION.
       01  IN-FILE-PATH                          PIC X(20000).
       COPY "disk-map" IN "09".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           BY REFERENCE DISK-MAP-GRP.

           OPEN INPUT FD-DATA
           READ FD-DATA INTO F-FILE-RECORD
           MOVE F-FILE-RECORD TO LS-LINE
           CLOSE FD-DATA
           SET LS-LINE-LENGTH TO LENGTH OF FUNCTION TRIM(LS-LINE)

      *> Parse the disk map from the line
           PERFORM VARYING LS-INPUT-INDEX FROM 1 BY 1
               UNTIL LS-INPUT-INDEX > LS-LINE-LENGTH
               EVALUATE FUNCTION MOD(LS-INPUT-INDEX, 2)
                   WHEN 0
                       SET LS-CURRENT-DISK-ENTRY-ID TO C-FREE
                   WHEN OTHER
                       COMPUTE LS-CURRENT-DISK-ENTRY-ID
                           = (LS-INPUT-INDEX - 1) / 2
               END-EVALUATE
               MOVE LS-LINE(LS-INPUT-INDEX:1) TO LS-CURRENT-INPUT

               PERFORM LS-CURRENT-INPUT TIMES
                   ADD 1 TO DISK-MAP-SIZE
                   SET DISK-ENTRY(DISK-MAP-SIZE) TO
                       LS-CURRENT-DISK-ENTRY-ID
               END-PERFORM
           END-PERFORM
           GOBACK.
       END PROGRAM PARSE-DISK-MAP.

      *> ===============================================================
      *> DEFRAGMENT-DISK-1
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEFRAGMENT-DISK-1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-FIRST-FREE-BLOCK-INDEX             PIC 9(6) COMP VALUE 0.

       LINKAGE SECTION.
       COPY "disk-map" IN "09".
       PROCEDURE DIVISION USING
           BY REFERENCE DISK-MAP-GRP.

      *> Start from the end of the disk map, going left
           SET LS-FIRST-FREE-BLOCK-INDEX TO 1
           PERFORM VARYING DISK-INDEX FROM DISK-MAP-SIZE BY -1
               UNTIL DISK-INDEX < LS-FIRST-FREE-BLOCK-INDEX
               IF DISK-ENTRY(DISK-INDEX) NOT= C-FREE
               THEN

      *> Look for the first free block from the left, going right
                   PERFORM UNTIL LS-FIRST-FREE-BLOCK-INDEX >
                       DISK-MAP-SIZE
                       IF DISK-ENTRY(LS-FIRST-FREE-BLOCK-INDEX)
                           = C-FREE
                       THEN
                           EXIT PERFORM
                       ELSE
                           ADD 1 TO LS-FIRST-FREE-BLOCK-INDEX
                       END-IF

                   END-PERFORM
                   IF DISK-INDEX < LS-FIRST-FREE-BLOCK-INDEX
                   THEN
      *> We've already moved all the blocks to the left.
      *> The first free block is already to the right of all the
      *> occupied blocks.
                       EXIT PERFORM
                   END-IF
      *> Swap the occupied block on the right with the free block
      *> on the left.
                   SET DISK-ENTRY(LS-FIRST-FREE-BLOCK-INDEX) TO
                       DISK-ENTRY(DISK-INDEX)
                   SET DISK-ENTRY(DISK-INDEX) TO C-FREE
               END-IF

           END-PERFORM

           GOBACK.
       END PROGRAM DEFRAGMENT-DISK-1.

      *> ===============================================================
      *> DEFRAGMENT-DISK-2
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEFRAGMENT-DISK-2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-FIRST-FREE-BLOCK-INDEX             PIC 9(6) COMP VALUE 0.
       01  LS-FILE-BLOCK-PTR                     PIC 9(6) COMP.
       01  LS-FILE-START-PTR                     PIC 9(6) COMP.
       01  LS-FILE-END-PTR                       PIC 9(6) COMP.
       01  LS-FILE-LENGTH                        PIC 9(6) COMP.

       LINKAGE SECTION.
       COPY "disk-map" IN "09".
       PROCEDURE DIVISION USING
           BY REFERENCE DISK-MAP-GRP.

           SET LS-FILE-START-PTR TO DISK-MAP-SIZE
           SET LS-FILE-END-PTR TO DISK-MAP-SIZE
           COMPUTE LS-FILE-BLOCK-PTR = DISK-MAP-SIZE - 1
           PERFORM UNTIL EXIT
      *> Go to the beginning of the rightmost file
               PERFORM UNTIL LS-FILE-START-PTR < 1
                   IF DISK-ENTRY(LS-FILE-START-PTR) NOT =
                       DISK-ENTRY(LS-FILE-START-PTR - 1)
                   THEN
                       EXIT PERFORM
                   ELSE
                       ADD -1 TO LS-FILE-START-PTR
                   END-IF
               END-PERFORM
               IF DISK-ENTRY(LS-FILE-START-PTR) NOT = C-FREE
               THEN
                   COMPUTE LS-FILE-LENGTH = LS-FILE-END-PTR -
                       LS-FILE-START-PTR + 1

                   CALL "FIND-FIRST-FREE-SPAN" USING
                       BY REFERENCE DISK-MAP-GRP
                       BY REFERENCE LS-FILE-LENGTH
                       BY REFERENCE LS-FILE-START-PTR
                       BY REFERENCE LS-FIRST-FREE-BLOCK-INDEX
                   IF LS-FIRST-FREE-BLOCK-INDEX > 0
                       AND LS-FIRST-FREE-BLOCK-INDEX < LS-FILE-START-PTR
                   THEN
                       CALL "MOVE-FILE" USING
                           BY REFERENCE DISK-MAP-GRP
                           BY REFERENCE LS-FILE-START-PTR
                           BY REFERENCE LS-FIRST-FREE-BLOCK-INDEX
                           BY REFERENCE LS-FILE-LENGTH
                   END-IF
      *> Compute the file length
               END-IF
               ADD -1 TO LS-FILE-START-PTR
               SET LS-FILE-END-PTR TO LS-FILE-START-PTR
      *> We reached the beginning of the disk map, exit.
               IF LS-FILE-START-PTR < 1
               THEN
                   EXIT PERFORM
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM DEFRAGMENT-DISK-2.

      *> ===============================================================
      *> FIND-FIRST-FREE-SPAN
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-FIRST-FREE-SPAN.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-FILE-BLOCK-PTR                     PIC 9(6) COMP.

       LINKAGE SECTION.
       COPY "disk-map" IN "09".
       01  IN-SPAN-LENGTH                        PIC 9(6) COMP.
       01  IN-MAX-PTR                            PIC 9(6) COMP.
       01  OUT-FIRST-FREE-INDEX                  PIC 9(6) COMP VALUE 0.

       PROCEDURE DIVISION USING
           BY REFERENCE DISK-MAP-GRP
           BY REFERENCE IN-SPAN-LENGTH
           BY REFERENCE IN-MAX-PTR
           BY REFERENCE OUT-FIRST-FREE-INDEX.

           SET OUT-FIRST-FREE-INDEX TO 0
           PERFORM VARYING OUT-FIRST-FREE-INDEX FROM 1 BY 1
               UNTIL OUT-FIRST-FREE-INDEX > IN-MAX-PTR
               IF DISK-ENTRY(OUT-FIRST-FREE-INDEX) = C-FREE
               THEN
                   SET LS-FILE-BLOCK-PTR TO OUT-FIRST-FREE-INDEX
                   PERFORM VARYING LS-FILE-BLOCK-PTR FROM
                       OUT-FIRST-FREE-INDEX BY 1 UNTIL
                       LS-FILE-BLOCK-PTR - OUT-FIRST-FREE-INDEX + 1 =
                       IN-SPAN-LENGTH

                       IF DISK-ENTRY(LS-FILE-BLOCK-PTR) NOT = C-FREE
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   IF  DISK-ENTRY(LS-FILE-BLOCK-PTR) = C-FREE AND
                       LS-FILE-BLOCK-PTR - OUT-FIRST-FREE-INDEX + 1 =
                       IN-SPAN-LENGTH
                   THEN
                       GOBACK
                   END-IF

               END-IF
           END-PERFORM
           SET OUT-FIRST-FREE-INDEX TO 0


           GOBACK.
       END PROGRAM FIND-FIRST-FREE-SPAN.

      *> ===============================================================
      *> MOVE-FILE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE-FILE.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-FILE-BLOCK-PTR                     PIC 9(6) COMP.

       LINKAGE SECTION.
       COPY "disk-map" IN "09".
       01  IN-SOURCE-INDEX                       PIC 9(6) COMP.
       01  IN-DEST-INDEX                         PIC 9(6) COMP.
       01  IN-FILE-LENGTH                        PIC 9(6) COMP.

       PROCEDURE DIVISION USING
           BY REFERENCE DISK-MAP-GRP
           BY REFERENCE IN-SOURCE-INDEX
           BY REFERENCE IN-DEST-INDEX
           BY REFERENCE IN-FILE-LENGTH.

           PERFORM VARYING LS-FILE-BLOCK-PTR FROM 0 BY 1
               UNTIL LS-FILE-BLOCK-PTR = IN-FILE-LENGTH
               SET DISK-ENTRY(IN-DEST-INDEX + LS-FILE-BLOCK-PTR) TO
                   DISK-ENTRY(IN-SOURCE-INDEX + LS-FILE-BLOCK-PTR)
               SET DISK-ENTRY(IN-SOURCE-INDEX + LS-FILE-BLOCK-PTR) TO
                   C-FREE
           END-PERFORM

           GOBACK.
       END PROGRAM MOVE-FILE.

      *> ===============================================================
      *> CALCULATE-CHECKSUM.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-CHECKSUM.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY "constants" IN "09".

       LOCAL-STORAGE SECTION.
       01  LS-CHECKSUM-PART                      PIC 9(18) COMP VALUE 0.

       LINKAGE SECTION.
       COPY "disk-map" IN "09".
       01  OUT-CHECKSUM-TOTAL                    PIC 9(18) COMP VALUE 0.

       PROCEDURE DIVISION USING
           BY REFERENCE DISK-MAP-GRP
           BY REFERENCE OUT-CHECKSUM-TOTAL.

           PERFORM VARYING DISK-INDEX FROM 1 BY 1
               UNTIL DISK-INDEX > DISK-MAP-SIZE
               IF DISK-ENTRY(DISK-INDEX) NOT = C-FREE
                   COMPUTE LS-CHECKSUM-PART =
                       DISK-ENTRY(DISK-INDEX) * (DISK-INDEX -
                   1)
                   ADD LS-CHECKSUM-PART TO OUT-CHECKSUM-TOTAL
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM CALCULATE-CHECKSUM.

