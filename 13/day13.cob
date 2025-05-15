       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY13.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PROCESS-FILE" USING
               BY REFERENCE LS-FILE-PATH.

       END PROGRAM DAY13.

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
       01  F-FILE-RECORD             PIC X(47).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(47).
       01  LS-A1                     PIC 9(3).
       01  LS-B1                     PIC 9(3).
       01  LS-C1                     PIC 9(7).
       01  LS-A2                     PIC 9(3).
       01  LS-B2                     PIC 9(3).
       01  LS-C2                     PIC 9(7).
       01  LS-A                      PIC 9(7).
       01  LS-B                      PIC 9(7).
       01  LS-TOKEN-COUNT            PIC 9(8) VALUE 0.
       01  LS-SOLUTION-EXISTS        PIC 9(1).

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
                           WHEN LS-LINE = SPACE
                               CONTINUE
                           WHEN LS-LINE(8:1) = "A"
                               CALL "PARSE-BUTTON-LINE" USING
                                   BY REFERENCE LS-LINE
                                   LS-A1
                                   LS-A2
                           WHEN LS-LINE(8:1) = "B"
                               CALL "PARSE-BUTTON-LINE" USING
                                   BY REFERENCE LS-LINE
                                   LS-B1
                                   LS-B2
                           WHEN LS-LINE(1:5) = "Prize"
                               CALL "PARSE-PRIZE-LINE" USING
                                   BY REFERENCE LS-LINE
                                   LS-C1
                                   LS-C2

                               CALL "SOLVE" USING
                                   BY REFERENCE
                                   LS-A1
                                   LS-B1
                                   LS-C1
                                   LS-A2
                                   LS-B2
                                   LS-C2
                                   LS-A
                                   LS-B
                                   RETURNING LS-SOLUTION-EXISTS

                               IF LS-SOLUTION-EXISTS = 0
                                   COMPUTE LS-TOKEN-COUNT =
                                       LS-TOKEN-COUNT
                                       + (LS-A * 3) + (LS-B)
                               END-IF
                       END-EVALUATE

           END-PERFORM
           CLOSE FD-DATA

           DISPLAY LS-TOKEN-COUNT " tokens."

           GOBACK.
       END PROGRAM PROCESS-FILE.

      *> ===============================================================
      *> PARSE-BUTTON-LINE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-BUTTON-LINE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-X-TOKEN                PIC X(5).
       01  LS-Y-TOKEN                PIC X(5).

       LINKAGE SECTION.
       01  IN-LINE                   PIC X(30).
       01  OUT-X                     PIC 9(3).
       01  OUT-Y                     PIC 9(3).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LINE
           OUT-X
           OUT-Y.

           UNSTRING IN-LINE(11:19)
               DELIMITED BY ", "
               INTO LS-X-TOKEN LS-Y-TOKEN
           END-UNSTRING

           MOVE LS-X-TOKEN(3:3) TO OUT-X
           MOVE LS-Y-TOKEN(3:3) TO OUT-Y

           GOBACK.
       END PROGRAM PARSE-BUTTON-LINE.

      *> ===============================================================
      *> PARSE-PRIZE-LINE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-PRIZE-LINE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-X-TOKEN                PIC X(7).
       01  LS-Y-TOKEN                PIC X(7).

       LINKAGE SECTION.
       01  IN-LINE                   PIC X(30).
       01  OUT-X                     PIC 9(7).
       01  OUT-Y                     PIC 9(7).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LINE
           OUT-X
           OUT-Y.

           UNSTRING IN-LINE(8:22)
               DELIMITED BY ", "
               INTO LS-X-TOKEN LS-Y-TOKEN
           END-UNSTRING

           MOVE LS-X-TOKEN(3:5) TO OUT-X
           MOVE LS-Y-TOKEN(3:5) TO OUT-Y

           GOBACK.
       END PROGRAM PARSE-PRIZE-LINE.


      *> ===============================================================
      *> SOLVE.
      *>
      *> Given two formulas:
      *> a1x + b1y = c1
      *> a2x + b2y = c2
      *>
      *> Calculate and fill the values for x and y.
      *>
      *> Return 0 if a solution was found, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLVE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-A1                     PIC 9(7).
       01  LS-B1                     PIC 9(7).
       01  LS-C1                     PIC 9(7).
       01  LS-A2                     PIC 9(7).
       01  LS-B2                     PIC 9(7).
       01  LS-C2                     PIC 9(7).
       01  LS-A                      PIC 9(3)V9(3).
       01  LS-B                      PIC 9(3)V9(3).

       LINKAGE SECTION.
       01  IN-A1                     PIC 9(3).
       01  IN-B1                     PIC 9(3).
       01  IN-C1                     PIC 9(7).
       01  IN-A2                     PIC 9(3).
       01  IN-B2                     PIC 9(3).
       01  IN-C2                     PIC 9(7).
       01  OUT-A                     PIC 9(7).
       01  OUT-B                     PIC 9(7).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-A1
           IN-B1
           IN-C1
           IN-A2
           IN-B2
           IN-C2
           OUT-A
           OUT-B
           .

           COMPUTE LS-A1 = IN-A1 * IN-A2
           COMPUTE LS-B1 = IN-B1 * IN-A2
           COMPUTE LS-C1 = IN-C1 * IN-A2
           COMPUTE LS-A2 = IN-A2 * IN-A1
           COMPUTE LS-B2 = IN-B2 * IN-A1
           COMPUTE LS-C2 = IN-C2 * IN-A1

           COMPUTE LS-B = (LS-C1 - LS-C2) / (LS-B1 - LS-B2)
           COMPUTE LS-A = (IN-C1 - (IN-B1 * LS-B)) / IN-A1

           SET RETURN-CODE TO 0
           SET OUT-A TO FUNCTION INTEGER-PART(LS-A)
           SET OUT-B TO FUNCTION INTEGER-PART(LS-B)
           IF LS-A - OUT-A NOT = 0
               SET RETURN-CODE TO 1
           END-IF
           IF LS-B - OUT-B NOT = 0
               SET RETURN-CODE TO 1
           END-IF

           GOBACK.
       END PROGRAM SOLVE.
