       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY22.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH.
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
       01  LS-ITERATION-COUNT        PIC 9(4) VALUE 2000.
       01  LS-SECRET-NUMBER          PIC 9(16) COMP.
       01  LS-NEW-SECRET-NUMBER      PIC 9(16) COMP.
       01  LS-TOTAL                  PIC 9(16) COMP VALUE 0.

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
                       display ls-line
                       SET LS-SECRET-NUMBER TO FUNCTION NUMVAL(
                           LS-LINE
                       )
                       CALL "GET-NEXT-SECRET-NUMBERS" USING
                           LS-ITERATION-COUNT
                           LS-SECRET-NUMBER
                           LS-NEW-SECRET-NUMBER
                       DISPLAY LS-SECRET-NUMBER ": "
                           LS-NEW-SECRET-NUMBER
                       ADD LS-NEW-SECRET-NUMBER TO LS-TOTAL

           END-PERFORM
           CLOSE FD-DATA

           DISPLAY "Total: " LS-TOTAL

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
       01  LS-CUR-SECRET-NUMBER                  PIC 9(16) COMP.
       01  LS-NEXT-SECRET-NUMBER                 PIC 9(16) COMP.
       LINKAGE SECTION.
       01  IN-ITERATION-COUNT                    PIC 9(4).
       01  IN-SECRET-NUMBER                      PIC 9(16) COMP.
       01  OUT-SECRET-NUMBER                     PIC 9(16) COMP.
       PROCEDURE DIVISION USING BY REFERENCE
           IN-ITERATION-COUNT
           IN-SECRET-NUMBER
           OUT-SECRET-NUMBER.

           SET LS-CUR-SECRET-NUMBER TO IN-SECRET-NUMBER
           PERFORM IN-ITERATION-COUNT TIMES
               SET LS-NEXT-SECRET-NUMBER TO 0

               CALL "GET-NEXT-SECRET-NUMBER" USING
                   LS-CUR-SECRET-NUMBER
                   LS-NEXT-SECRET-NUMBER

               SET LS-CUR-SECRET-NUMBER TO LS-NEXT-SECRET-NUMBER

           END-PERFORM

           SET OUT-SECRET-NUMBER TO LS-NEXT-SECRET-NUMBER

           GOBACK.
       END PROGRAM GET-NEXT-SECRET-NUMBERS.

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
