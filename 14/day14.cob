       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY14.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-COMMAND-LINE           PIC X(30).
       01  LS-FILE-PATH              PIC X(30).
       01  LS-ROW-COUNT              PIC 9(3).
       01  LS-COL-COUNT              PIC 9(3).
       01  LS-DURATION-S             PIC 9(3).

       PROCEDURE DIVISION.

           ACCEPT LS-COMMAND-LINE FROM COMMAND-LINE

           UNSTRING LS-COMMAND-LINE
               DELIMITED BY " "
               INTO
                   LS-FILE-PATH
                   LS-ROW-COUNT
                   LS-COL-COUNT
                   LS-DURATION-S
           END-UNSTRING

           CALL "PARSE-FILE" USING
               BY REFERENCE
               LS-FILE-PATH
               LS-ROW-COUNT
               LS-COL-COUNT
               LS-DURATION-S
               .
       END PROGRAM DAY14.

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
       01  LS-START-ROW              PIC 9(3).
       01  LS-START-COL              PIC 9(3).
       01  LS-VELOCITY-ROW-PER-S     PIC S9(3).
       01  LS-VELOCITY-COL-PER-S     PIC S9(3).
       01  LS-END-ROW                PIC S9(3).
       01  LS-END-COL                PIC S9(3).
       01  LS-QUAD-1-COUNT           PIC 9(3) VALUE 0.
       01  LS-QUAD-2-COUNT           PIC 9(3) VALUE 0.
       01  LS-QUAD-3-COUNT           PIC 9(3) VALUE 0.
       01  LS-QUAD-4-COUNT           PIC 9(3) VALUE 0.
       01  LS-MID-ROW                PIC 9(3).
       01  LS-MID-COL                PIC 9(3).
       01  LS-SAFETY-FACTOR          PIC 9(11).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       01  IN-ROW-COUNT              PIC 9(3).
       01  IN-COL-COUNT              PIC 9(3).
       01  IN-DURATION-S             PIC 9(3).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-FILE-PATH
           IN-ROW-COUNT
           IN-COL-COUNT
           IN-DURATION-S
           .

           COMPUTE LS-MID-ROW = IN-ROW-COUNT / 2
           COMPUTE LS-MID-COL = IN-COL-COUNT / 2
           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       CALL "PARSE-LINE" USING BY REFERENCE
                           LS-LINE
                           LS-START-ROW
                           LS-START-COL
                           LS-VELOCITY-ROW-PER-S
                           LS-VELOCITY-COL-PER-S

                       CALL "CALCULATE-LOCATION" USING BY REFERENCE
                           IN-ROW-COUNT
                           IN-COL-COUNT
                           LS-START-ROW
                           LS-START-COL
                           LS-VELOCITY-ROW-PER-S
                           LS-VELOCITY-COL-PER-S
                           IN-DURATION-S
                           LS-END-ROW
                           LS-END-COL

                       EVALUATE LS-END-ROW ALSO LS-END-COL
                           WHEN LESS THAN LS-MID-ROW
                               ALSO LESS THAN LS-MID-COL
                               ADD 1 TO LS-QUAD-1-COUNT
                           WHEN LESS THAN LS-MID-ROW
                               ALSO GREATER THAN LS-MID-COL
                               ADD 1 TO LS-QUAD-2-COUNT
                           WHEN GREATER THAN LS-MID-ROW
                               ALSO LESS THAN LS-MID-COL
                               ADD 1 TO LS-QUAD-3-COUNT
                           WHEN GREATER THAN LS-MID-ROW
                               ALSO GREATER THAN LS-MID-COL
                               ADD 1 TO LS-QUAD-4-COUNT
                       END-EVALUATE


           END-PERFORM
           CLOSE FD-DATA
           COMPUTE LS-SAFETY-FACTOR = LS-QUAD-1-COUNT *
               LS-QUAD-2-COUNT *
               LS-QUAD-3-COUNT *
               LS-QUAD-4-COUNT

               DISPLAY "Safety factor: " ls-quad-1-count " * "
                   ls-quad-2-count " * " ls-quad-3-count " * "
                   ls-quad-4-count " = " LS-SAFETY-FACTOR
           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> PARSE-LINE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-LINE.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-TOKEN-LEFT               PIC X(30).
       01  LS-TOKEN-RIGHT               PIC X(30).
       LINKAGE SECTION.
       01  IN-LINE                     PIC X(30).
       01  OUT-START-ROW               PIC 9(3).
       01  OUT-START-COL               PIC 9(3).
       01  OUT-VELOCITY-ROW-PER-S      PIC S9(3).
       01  OUT-VELOCITY-COL-PER-S      PIC S9(3).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-LINE
           OUT-START-ROW
           OUT-START-COL
           OUT-VELOCITY-ROW-PER-S
           OUT-VELOCITY-COL-PER-S.

           UNSTRING IN-LINE(3:28)
               DELIMITED BY " "
               INTO LS-TOKEN-LEFT LS-TOKEN-RIGHT
           END-UNSTRING
           UNSTRING LS-TOKEN-LEFT
               DELIMITED BY ","
               INTO OUT-START-COL OUT-START-ROW
           END-UNSTRING
           UNSTRING LS-TOKEN-RIGHT(3:28)
               DELIMITED BY ","
               INTO OUT-VELOCITY-COL-PER-S OUT-VELOCITY-ROW-PER-S
           END-UNSTRING

           GOBACK.
       END PROGRAM PARSE-LINE.

      *> ===============================================================
      *> CALCULATE-LOCATION.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-LOCATION.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-END-ROW-NOWRAP          PIC S9(5).
       01  LS-END-COL-NOWRAP          PIC S9(5).
       LINKAGE SECTION.
       01  IN-ROW-COUNT               PIC 9(3).
       01  IN-COL-COUNT               PIC 9(3).
       01  IN-START-ROW               PIC 9(3).
       01  IN-START-COL               PIC 9(3).
       01  IN-VELOCITY-ROW-PER-S      PIC S9(3).
       01  IN-VELOCITY-COL-PER-S      PIC S9(3).
       01  IN-DURATION-S              PIC 9(3).
       01  OUT-END-ROW                PIC 9(3).
       01  OUT-END-COL                PIC 9(3).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-ROW-COUNT
           IN-COL-COUNT
           IN-START-ROW
           IN-START-COL
           IN-VELOCITY-ROW-PER-S
           IN-VELOCITY-COL-PER-S
           IN-DURATION-S
           OUT-END-ROW
           OUT-END-COL.

           COMPUTE LS-END-ROW-NOWRAP = IN-START-ROW +
               IN-VELOCITY-ROW-PER-S * IN-DURATION-S

           COMPUTE LS-END-COL-NOWRAP = IN-START-COL +
               IN-VELOCITY-COL-PER-S * IN-DURATION-S

           COMPUTE OUT-END-ROW = FUNCTION MOD(
               LS-END-ROW-NOWRAP,
               IN-ROW-COUNT
           )
           COMPUTE OUT-END-COL = FUNCTION MOD(
               LS-END-COL-NOWRAP,
               IN-COL-COUNT
           )

           GOBACK.
       END PROGRAM CALCULATE-LOCATION.
