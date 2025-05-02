       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY03.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FD-DATA.
      *> Example data:
      *> xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
       01  F-DATA-RECORD     PIC X(5000).

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH      PIC X(20).
       01  LS-LINE           PIC X(5000).
       01  LS-LINE-VALUE     USAGE BINARY-LONG.
       01  LS-TOTAL          USAGE BINARY-LONG.

       PROCEDURE DIVISION.
           ACCEPT LS-FILE-PATH FROM COMMAND-LINE.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-DATA-RECORD
               AT END
                   EXIT PERFORM
               NOT AT END
                   MOVE F-DATA-RECORD TO LS-LINE
                   CALL "PARSE-LINE" USING
                       BY REFERENCE LS-LINE
                       RETURNING LS-LINE-VALUE
                   COMPUTE LS-TOTAL = LS-TOTAL + LS-LINE-VALUE
           END-PERFORM

           DISPLAY LS-TOTAL

           CLOSE FD-DATA
       GOBACK.
       END PROGRAM DAY03.

      *> ===============================================================
      *> PARSE-LINE
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-LINE.
       DATA DIVISION.

       LOCAL-STORAGE SECTION.
      *> Slightly modified text-input,b efore we parse it.
       01  LS-TEXT-INPUT                PIC x(5001).
       01  LS-TEXT-LENGTH               USAGE BINARY-LONG.
      *> Data items to split the input text into chunks:
       01  LS-TEXT-CHUNK-POINTER        USAGE BINARY-LONG.
       01  LS-CHUNK-INDEX               USAGE BINARY-LONG VALUE 1.
       01  LS-CURRENT-CHUNK-TEXT        PIC X(5000).
      *> Data items for the math calculations:
       01  LS-CHUNK-VALUE               USAGE BINARY-LONG.
       01  LS-TOTAL                     USAGE BINARY-LONG VALUE 0.

       LINKAGE SECTION.
       01  IN-TEXT                      PIC X(5000).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-TEXT.

           STRING "x" IN-TEXT INTO LS-TEXT-INPUT
           COMPUTE LS-TEXT-LENGTH =
               LENGTH OF FUNCTION TRIM(LS-TEXT-INPUT).

           SET LS-TEXT-CHUNK-POINTER TO 1
           PERFORM UNTIL LS-TEXT-CHUNK-POINTER > LS-TEXT-LENGTH
               UNSTRING LS-TEXT-INPUT
                   DELIMITED BY "mul("
                   INTO LS-CURRENT-CHUNK-TEXT
                   WITH POINTER LS-TEXT-CHUNK-POINTER
               END-UNSTRING
               IF LS-CHUNK-INDEX > 1
               THEN
                   CALL "PROCESS-CHUNK" USING
                       LS-CURRENT-CHUNK-TEXT
                       RETURNING LS-CHUNK-VALUE
                   ADD LS-CHUNK-VALUE TO LS-TOTAL
               END-IF
               ADD 1 TO LS-CHUNK-INDEX

           END-PERFORM
           MOVE LS-TOTAL TO RETURN-CODE
           GOBACK.
       END PROGRAM PARSE-LINE.


      *> ===============================================================
      *> PROCESS-CHUNK.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-CHUNK.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  C-TOKEN-INVALID              CONSTANT 0.
       01  C-TOKEN-MUL-BEGIN            CONSTANT 1.
       01  C-TOKEN-FIRST-DIGIT          CONSTANT 2.
       01  C-TOKEN-COMMA                CONSTANT 3.
       01  C-TOKEN-SECOND-DIGIT         CONSTANT 4.
       01  C-TOKEN-MUL-END              CONSTANT 5.

       LOCAL-STORAGE SECTION.
       01  LS-TEXT-LENGTH               USAGE BINARY-LONG.
       01  LS-TEXT-INDEX                USAGE BINARY-LONG.
       01  LS-PREVIOUS-TOKEN            USAGE BINARY-SHORT
                                            VALUE C-TOKEN-MUL-BEGIN.
       01  LS-CURRENT-TOKEN             USAGE BINARY-SHORT.


       01  LS-CURRENT-CHAR              PIC x(1).
       01  LS-FIRST-NUMBER-TEXT         PIC x(10) VALUE SPACES.
       01  LS-SECOND-NUMBER-TEXT        PIC x(10) VALUE SPACES.

       01  LS-FIRST-NUMBER              USAGE BINARY-LONG.
       01  LS-SECOND-NUMBER             USAGE BINARY-LONG.
       01  LS-VALUE                     USAGE BINARY-LONG.

       LINKAGE SECTION.
       01  IN-TEXT                      PIC X(5000).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-TEXT.

           DISPLAY "Process chunk " function trim(in-text)
           COMPUTE LS-TEXT-LENGTH = LENGTH OF FUNCTION TRIM(IN-TEXT)

           PERFORM VARYING LS-TEXT-INDEX FROM 1 BY 1
               UNTIL LS-TEXT-INDEX > LS-TEXT-LENGTH
                   OR LS-CURRENT-TOKEN = C-TOKEN-MUL-END
               SET LS-CURRENT-CHAR TO IN-TEXT(LS-TEXT-INDEX:1)
               PERFORM NEXT-TOKEN
               SET LS-PREVIOUS-TOKEN TO LS-CURRENT-TOKEN

           END-PERFORM

           IF LS-CURRENT-TOKEN = C-TOKEN-MUL-END
           THEN
               DISPLAY
                   " => "
                   FUNCTION TRIM(LS-FIRST-NUMBER-TEXT)
                   " x "
                   FUNCTION TRIM(LS-SECOND-NUMBER-TEXT)
               MOVE LS-FIRST-NUMBER-TEXT TO LS-FIRST-NUMBER
               MOVE LS-SECOND-NUMBER-TEXT TO LS-SECOND-NUMBER
               COMPUTE LS-VALUE = LS-FIRST-NUMBER * LS-SECOND-NUMBER
           ELSE
               DISPLAY " => invalid"
           END-IF

           MOVE LS-VALUE TO RETURN-CODE
           GOBACK.

       NEXT-TOKEN.
           EVALUATE LS-PREVIOUS-TOKEN
               WHEN C-TOKEN-MUL-BEGIN
                   IF LS-CURRENT-CHAR IS NUMERIC
                   THEN
                       SET LS-CURRENT-TOKEN TO C-TOKEN-FIRST-DIGIT
                       MOVE SPACES TO LS-FIRST-NUMBER-TEXT
                       STRING LS-CURRENT-CHAR INTO LS-FIRST-NUMBER-TEXT
                   ELSE
                       SET LS-CURRENT-TOKEN TO C-TOKEN-INVALID
                   END-IF
               WHEN C-TOKEN-FIRST-DIGIT
                   EVALUATE TRUE
                       WHEN LS-CURRENT-CHAR IS NUMERIC
                           STRING
                               LS-FIRST-NUMBER-TEXT DELIMITED BY SPACE
                               LS-CURRENT-CHAR DELIMITED BY SPACE
                               INTO LS-FIRST-NUMBER-TEXT
                       WHEN LS-CURRENT-CHAR = ","
                           SET LS-CURRENT-TOKEN TO C-TOKEN-COMMA
                       WHEN OTHER
                           SET LS-CURRENT-TOKEN TO C-TOKEN-INVALID
                   END-EVALUATE
               WHEN C-TOKEN-COMMA
                   IF LS-CURRENT-CHAR IS NUMERIC
                   THEN
                       SET LS-CURRENT-TOKEN TO C-TOKEN-SECOND-DIGIT
                       MOVE SPACES TO LS-SECOND-NUMBER-TEXT
                       STRING LS-CURRENT-CHAR INTO LS-SECOND-NUMBER-TEXT
                   ELSE
                       SET LS-CURRENT-TOKEN TO C-TOKEN-INVALID
                   END-IF
               WHEN C-TOKEN-SECOND-DIGIT
                   EVALUATE TRUE
                       WHEN LS-CURRENT-CHAR IS NUMERIC
                           STRING
                               LS-SECOND-NUMBER-TEXT DELIMITED BY SPACE
                               LS-CURRENT-CHAR DELIMITED BY SPACE
                               INTO LS-SECOND-NUMBER-TEXT
                       WHEN LS-CURRENT-CHAR = ")"
                           SET LS-CURRENT-TOKEN TO C-TOKEN-MUL-END
                       WHEN OTHER
                           SET LS-CURRENT-TOKEN TO C-TOKEN-INVALID
                   END-EVALUATE
               WHEN OTHER
                   SET LS-CURRENT-TOKEN TO C-TOKEN-INVALID
           END-EVALUATE.
       END PROGRAM PROCESS-CHUNK.
