      *> ===============================================================
      *> TO-BINARY-STRING
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TO-BINARY-STRING.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-NUMBER                            PIC 9(18) COMP.
       01  LS-QUOTIENT                          PIC 9(18) COMP.
       01  LS-REMAINDER                         PIC 9(1).
       01  LS-TEMP-STRING                       PIC X(50).
       01  LS-STRING-PTR                        PIC 9(2).
       01  LS-LEFT-PADDING-LENGTH               PIC 9(2).
       01  LS-LEFT-PADDING                      PIC X(50) VALUE SPACES.
       LINKAGE SECTION.
       01  IN-NUMBER                            PIC 9(18) COMP.
       01  IN-STRING-LENGTH                     PIC 9(2).
       01  OUT-BINARY-STRING                    PIC x(50).

       PROCEDURE DIVISION USING BY REFERENCE
           IN-NUMBER
           IN-STRING-LENGTH
           OUT-BINARY-STRING.

           SET OUT-BINARY-STRING TO SPACE

           SET LS-NUMBER TO IN-NUMBER
           PERFORM UNTIL LS-NUMBER = 0
               DIVIDE 2 INTO LS-NUMBER
                   GIVING LS-QUOTIENT REMAINDER LS-REMAINDER

               STRING
                   LS-REMAINDER
                   FUNCTION TRIM(OUT-BINARY-STRING)
                   INTO LS-TEMP-STRING
               END-STRING
               MOVE LS-TEMP-STRING TO OUT-BINARY-STRING

               COMPUTE LS-NUMBER = LS-QUOTIENT

           END-PERFORM
           COMPUTE LS-LEFT-PADDING-LENGTH = 
               IN-STRING-LENGTH -
               LENGTH OF FUNCTION TRIM(OUT-BINARY-STRING) 
           PERFORM LS-LEFT-PADDING-LENGTH TIMES
               STRING FUNCTION TRIM(LS-LEFT-PADDING) "0"
                   INTO LS-LEFT-PADDING
               END-STRING
           END-PERFORM
           STRING FUNCTION TRIM(LS-LEFT-PADDING)
               FUNCTION TRIM(OUT-BINARY-STRING)
               INTO LS-TEMP-STRING
           END-STRING
           MOVE LS-TEMP-STRING TO OUT-BINARY-STRING
           GOBACK.
       END PROGRAM TO-BINARY-STRING.

      *> ===============================================================
      *> FROM-BINARY-STRING
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FROM-BINARY-STRING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-END-PTR                            CONSTANT 0.
       01  WS-BASE                               CONSTANT 2.
       LOCAL-STORAGE SECTION.
       01  LS-INDEX                              PIC 9(18).
       01  LS-LENGTH                             PIC 9(2).
       01  LS-DIGIT                              PIC 9(1).
       01  LS-POWER                              PIC 9(2).
       LINKAGE SECTION.
       01  IN-BINARY-STRING                      PIC X(50).
       01  OUT-NUMBER                            PIC 9(18) COMP.

       PROCEDURE DIVISION USING BY REFERENCE
           IN-BINARY-STRING
           OUT-NUMBER.

           SET OUT-NUMBER TO 0
           SET LS-POWER TO 0
           SET LS-LENGTH TO LENGTH OF FUNCTION TRIM(IN-BINARY-STRING)
           PERFORM VARYING LS-INDEX FROM LS-LENGTH BY -1
               UNTIL LS-INDEX = 0
               SET LS-DIGIT TO IN-BINARY-STRING(LS-INDEX:1)
               COMPUTE OUT-NUMBER = OUT-NUMBER + LS-DIGIT *(2**LS-POWER)
               ADD 1 TO LS-POWER
           END-PERFORM
           GOBACK.
       END PROGRAM FROM-BINARY-STRING.
