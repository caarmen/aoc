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
       PROGRAM-ID. DAY25.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       01  LS-MATCH-COUNT            PIC 9(8).
       COPY "key" IN "25".
       COPY "lock" IN "25".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               LOCK-GRP
               KEY-GRP

           DISPLAY "Locks:"
           PERFORM VARYING LOCK-IDX FROM 1 BY 1 UNTIL
               LOCK-IDX > LOCKS-SIZE
               PERFORM VARYING LOCK-PIN-IDX FROM 1 BY 1 UNTIL
                   LOCK-PIN-IDX > 5
                   DISPLAY LOCK-PIN-HEIGHT(
                       LOCK-IDX, LOCK-PIN-IDX
                   ) NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM
           DISPLAY "Keys:"
           PERFORM VARYING KEY-IDX FROM 1 BY 1 UNTIL
               KEY-IDX > KEYS-SIZE
               PERFORM VARYING KEY-PEAK-IDX FROM 1 BY 1 UNTIL
                   KEY-PEAK-IDX > 5
                   DISPLAY KEY-PEAK-HEIGHT(
                       KEY-IDX, KEY-PEAK-IDX
                   ) NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM

           CALL "FIND-MATCHES" USING
               LOCK-GRP
               KEY-GRP
               LS-MATCH-COUNT

           DISPLAY "Match count: " LS-MATCH-COUNT
           .
       END PROGRAM DAY25.

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

       WORKING-STORAGE SECTION.
       01  C-LOCK                    CONSTANT 0.
       01  C-KEY                     CONSTANT 1.

       LOCAL-STORAGE SECTION.
       01  LS-LINE-IDX               PIC 9(4) VALUE 0.
       01  LS-LINE                   PIC X(47).
       01  LS-PARSE-TYPE             PIC 9(1).
       01  LS-COL-IDX                PIC 9(1).
       01  LS-LINE-IDX-MOD           PIC 9(1).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "key" IN "25".
       COPY "lock" IN "25".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           LOCK-GRP
           KEY-GRP.

           OPEN INPUT FD-DATA
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       display ls-line
                       COMPUTE LS-LINE-IDX-MOD = FUNCTION MOD(
                           LS-LINE-IDX, 8
                       )
                       EVALUATE LS-LINE-IDX-MOD
                           WHEN 0
                               IF LS-LINE(1:1) = "#"
                                   SET LS-PARSE-TYPE TO C-LOCK
                                   ADD 1 TO LOCKS-SIZE
                                   PERFORM VARYING LS-COL-IDX
                                       FROM 1 BY 1 UNTIL LS-COL-IDX > 5
                                       SET LOCK-PIN-HEIGHT(
                                           LOCKS-SIZE,
                                           LS-COL-IDX
                                       ) TO 0
                                   END-PERFORM
                               ELSE
                                   SET LS-PARSE-TYPE TO C-KEY
                                   ADD 1 TO KEYS-SIZE
                                   PERFORM VARYING LS-COL-IDX
                                       FROM 1 BY 1 UNTIL LS-COL-IDX > 5
                                       SET KEY-PEAK-HEIGHT(
                                           KEYS-SIZE,
                                           LS-COL-IDX
                                       ) TO 0
                                   END-PERFORM
                               END-IF
                           WHEN NOT = 7
                               PERFORM VARYING LS-COL-IDX FROM 1 BY 1
                                   UNTIL LS-COL-IDX > 5
                                   IF LS-LINE(LS-COL-IDX:1) = "#"
                                       EVALUATE LS-PARSE-TYPE ALSO
                                           LS-LINE-IDX-MOD
                                           WHEN C-LOCK ALSO ANY
                                               ADD 1 TO LOCK-PIN-HEIGHT(
                                                   LOCKS-SIZE,
                                                   LS-COL-IDX
                                               )
                                           WHEN C-KEY ALSO NOT = 6
                                               ADD 1 TO KEY-PEAK-HEIGHT(
                                                   KEYS-SIZE,
                                                   LS-COL-IDX
                                               )
                                       END-EVALUATE
                                   END-IF
                               END-PERFORM
                       END-EVALUATE
                       ADD 1 TO LS-LINE-IDX
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> FIND-MATCHES.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-MATCHES.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
      *> LS-IS-MATCH is 0 if all the columns fit
      *> for a given lock/key pair.
       01  LS-IS-MATCH                          PIC 9(1) VALUE 0.
       01  LS-COL-IDX                           PIC 9(1).
       LINKAGE SECTION.
       COPY "lock" IN "25".
       COPY "key" IN "25".
       01  OUT-MATCH-COUNT                      PIC 9(8).

       PROCEDURE DIVISION USING BY REFERENCE
           LOCK-GRP
           KEY-GRP
           OUT-MATCH-COUNT.

           SET OUT-MATCH-COUNT TO 0.

           PERFORM VARYING LOCK-IDX FROM 1 BY 1 UNTIL
               LOCK-IDX > LOCKS-SIZE
               PERFORM VARYING KEY-IDX FROM 1 BY 1 UNTIL
                   KEY-IDX > KEYS-SIZE
                   SET LS-IS-MATCH TO 0
                   PERFORM VARYING LS-COL-IDX FROM 1 BY 1 UNTIL
                       LS-COL-IDX > 5
                       IF LOCK-PIN-HEIGHT(LOCK-IDX, LS-COL-IDX) +
                           KEY-PEAK-HEIGHT(KEY-IDX, LS-COL-IDX) > 5
                           SET LS-IS-MATCH TO 1
                       END-IF
                   END-PERFORM
                   IF LS-IS-MATCH = 0
                       ADD 1 TO OUT-MATCH-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM

           .
       END PROGRAM FIND-MATCHES.
