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
       PROGRAM-ID. DAYXX.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH.
       END PROGRAM DAYXX.

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
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.
