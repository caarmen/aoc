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
       PROGRAM-ID. DAY01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-DATA ASSIGN TO LS-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FD-DATA.
      *> Example data:
      *> 3   4
      *> 4   3
      *> 2   5
      *> 1   3
      *> 3   9
      *> 3   3

       01  F-DATA-RECORD             PIC X(100).

       WORKING-STORAGE SECTION.
       01 C-DELIMITER                CONSTANT "   ".
       01 C-MAX-FILE-LENGTH          CONSTANT 1000.

       LOCAL-STORAGE SECTION.
       01 LS-FILE-PATH               PIC X(100).
       01 LS-FILE-LENGTH             PIC 9(4) VALUE 0.

       01 LS-TABLE-INDEX             PIC 9(4) VALUE 1.
       01 LS-DATA-TABLE-1 OCCURS 1 TO C-MAX-FILE-LENGTH TIMES
           DEPENDING ON LS-FILE-LENGTH.
           05 LS-DATA-ITEM-1         PIC 9(5) COMP.
       01 LS-DATA-TABLE-2 OCCURS 1 TO C-MAX-FILE-LENGTH TIMES
           DEPENDING ON LS-FILE-LENGTH
           INDEXED BY LS-SEARCH-INDEX.
           05 LS-DATA-ITEM-2         PIC 9(5) COMP.

       01 LS-DIFFERENCE              PIC 9(10) VALUE 0.
       01 LS-SIMILARITY              PIC 9(10) VALUE 0.
       PROCEDURE DIVISION.

      *> Read the file path from the command line arguments.
       ACCEPT LS-FILE-PATH FROM COMMAND-LINE

      *> Open the file and read the data into the tables.
       OPEN INPUT FD-DATA
       PERFORM UNTIL EXIT
           READ FD-DATA INTO F-DATA-RECORD
               AT END
                   EXIT PERFORM
               NOT AT END
                   UNSTRING F-DATA-RECORD
                       DELIMITED BY C-DELIMITER
                       INTO LS-DATA-ITEM-1(LS-TABLE-INDEX)
                            LS-DATA-ITEM-2(LS-TABLE-INDEX)
                   END-UNSTRING
                   COMPUTE LS-TABLE-INDEX = LS-TABLE-INDEX + 1
           END-READ
       END-PERFORM
       CLOSE FD-DATA

       COMPUTE LS-FILE-LENGTH = LS-TABLE-INDEX - 1

      *> Sort the tables.
       SORT LS-DATA-TABLE-1 ON ASCENDING KEY LS-DATA-ITEM-1
       SORT LS-DATA-TABLE-2 ON ASCENDING KEY LS-DATA-ITEM-2

       PERFORM VARYING LS-TABLE-INDEX FROM 1 BY 1
           UNTIL LS-TABLE-INDEX > LS-FILE-LENGTH
      *> Part 1: Calculate the absolute difference for each pair of
      *> items from the two tables, and display the sum.
               COMPUTE LS-DIFFERENCE = LS-DIFFERENCE +
                   FUNCTION ABS(
                       LS-DATA-ITEM-1(LS-TABLE-INDEX) -
                       LS-DATA-ITEM-2(LS-TABLE-INDEX)
                   )
      *> Part 2: Calculate the number of times the item from the first
      *> table appears in the second table.
      *> The sum of these calculations is the similarity.
               PERFORM VARYING LS-SEARCH-INDEX FROM 1 BY 1
                   UNTIL LS-SEARCH-INDEX > LS-FILE-LENGTH
                       IF LS-DATA-ITEM-1(LS-TABLE-INDEX) =
                           LS-DATA-ITEM-2(LS-SEARCH-INDEX)
                           COMPUTE LS-SIMILARITY = LS-SIMILARITY
                               + LS-DATA-ITEM-1(LS-TABLE-INDEX)
               END-PERFORM
       END-PERFORM
       DISPLAY LS-DIFFERENCE
       DISPLAY LS-SIMILARITY
       .
