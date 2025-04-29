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
       01 WS-DELIMITER               PIC X(3) VALUE "   ".
       LOCAL-STORAGE SECTION.
       01 C-MAX-FILE-LENGTH          CONSTANT 1000.
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
       01 LS-DIFFERENCE-ROW          PIC 9(5).
       01 LS-DIFFERENCE-ACC          PIC 9(10) VALUE 0.
       01 LS-SIMILARITY-ACC          PIC 9(10) VALUE 0.
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
                       DELIMITED BY WS-DELIMITER
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
               COMPUTE LS-DIFFERENCE-ROW = FUNCTION ABS(
                   LS-DATA-ITEM-1(LS-TABLE-INDEX) -
                   LS-DATA-ITEM-2(LS-TABLE-INDEX))
               COMPUTE LS-DIFFERENCE-ACC = LS-DIFFERENCE-ACC
                   + LS-DIFFERENCE-ROW
      *> Part 2: Calculate the number of times the item from the first
      *> table appears in the second table.
      *> The sum of these calculations is the similarity.
               PERFORM VARYING LS-SEARCH-INDEX FROM 1 BY 1
                   UNTIL LS-SEARCH-INDEX > LS-FILE-LENGTH
                       IF LS-DATA-ITEM-1(LS-TABLE-INDEX) =
                           LS-DATA-ITEM-2(LS-SEARCH-INDEX)
                           COMPUTE LS-SIMILARITY-ACC = LS-SIMILARITY-ACC
                               + LS-DATA-ITEM-1(LS-TABLE-INDEX)
               END-PERFORM
       END-PERFORM
       DISPLAY LS-DIFFERENCE-ACC
       DISPLAY LS-SIMILARITY-ACC
       .
