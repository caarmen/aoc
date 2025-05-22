       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY16.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).
       COPY "grid" IN "16".

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH
               GRID-GRP

           CALL "DISPLAY-GRID" USING BY REFERENCE
               GRID-GRP

           CALL "PROCESS-GRID" USING BY REFERENCE
               GRID-GRP

           CALL "DISPLAY-GRID" USING BY REFERENCE
               GRID-GRP
           .
       END PROGRAM DAY16.

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
       01  F-FILE-RECORD             PIC X(141).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                   PIC X(141).

       LINKAGE SECTION.
       01  IN-FILE-PATH              PIC X(30).
       COPY "grid" IN "16".

       PROCEDURE DIVISION USING
           BY REFERENCE IN-FILE-PATH
           GRID-GRP.

           OPEN INPUT FD-DATA
           SET GRID-ROW-INDEX TO 0
           PERFORM UNTIL EXIT
               READ FD-DATA INTO F-FILE-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE F-FILE-RECORD TO LS-LINE
                       IF GRID-SIZE = 0
                           SET GRID-SIZE TO LENGTH OF
                               FUNCTION TRIM(LS-LINE)
                       END-IF
                       ADD 1 TO GRID-ROW-INDEX
                       PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                           UNTIL GRID-COL-INDEX = LENGTH OF
                           FUNCTION TRIM(LS-LINE)
                           SET GRID-CELL(GRID-ROW-INDEX,GRID-COL-INDEX)
                               TO LS-LINE(GRID-COL-INDEX:1)
                           SET DIRECTION(GRID-ROW-INDEX, GRID-COL-INDEX)
                               TO SPACE
                           EVALUATE LS-LINE(GRID-COL-INDEX:1)
                               WHEN "S"
                                   SET START-ROW TO GRID-ROW-INDEX
                                   SET START-COL TO GRID-COL-INDEX
                                   SET CUR-ROW TO GRID-ROW-INDEX
                                   SET CUR-COL TO GRID-COL-INDEX
                               WHEN "E"
                                   SET END-ROW TO GRID-ROW-INDEX
                                   SET END-COL TO GRID-COL-INDEX
                           END-EVALUATE
                       END-PERFORM
           END-PERFORM
           CLOSE FD-DATA

           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> PROCESS-GRID.
      *> https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-GRID.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-INFINITY                        CONSTANT 99999.

       LOCAL-STORAGE SECTION.
       01  LS-NEIGHBOR-ROW                      PIC 9(3).
       01  LS-NEIGHBOR-COL                      PIC 9(3).
       01  LS-NEIGHBOR-NEW-DIST                 PIC 9(5).
       01  LS-NEIGHBOR-UNVISITED-INDEX          PIC 9(5).
       COPY "unvisited" IN "16".

       LINKAGE SECTION.
       COPY "grid" IN "16".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.
      *> Fill the unvisited set with all the nodes.
           SET UNVISITED-INDEX TO 0
           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1 UNTIL
               GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1 UNTIL
                   GRID-COL-INDEX > GRID-SIZE
                   EVALUATE GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                       WHEN "."
                       WHEN "E"
                           ADD 1 TO UNVISITED-SIZE
                           SET UNVISITED-ROW(UNVISITED-SIZE) TO
                               GRID-ROW-INDEX
                           SET UNVISITED-COL(UNVISITED-SIZE) TO
                               GRID-COL-INDEX
                           SET UNVISITED-DIST-FROM-START(
                               UNVISITED-SIZE) TO C-INFINITY
                   END-EVALUATE
               END-PERFORM
           END-PERFORM
           ADD 1 TO UNVISITED-SIZE
           SET UNVISITED-ROW(UNVISITED-SIZE) TO START-ROW
           SET UNVISITED-COL(UNVISITED-SIZE) TO START-COL
           SET UNVISITED-DIST-FROM-START(UNVISITED-SIZE) TO 0
           SET DIRECTION(START-ROW, START-COL) TO ">"
           SORT UNVISITED

      *> Continue until all nodes have been visited?
           PERFORM UNTIL UNVISITED-SIZE = 0
               SORT UNVISITED
               IF UNVISITED-DIST-FROM-START(1) = C-INFINITY
                   DISPLAY "Only uneachable nodes"
                   EXIT PERFORM
               END-IF
               IF UNVISITED-ROW(1) = END-ROW
                   AND UNVISITED-COL(1) = END-COL
                   DISPLAY "At end: " UNVISITED-DIST-FROM-START(1)
                   EXIT PERFORM
               END-IF

               SET CUR-ROW TO UNVISITED-ROW(1)
               SET CUR-COL TO UNVISITED-COL(1)
               IF CUR-ROW = END-ROW AND CUR-COL = END-COL
                   EXIT PERFORM
               END-IF
      *> =========
      *> Look up ^
      *> =========
               COMPUTE LS-NEIGHBOR-ROW = CUR-ROW - 1
               COMPUTE LS-NEIGHBOR-COL = CUR-COL
               IF GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "."
                   OR GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "E"
      *> Find the index of this neighbor in the unvisited set
                   CALL "FIND-UNVISITED-NODE" USING BY REFERENCE
                       UNVISITED-GRP
                       LS-NEIGHBOR-ROW
                       LS-NEIGHBOR-COL
                       LS-NEIGHBOR-UNVISITED-INDEX
                   IF LS-NEIGHBOR-UNVISITED-INDEX > 0
      *> Calculate the cost to move ^
                       SET LS-NEIGHBOR-NEW-DIST
                           TO UNVISITED-DIST-FROM-START(1)
      *> Calculate rotation cost
                       EVALUATE DIRECTION(CUR-ROW, CUR-COL)
                           WHEN "v"
                               ADD 2000 TO LS-NEIGHBOR-NEW-DIST
                           WHEN "<"
                           WHEN ">"
                               ADD 1000 TO LS-NEIGHBOR-NEW-DIST
                       END-EVALUATE
                       ADD 1 TO LS-NEIGHBOR-NEW-DIST
      *> Update distance if it's shorter
                       IF LS-NEIGHBOR-NEW-DIST <
                           UNVISITED-DIST-FROM-START(
                           LS-NEIGHBOR-UNVISITED-INDEX
                       )
                           SET UNVISITED-DIST-FROM-START(
                               LS-NEIGHBOR-UNVISITED-INDEX
                           ) TO LS-NEIGHBOR-NEW-DIST
                           SET DIRECTION(LS-NEIGHBOR-ROW,
                               LS-NEIGHBOR-COL) TO "^"
                       END-IF
                   END-IF
               END-IF
      *> ============
      *> Look right >
      *> ============
               COMPUTE LS-NEIGHBOR-ROW = CUR-ROW
               COMPUTE LS-NEIGHBOR-COL = CUR-COL + 1
               IF GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "."
                   OR GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "E"
      *> Find the index of this neighbor in the unvisited set
                   CALL "FIND-UNVISITED-NODE" USING BY REFERENCE
                       UNVISITED-GRP
                       LS-NEIGHBOR-ROW
                       LS-NEIGHBOR-COL
                       LS-NEIGHBOR-UNVISITED-INDEX
                   IF LS-NEIGHBOR-UNVISITED-INDEX > 0
      *> Calculate the cost to move ^
                       SET LS-NEIGHBOR-NEW-DIST
                           TO UNVISITED-DIST-FROM-START(1)
      *> Calculate rotation cost
                       EVALUATE DIRECTION(CUR-ROW, CUR-COL)
                           WHEN "<"
                               ADD 2000 TO LS-NEIGHBOR-NEW-DIST
                           WHEN "^"
                           WHEN "v"
                               ADD 1000 TO LS-NEIGHBOR-NEW-DIST
                       END-EVALUATE
                       ADD 1 TO LS-NEIGHBOR-NEW-DIST
      *> Update distance if it's shorter
                       IF LS-NEIGHBOR-NEW-DIST <
                           UNVISITED-DIST-FROM-START(
                           LS-NEIGHBOR-UNVISITED-INDEX
                       )
                           SET UNVISITED-DIST-FROM-START(
                               LS-NEIGHBOR-UNVISITED-INDEX
                           ) TO LS-NEIGHBOR-NEW-DIST
                           SET DIRECTION(LS-NEIGHBOR-ROW,
                               LS-NEIGHBOR-COL) TO ">"
                       END-IF
                   END-IF
               END-IF
      *> ===========
      *> Look down v
      *> ===========
               COMPUTE LS-NEIGHBOR-ROW = CUR-ROW + 1
               COMPUTE LS-NEIGHBOR-COL = CUR-COL
               IF GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "."
                   OR GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "E"
      *> Find the index of this neighbor in the unvisited set
                   CALL "FIND-UNVISITED-NODE" USING BY REFERENCE
                       UNVISITED-GRP
                       LS-NEIGHBOR-ROW
                       LS-NEIGHBOR-COL
                       LS-NEIGHBOR-UNVISITED-INDEX
                   IF LS-NEIGHBOR-UNVISITED-INDEX > 0
      *> Calculate the cost to move ^
                       SET LS-NEIGHBOR-NEW-DIST
                           TO UNVISITED-DIST-FROM-START(1)
      *> Calculate rotation cost
                       EVALUATE DIRECTION(CUR-ROW, CUR-COL)
                           WHEN "^"
                               ADD 2000 TO LS-NEIGHBOR-NEW-DIST
                           WHEN "<"
                           WHEN ">"
                               ADD 1000 TO LS-NEIGHBOR-NEW-DIST
                       END-EVALUATE
                       ADD 1 TO LS-NEIGHBOR-NEW-DIST
      *> Update distance if it's shorter
                       IF LS-NEIGHBOR-NEW-DIST <
                           UNVISITED-DIST-FROM-START(
                           LS-NEIGHBOR-UNVISITED-INDEX
                       )
                           SET UNVISITED-DIST-FROM-START(
                               LS-NEIGHBOR-UNVISITED-INDEX
                           ) TO LS-NEIGHBOR-NEW-DIST
                           SET DIRECTION(LS-NEIGHBOR-ROW,
                               LS-NEIGHBOR-COL) TO "v"
                       END-IF
                   END-IF
               END-IF
      *> ===========
      *> Look left <
      *> ===========
               COMPUTE LS-NEIGHBOR-ROW = CUR-ROW
               COMPUTE LS-NEIGHBOR-COL = CUR-COL - 1
               IF GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "."
                   OR GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL) = "E"
      *> Find the index of this neighbor in the unvisited set
                   CALL "FIND-UNVISITED-NODE" USING BY REFERENCE
                       UNVISITED-GRP
                       LS-NEIGHBOR-ROW
                       LS-NEIGHBOR-COL
                       LS-NEIGHBOR-UNVISITED-INDEX
                   IF LS-NEIGHBOR-UNVISITED-INDEX > 0
      *> Calculate the cost to move ^
                       SET LS-NEIGHBOR-NEW-DIST
                           TO UNVISITED-DIST-FROM-START(1)
      *> Calculate rotation cost
                       EVALUATE DIRECTION(CUR-ROW, CUR-COL)
                           WHEN ">"
                               ADD 2000 TO LS-NEIGHBOR-NEW-DIST
                           WHEN "^"
                           WHEN "v"
                               ADD 1000 TO LS-NEIGHBOR-NEW-DIST
                       END-EVALUATE
                       ADD 1 TO LS-NEIGHBOR-NEW-DIST
      *> Update distance if it's shorter
                       IF LS-NEIGHBOR-NEW-DIST <
                           UNVISITED-DIST-FROM-START(
                           LS-NEIGHBOR-UNVISITED-INDEX
                       )
                           SET UNVISITED-DIST-FROM-START(
                               LS-NEIGHBOR-UNVISITED-INDEX
                           ) TO LS-NEIGHBOR-NEW-DIST
                           SET DIRECTION(LS-NEIGHBOR-ROW,
                               LS-NEIGHBOR-COL) TO "<"
                       END-IF
                   END-IF
               END-IF

      *> Remove this node from the unvisited
               PERFORM VARYING UNVISITED-INDEX FROM 1 BY 1 UNTIL
                   UNVISITED-INDEX = UNVISITED-SIZE
                   SET UNVISITED-ROW(UNVISITED-INDEX) TO
                       UNVISITED-ROW(UNVISITED-INDEX + 1)
                   SET UNVISITED-COL(UNVISITED-INDEX) TO
                       UNVISITED-COL(UNVISITED-INDEX + 1)
                   SET UNVISITED-DIST-FROM-START(UNVISITED-INDEX) TO
                       UNVISITED-DIST-FROM-START(UNVISITED-INDEX + 1)
               END-PERFORM
               ADD -1 TO UNVISITED-SIZE
           END-PERFORM

           GOBACK.
       END PROGRAM PROCESS-GRID.

      *> ===============================================================
      *> FIND-UNVISITED-NODE.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-UNVISITED-NODE.
       DATA DIVISION.

       LOCAL-STORAGE SECTION.

       LINKAGE SECTION.
       COPY "unvisited" IN "16".
       01  IN-ROW                 PIC 9(3).
       01  IN-COL                 PIC 9(3).
       01  OUT-INDEX              PIC 9(5).

       PROCEDURE DIVISION USING BY REFERENCE
           UNVISITED-GRP
           IN-ROW
           IN-COL
           OUT-INDEX.
           PERFORM VARYING OUT-INDEX FROM 1 BY 1
               UNTIL OUT-INDEX > UNVISITED-SIZE
               IF UNVISITED-ROW(OUT-INDEX) = IN-ROW
                   AND UNVISITED-COL(OUT-INDEX) = IN-COL
                   GOBACK
               END-IF
           END-PERFORM

           SET OUT-INDEX TO 0

           GOBACK.
       END PROGRAM FIND-UNVISITED-NODE.

      *> ===============================================================
      *> DISPLAY-GRID.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-GRID.

       DATA DIVISION.

       LINKAGE SECTION.
       COPY "grid" IN "16".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.
           DISPLAY "Start: " START-ROW "," START-COL
           DISPLAY "Current: " CUR-ROW "," CUR-COL
           DISPLAY "End: " END-ROW "," END-COL
           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-SIZE
                   IF DIRECTION(GRID-ROW-INDEX, GRID-COL-INDEX)
                       NOT = SPACE
                       
                       DISPLAY DIRECTION(
                           GRID-ROW-INDEX,GRID-COL-INDEX)
                           NO ADVANCING
                   ELSE
                       DISPLAY GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                           NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY "|"
           END-PERFORM

           GOBACK.
       END PROGRAM DISPLAY-GRID.
