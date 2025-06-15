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

           CALL "TRACE-PATHS" USING BY REFERENCE
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

       WORKING-STORAGE SECTION.
       COPY "constants" IN "16".

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
      *> Set the character in the grid cell.
                           SET GRID-CELL(GRID-ROW-INDEX,GRID-COL-INDEX)
                               TO LS-LINE(GRID-COL-INDEX:1)
      *> Set the distance for all directions coming into this node
      *> to infinity.
                           PERFORM VARYING DIRECTION-INDEX
                               FROM 1 BY 1 UNTIL DIRECTION-INDEX > 4
                               SET DIST-THRU-PARENT(
                                   GRID-ROW-INDEX,
                                   GRID-COL-INDEX,
                                   DIRECTION-INDEX
                               ) TO C-INFINITY
                           END-PERFORM

      *> Look for special nodes (start, finish).
                           EVALUATE LS-LINE(GRID-COL-INDEX:1)
                               WHEN "S"
                                   SET START-ROW TO GRID-ROW-INDEX
                                   SET START-COL TO GRID-COL-INDEX
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
       COPY "constants" IN "16".

       LOCAL-STORAGE SECTION.
       01  LS-CUR-ROW                           PIC 9(3).
       01  LS-CUR-COL                           PIC 9(3).
       01  LS-CUR-DIR                           PIC 9(1).
       01  LS-NEIGHBOR-ROW                      PIC 9(3).
       01  LS-NEIGHBOR-COL                      PIC 9(3).
       01  LS-NEIGHBOR-DIR                      PIC 9(1).
       01  LS-NEIGHBOR-NEW-DIST                 PIC 9(6).
       01  LS-NEIGHBOR-UNVISITED-INDEX          PIC 9(6).
       COPY "unvisited" IN "16".

       LINKAGE SECTION.
       COPY "grid" IN "16".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.
      *> Fill the unvisited set with all the nodes except the start node.
      *> Set their distance to infinity.
      *> Add unvisited nodes for all 4 directions for each node.
           SET UNVISITED-INDEX TO 0
           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1 UNTIL
               GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1 UNTIL
                   GRID-COL-INDEX > GRID-SIZE
                   EVALUATE GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                       WHEN "."
                       WHEN "E"
                           PERFORM VARYING DIRECTION-INDEX
                               FROM 1 BY 1 UNTIL DIRECTION-INDEX > 4
                               ADD 1 TO UNVISITED-SIZE
                               SET UNVISITED-ROW(UNVISITED-SIZE) TO
                                   GRID-ROW-INDEX
                               SET UNVISITED-COL(UNVISITED-SIZE) TO
                                   GRID-COL-INDEX
                               SET UNVISITED-DIST-FROM-START(
                                   UNVISITED-SIZE) TO C-INFINITY
                               SET UNVISITED-DIR(UNVISITED-SIZE)
                                   TO DIRECTION-INDEX
                           END-PERFORM
                   END-EVALUATE
               END-PERFORM
           END-PERFORM
      *> Add the start node with a distance of 0, and just
      *> one direction, right.
           ADD 1 TO UNVISITED-SIZE
           SET UNVISITED-ROW(UNVISITED-SIZE) TO START-ROW
           SET UNVISITED-COL(UNVISITED-SIZE) TO START-COL
           SET UNVISITED-DIR(UNVISITED-SIZE) TO C-RIGHT
           SET UNVISITED-DIST-FROM-START(UNVISITED-SIZE) TO 0
           SORT UNVISITED

      *> Continue until all nodes have been visited or until
      *> we only have unreachable nodes.
           PERFORM UNTIL UNVISITED-SIZE = 0
               SORT UNVISITED
      *> Unreachable nodes only:
               IF UNVISITED-DIST-FROM-START(1) = C-INFINITY
                   EXIT PERFORM
               END-IF

               SET LS-CUR-ROW TO UNVISITED-ROW(1)
               SET LS-CUR-COL TO UNVISITED-COL(1)
               SET LS-CUR-DIR TO UNVISITED-DIR(1)

               PERFORM VARYING LS-NEIGHBOR-DIR FROM 1 BY 1
                   UNTIL LS-NEIGHBOR-DIR > 4
                   SET LS-NEIGHBOR-ROW TO LS-CUR-ROW
                   SET LS-NEIGHBOR-COL TO LS-CUR-COL
                   EVALUATE LS-NEIGHBOR-DIR
                       WHEN C-TOP
                           COMPUTE LS-NEIGHBOR-ROW = LS-CUR-ROW - 1
                       WHEN C-RIGHT
                           COMPUTE LS-NEIGHBOR-COL = LS-CUR-COL + 1
                       WHEN C-BOTTOM
                           COMPUTE LS-NEIGHBOR-ROW = LS-CUR-ROW + 1
                       WHEN C-LEFT
                           COMPUTE LS-NEIGHBOR-COL = LS-CUR-COL - 1
                   END-EVALUATE
                   SET LS-NEIGHBOR-NEW-DIST
                       TO UNVISITED-DIST-FROM-START(1)
                   IF GRID-CELL(LS-NEIGHBOR-ROW, LS-NEIGHBOR-COL)
                           NOT = "#"
      *> Find the index of this neighbor in the unvisited set
                       CALL "FIND-UNVISITED-NODE" USING BY REFERENCE
                           UNVISITED-GRP
                           LS-NEIGHBOR-ROW
                           LS-NEIGHBOR-COL
                           LS-NEIGHBOR-DIR
                           LS-NEIGHBOR-UNVISITED-INDEX
                       IF LS-NEIGHBOR-UNVISITED-INDEX > 0
      *> Calculate rotation cost
                           EVALUATE LS-CUR-DIR ALSO LS-NEIGHBOR-DIR
                               WHEN C-TOP ALSO C-BOTTOM
                               WHEN C-RIGHT ALSO C-LEFT
                               WHEN C-BOTTOM ALSO C-TOP
                               WHEN C-LEFT ALSO C-RIGHT
                                   ADD 2000 TO LS-NEIGHBOR-NEW-DIST
                               WHEN C-TOP ALSO C-RIGHT
                               WHEN C-TOP ALSO C-LEFT
                               WHEN C-RIGHT ALSO C-TOP
                               WHEN C-RIGHT ALSO C-BOTTOM
                               WHEN C-BOTTOM ALSO C-RIGHT
                               WHEN C-BOTTOM ALSO C-LEFT
                               WHEN C-LEFT ALSO C-TOP
                               WHEN C-LEFT ALSO C-BOTTOM
                                   ADD 1000 TO LS-NEIGHBOR-NEW-DIST
                           END-EVALUATE
                           ADD 1 TO LS-NEIGHBOR-NEW-DIST
      *> Update distance if it's shorter
                           IF LS-NEIGHBOR-NEW-DIST <=
                               UNVISITED-DIST-FROM-START(
                               LS-NEIGHBOR-UNVISITED-INDEX
                           )
                               SET UNVISITED-DIST-FROM-START(
                                   LS-NEIGHBOR-UNVISITED-INDEX
                               ) TO LS-NEIGHBOR-NEW-DIST
                               CALL "ADD-PARENT" USING BY REFERENCE
                                   GRID-GRP
                                   LS-NEIGHBOR-ROW
                                   LS-NEIGHBOR-COL
                                   LS-CUR-ROW
                                   LS-CUR-COL
                                   LS-NEIGHBOR-DIR
                                   LS-NEIGHBOR-NEW-DIST
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *> Remove this node from the unvisited
               PERFORM VARYING UNVISITED-INDEX FROM 1 BY 1 UNTIL
                   UNVISITED-INDEX = UNVISITED-SIZE
                   SET UNVISITED-ROW(UNVISITED-INDEX) TO
                       UNVISITED-ROW(UNVISITED-INDEX + 1)
                   SET UNVISITED-COL(UNVISITED-INDEX) TO
                       UNVISITED-COL(UNVISITED-INDEX + 1)
                   SET UNVISITED-DIR(UNVISITED-INDEX) TO
                       UNVISITED-DIR(UNVISITED-INDEX + 1)
                   SET UNVISITED-DIST-FROM-START(UNVISITED-INDEX) TO
                       UNVISITED-DIST-FROM-START(UNVISITED-INDEX + 1)
               END-PERFORM
               ADD -1 TO UNVISITED-SIZE
           END-PERFORM
           GOBACK.
       END PROGRAM PROCESS-GRID.

      *> ===============================================================
      *> FIND-UNVISITED-NODE.
      *>
      *> Return the index of the node in the unvisited nodes set
      *> which matches the given lookup (row, column direction).
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-UNVISITED-NODE.
       DATA DIVISION.

       LOCAL-STORAGE SECTION.

       LINKAGE SECTION.
       COPY "unvisited" IN "16".
       01  IN-ROW                 PIC 9(3).
       01  IN-COL                 PIC 9(3).
       01  IN-DIR                 PIC 9(1).
       01  OUT-INDEX              PIC 9(6).

       PROCEDURE DIVISION USING BY REFERENCE
           UNVISITED-GRP
           IN-ROW
           IN-COL
           IN-DIR
           OUT-INDEX.

           PERFORM VARYING OUT-INDEX FROM 1 BY 1
               UNTIL OUT-INDEX > UNVISITED-SIZE
               IF UNVISITED-ROW(OUT-INDEX) = IN-ROW
                   AND UNVISITED-COL(OUT-INDEX) = IN-COL
                   AND UNVISITED-DIR(OUT-INDEX) = IN-DIR
                   GOBACK
               END-IF
           END-PERFORM

           SET OUT-INDEX TO 0

           GOBACK.
       END PROGRAM FIND-UNVISITED-NODE.

      *> ===============================================================
      *> ADD-PARENT.
      *>
      *> Set the parent for a given node, accessing the given node
      *> from a specific direction.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-PARENT.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "grid" IN "16".
       01  IN-ROW                              PIC 9(3).
       01  IN-COL                              PIC 9(3).
       01  IN-PARENT-ROW                       PIC 9(3).
       01  IN-PARENT-COL                       PIC 9(3).
       01  IN-PARENT-DIR                       PIC 9(1).
       01  IN-DIST-THRU-PARENT                 PIC 9(6).

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP
           IN-ROW
           IN-COL
           IN-PARENT-ROW
           IN-PARENT-COL
           IN-PARENT-DIR
           IN-DIST-THRU-PARENT.


           SET PARENT-ROW(
               IN-ROW, IN-COL, IN-PARENT-DIR) TO IN-PARENT-ROW
           SET PARENT-COL(
               IN-ROW, IN-COL, IN-PARENT-DIR) TO IN-PARENT-COL
           SET DIST-THRU-PARENT(
               IN-ROW, IN-COL, IN-PARENT-DIR) TO IN-DIST-THRU-PARENT

           GOBACK.
       END PROGRAM ADD-PARENT.

      *> ===============================================================
      *> TRACE-PATHS.
      *>
      *> Starting from the end node, backtrack via the parents, marking
      *> O along all the cells in the paths which have the lowest cost.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRACE-PATHS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PUSH-TO-STACK
           FUNCTION POP-STACK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "constants" IN "16".
       LOCAL-STORAGE SECTION.
       01  LS-POP-RESULT               PIC 9(1).
       01  LS-PUSH-RESULT              PIC 9(1).
       01  LS-CUR-ROW                  PIC 9(3).
       01  LS-CUR-COL                  PIC 9(3).
       01  LS-CUR-DIR                  PIC 9(1).
       01  LS-NEXT-ROW                 PIC 9(3).
       01  LS-NEXT-COL                 PIC 9(3).
       01  LS-NODE-COUNT               PIC 9(6) VALUE 0.
       01  LS-SHORTEST-PATH-COST       PIC 9(6) VALUE 999999.
       01  LS-NEXT-COST                PIC 9(4).
       COPY "stack" IN "16".

       LINKAGE SECTION.
       COPY "grid" IN "16".

       PROCEDURE DIVISION USING BY REFERENCE
           GRID-GRP.

      *> Find shortest path cost for the whole puzzle.
           SET LS-CUR-ROW TO END-ROW
           SET LS-CUR-COL TO END-COL
           PERFORM VARYING DIRECTION-INDEX
               FROM 1 BY 1 UNTIL DIRECTION-INDEX > 4

               IF DIST-THRU-PARENT(LS-CUR-ROW, LS-CUR-COL,
                   DIRECTION-INDEX)
                   < LS-SHORTEST-PATH-COST
                   SET LS-SHORTEST-PATH-COST TO
                      DIST-THRU-PARENT(LS-CUR-ROW, LS-CUR-COL,
                           DIRECTION-INDEX)
               END-IF
           END-PERFORM
           DISPLAY "Shortest path: " LS-SHORTEST-PATH-COST
      *> Add any terminal node + direction which have the
      *> shortest path.
           PERFORM VARYING DIRECTION-INDEX
               FROM 1 BY 1 UNTIL DIRECTION-INDEX > 4

               IF DIST-THRU-PARENT(
                   LS-CUR-ROW, LS-CUR-COL, DIRECTION-INDEX
               ) = LS-SHORTEST-PATH-COST
                   SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                       PARENT-ROW(
                           LS-CUR-ROW,
                           LS-CUR-COL,
                           DIRECTION-INDEX
                       )
                       PARENT-COL(
                           LS-CUR-ROW,
                           LS-CUR-COL,
                           DIRECTION-INDEX
                       )
                       DIRECTION-INDEX
                       LS-CUR-ROW
                       LS-CUR-COL
                       STACK-GRP
                   )
               END-IF
           END-PERFORM

      *> Continue processing parent nodes until none are left.
           PERFORM UNTIL STACK-SIZE = 0
               SET LS-POP-RESULT TO POP-STACK(
                   STACK-GRP
                   LS-CUR-ROW
                   LS-CUR-COL
                   LS-CUR-DIR
                   LS-NEXT-ROW
                   LS-NEXT-COL
               )
               IF LS-POP-RESULT = 1
                   EXIT PERFORM
               END-IF

      *> Mark an O for this node: if we popped it from the
      *> stack, we know it's on a shortest path.
               SET GRID-CELL(LS-CUR-ROW, LS-CUR-COL) TO "O"

      *> Calculate the shortest path up to this node,
      *> and including one additional step (to the next node).
               SET LS-SHORTEST-PATH-COST TO C-INFINITY

      *> Check the parents coming in to our node from all
      *> 4 directions.
               PERFORM VARYING DIRECTION-INDEX
                   FROM 1 BY 1 UNTIL DIRECTION-INDEX > 4

      * Calculate the cost of any turn to the next node, given
      * we've arrived inside this node, from the previous one,
      * in a specific direction.
                   SET LS-NEXT-COST TO 1
                   IF (DIRECTION-INDEX = C-TOP
                       OR DIRECTION-INDEX = C-BOTTOM)
                       AND LS-NEXT-COL NOT = LS-CUR-COL
                       ADD 1000 TO LS-NEXT-COST
                   END-IF
                   IF (DIRECTION-INDEX = C-LEFT
                       OR DIRECTION-INDEX = C-RIGHT)
                       AND LS-NEXT-ROW NOT = LS-CUR-ROW
                       ADD 1000 TO LS-NEXT-COST
                   END-IF


      *> If the path to this node from the start, plus the cost
      *> of going to the next node, is smaller than the
      *> shortest path we've calculated for this node, update the
      *> shortest path cost.
                   IF DIST-THRU-PARENT(
                       LS-CUR-ROW,
                       LS-CUR-COL,
                       DIRECTION-INDEX
                   ) + LS-NEXT-COST < LS-SHORTEST-PATH-COST
                       COMPUTE LS-SHORTEST-PATH-COST =
                           DIST-THRU-PARENT(
                               LS-CUR-ROW,
                               LS-CUR-COL,
                               DIRECTION-INDEX
                           ) + LS-NEXT-COST
                   END-IF
               END-PERFORM
      *> Go through all directions coming into this node:
      *> If the cost from the start to the parent of this direction,
      *> plus the cost of advancing to the next node, is
      *> the shortest cost for this node, add the parent node
      *> to the stack for backtracking

               PERFORM VARYING DIRECTION-INDEX
                   FROM 1 BY 1 UNTIL DIRECTION-INDEX > 4
      *> Calculate the cost of going to the next node, starting
      *> from the given direction.
                   SET LS-NEXT-COST TO 1
                   IF (DIRECTION-INDEX = C-TOP
                       OR DIRECTION-INDEX = C-BOTTOM)
                       AND LS-NEXT-COL NOT = LS-CUR-COL
                       ADD 1000 TO LS-NEXT-COST
                   END-IF
                   IF (DIRECTION-INDEX = C-LEFT
                       OR DIRECTION-INDEX = C-RIGHT)
                       AND LS-NEXT-ROW NOT = LS-CUR-ROW
                       ADD 1000 TO LS-NEXT-COST
                   END-IF

      *> Compare the cost from the start up to this node, plus
      *> advancing to the next node. If it's the shortest possible
      *> value, add this parent to the stack for backtracking.
                   IF DIST-THRU-PARENT(
                       LS-CUR-ROW,
                       LS-CUR-COL,
                       DIRECTION-INDEX
                   ) + LS-NEXT-COST = LS-SHORTEST-PATH-COST
                       SET LS-PUSH-RESULT TO PUSH-TO-STACK(
                           PARENT-ROW(
                               LS-CUR-ROW,
                               LS-CUR-COL,
                               DIRECTION-INDEX
                           )
                           PARENT-COL(
                               LS-CUR-ROW,
                               LS-CUR-COL,
                               DIRECTION-INDEX
                           )
                           DIRECTION-INDEX
                           LS-CUR-ROW
                           LS-CUR-COL
                           STACK-GRP
                       )
                   END-IF
               END-PERFORM
           END-PERFORM

      *> Count all the nodes where we placed a O.
      *> This is the answer to part 2.
           SET LS-NODE-COUNT TO 1
           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-SIZE
                   IF GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX) = "O"
                       ADD 1 TO LS-NODE-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM
           DISPLAY "Cells in shortest paths: " LS-NODE-COUNT
           GOBACK.
       END PROGRAM TRACE-PATHS.


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
           DISPLAY "End: " END-ROW "," END-COL
           PERFORM VARYING GRID-ROW-INDEX FROM 1 BY 1
               UNTIL GRID-ROW-INDEX > GRID-SIZE
               PERFORM VARYING GRID-COL-INDEX FROM 1 BY 1
                   UNTIL GRID-COL-INDEX > GRID-SIZE
                       DISPLAY GRID-CELL(GRID-ROW-INDEX, GRID-COL-INDEX)
                           NO ADVANCING
               END-PERFORM
               DISPLAY "|"
           END-PERFORM

           GOBACK.
       END PROGRAM DISPLAY-GRID.

      *> ===============================================================
      *> POP-STACK.
      *> Remove the last item of the stack.
      *> Return 0 if an item was popped, 1 if the stack was empty.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. POP-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "stack" IN "16".
       01  OUT-ITEM-ROW                    PIC 9(3).
       01  OUT-ITEM-COL                    PIC 9(3).
       01  OUT-ITEM-DIR                    PIC 9(1).
       01  OUT-NEXT-ROW                    PIC 9(3).
       01  OUT-NEXT-COL                    PIC 9(3).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE STACK-GRP OUT-ITEM-ROW OUT-ITEM-COL OUT-ITEM-DIR
           OUT-NEXT-ROW OUT-NEXT-COL
           RETURNING OUT-RESULT
           .

           IF STACK-SIZE > 0
               MOVE STACK-ITEM-ROW(STACK-SIZE) TO OUT-ITEM-ROW
               MOVE STACK-ITEM-COL(STACK-SIZE) TO OUT-ITEM-COL
               MOVE STACK-ITEM-DIR(STACK-SIZE) TO OUT-ITEM-DIR
               MOVE STACK-NEXT-ROW(STACK-SIZE) TO OUT-NEXT-ROW
               MOVE STACK-NEXT-COL(STACK-SIZE) TO OUT-NEXT-COL
               COMPUTE STACK-SIZE = STACK-SIZE - 1
               MOVE 0 TO OUT-RESULT
           ELSE
               MOVE 1 TO OUT-RESULT
           END-IF
           GOBACK.

       END FUNCTION POP-STACK.

      *> ===============================================================
      *> PUSH-TO-STACK.
      *> Add an item to the end of the stack
      *> ===============================================================
       IDENTIFICATION DIVISION.
       FUNCTION-ID. PUSH-TO-STACK.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ITEM-ROW                     PIC 9(3).
       01  IN-ITEM-COL                     PIC 9(3).
       01  IN-ITEM-DIR                     PIC 9(1).
       01  IN-NEXT-ROW                     PIC 9(3).
       01  IN-NEXT-COL                     PIC 9(3).
       COPY "stack" IN "16".
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ITEM-ROW IN-ITEM-COL IN-ITEM-DIR
           IN-NEXT-ROW IN-NEXT-COL STACK-GRP
           RETURNING OUT-RESULT.

           ADD 1 TO STACK-SIZE
           SET STACK-ITEM-ROW(STACK-SIZE) TO IN-ITEM-ROW
           SET STACK-ITEM-COL(STACK-SIZE) TO IN-ITEM-COL
           SET STACK-ITEM-DIR(STACK-SIZE) TO IN-ITEM-DIR
           SET STACK-NEXT-ROW(STACK-SIZE) TO IN-NEXT-ROW
           SET STACK-NEXT-COL(STACK-SIZE) TO IN-NEXT-COL

           MOVE 0 TO OUT-RESULT
           GOBACK.
       END FUNCTION PUSH-TO-STACK.

