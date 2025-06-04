      *> ===============================================================
      *> VISIT.
      *> Return 0 if the node has just been marked as visited, 1 if it
      *> was already marked as visited.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VISIT-STEP.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "visitedstep" IN "21/src".
       01  IN-ROW                           PIC 9(1).
       01  IN-COL                           PIC 9(1).
       01  IN-MOV-HIST                      PIC X(100).
       PROCEDURE DIVISION USING BY REFERENCE
           VISITED-GRP
           IN-ROW
           IN-COL
           IN-MOV-HIST
           .

           PERFORM VARYING VISITED-INDEX FROM 1 BY 1
               UNTIL VISITED-INDEX > VISITED-SIZE

      *> If, for the given keys already pressed,
      *> we've already navigated to this row,col
      *> with the same movement history, OR with a shorter movement
      *> history, return 1, as we don't want to visit this
      *> cell again.
       
               IF VISITED-ROW(VISITED-INDEX) = IN-ROW
                   AND VISITED-COL(VISITED-INDEX) = IN-COL
                   AND (
      *> We've already been exactly here before:
                       VISITED-MOV-HIST(VISITED-INDEX) = IN-MOV-HIST
      *> OR we've already been here with a shorter path before:
                       OR LENGTH OF FUNCTION TRIM(
                           VISITED-MOV-HIST(VISITED-INDEX)
                       ) < LENGTH OF FUNCTION TRIM(IN-MOV-HIST)
                   )
                   MOVE 1 TO RETURN-CODE
                   GOBACK
               END-IF
           END-PERFORM

           IF VISITED-SIZE = 9999999
               DISPLAY "Max visited size"
           END-IF
           ADD 1 TO VISITED-SIZE
           SET VISITED-ROW(VISITED-SIZE) TO IN-ROW
           SET VISITED-COL(VISITED-SIZE) TO IN-COL
           SET VISITED-MOV-HIST(VISITED-SIZE) TO IN-MOV-HIST

           MOVE 0 TO RETURN-CODE
           GOBACK.
       END PROGRAM VISIT-STEP.
