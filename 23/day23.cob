       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY23.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
       01  LS-FILE-PATH              PIC X(30).

       PROCEDURE DIVISION.

           ACCEPT LS-FILE-PATH FROM COMMAND-LINE

           CALL "PARSE-FILE" USING
               BY REFERENCE LS-FILE-PATH.
       END PROGRAM DAY23.

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
       01  F-FILE-RECORD                               PIC X(5).

       LOCAL-STORAGE SECTION.
       01  LS-LINE                                     PIC X(5).
       01  LS-LEFT                                     PIC X(2).
       01  LS-RIGHT                                    PIC X(2).
       01  LS-T-PARTY-COUNT                            PIC 9(6) VALUE 0.
       COPY "computer" IN "23".
       COPY "party" IN "23".
       LINKAGE SECTION.
       01  IN-FILE-PATH                                PIC X(30).

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
                       UNSTRING LS-LINE DELIMITED BY "-"
                           INTO LS-LEFT LS-RIGHT
                       END-UNSTRING
                       CALL "ADD-PAIR" USING
                           COMPUTER-GRP
                           PARTIES-GRP
                           LS-LEFT
                           LS-RIGHT
                       ADD RETURN-CODE TO LS-T-PARTY-COUNT
                       CALL "ADD-PAIR" USING
                           COMPUTER-GRP
                           PARTIES-GRP
                           LS-RIGHT
                           LS-LEFT
                       ADD RETURN-CODE TO LS-T-PARTY-COUNT
           END-PERFORM
           CLOSE FD-DATA
           SORT COMPUTERS
           CALL "DISPLAY-COMPUTERS" USING
               COMPUTER-GRP
           CALL "DISPLAY-PARTIES" USING
               PARTIES-GRP
           DISPLAY LS-T-PARTY-COUNT " parties with the t computer"
           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> ADD-PAIR.
      *> Returns the number of new parties discovered with a computer
      *> whose name starts with t.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-PAIR.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-COMPUTER-IDX                             PIC 9(3).
       01  LS-COMPUTER-LINKS-IDX                       PIC 9(3).
       01  LS-POSSIBLE-LINK-NAME                       PIC X(2).
       01  LS-PARTY-GRP.
           05 LS-PARTY OCCURS 3 TIMES
               ASCENDING KEY IS LS-PARTY-COMPUTER-NAME
               INDEXED BY LS-PARTY-IDX.
               10  LS-PARTY-COMPUTER-NAME              PIC X(2).
       01  LS-PARTY-STR                                PIC X(6).
       01  LS-NEW-T-PARTY-COUNT                        PIC 9(3) VALUE 0.
       LINKAGE SECTION.
       COPY "computer" IN "23".
       COPY "party" IN "23".
       01  IN-LEFT                                     PIC X(2).
       01  IN-RIGHT                                    PIC X(2).

       PROCEDURE DIVISION USING BY REFERENCE
           COMPUTER-GRP
           PARTIES-GRP
           IN-LEFT
           IN-RIGHT
           .
           SET COMPUTER-IDX TO 1
           SEARCH COMPUTERS 
               VARYING COMPUTER-IDX
               AT END
                   ADD 1 TO COMPUTERS-SIZE
                   SET COMPUTER-LINKS-SIZE(COMPUTERS-SIZE) TO 0
                   SET COMPUTER-NAME(COMPUTERS-SIZE) TO IN-LEFT

               WHEN COMPUTER-NAME(COMPUTER-IDX) = IN-LEFT
                   CONTINUE
           END-SEARCH
      *> Check if any of the computers linked to IN-LEFT are
      *> linked to IN-RIGHT. If so, we found a group of 3.

      *> Browse other computers connected to IN-LEFT:
           PERFORM VARYING LS-COMPUTER-LINKS-IDX FROM 1 BY 1
               UNTIL LS-COMPUTER-LINKS-IDX >
                   COMPUTER-LINKS-SIZE(COMPUTER-IDX)
      *> For a given computer connected to IN-LEFT, look up
      *> an entry in our computers:
               SET LS-POSSIBLE-LINK-NAME TO COMPUTER-LINK-NAME(
                   COMPUTER-IDX,
                   LS-COMPUTER-LINKS-IDX
               )
               PERFORM VARYING LS-COMPUTER-IDX FROM 1 BY 1
                   UNTIL LS-COMPUTER-IDX > COMPUTERS-SIZE
                   IF COMPUTER-NAME(LS-COMPUTER-IDX) =
                       LS-POSSIBLE-LINK-NAME

      *> Now see if this computer is connected to IN-RIGHT
                       PERFORM VARYING COMPUTER-LINKS-IDX FROM 1 BY 1
                           UNTIL COMPUTER-LINKS-IDX >
                               COMPUTER-LINKS-SIZE(LS-COMPUTER-IDX)
                           IF COMPUTER-LINK-NAME(
                               LS-COMPUTER-IDX,
                               COMPUTER-LINKS-IDX
                           ) = IN-RIGHT
                               SET LS-PARTY-COMPUTER-NAME(1) TO IN-LEFT
                               SET LS-PARTY-COMPUTER-NAME(2) TO IN-RIGHT
                               SET LS-PARTY-COMPUTER-NAME(3) TO 
                                   COMPUTER-LINK-NAME(
                                       COMPUTER-IDX,
                                       LS-COMPUTER-LINKS-IDX
                                   )
                               SORT LS-PARTY
                               STRING LS-PARTY-COMPUTER-NAME(1)
                                   LS-PARTY-COMPUTER-NAME(2)
                                   LS-PARTY-COMPUTER-NAME(3)
                                   INTO LS-PARTY-STR
                               END-STRING
                               SET PARTIES-IDX TO 1
                               SEARCH ALL PARTIES
                                   AT END
                                       ADD 1 TO PARTIES-SIZE
                                       SET PARTY(PARTIES-SIZE) TO
                                           LS-PARTY-STR
                                       IF LS-PARTY-COMPUTER-NAME(1)(1:1)
                                          = "t" OR
                                          LS-PARTY-COMPUTER-NAME(2)(1:1)
                                          = "t" OR
                                          LS-PARTY-COMPUTER-NAME(3)(1:1)
                                          = "t"
                                           ADD 1 TO LS-NEW-T-PARTY-COUNT
                                       END-IF
                                       SORT PARTIES
                                       EXIT PERFORM

                                   WHEN PARTY(PARTIES-IDX) =
                                       LS-PARTY-STR
                                       EXIT PERFORM
                               END-SEARCH
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           
           END-PERFORM

      *> Add IN-RIGHT to the list of computers linked to IN-LEFT.
           ADD 1 TO COMPUTER-LINKS-SIZE(COMPUTER-IDX)
           SET COMPUTER-LINK-NAME(
               COMPUTER-IDX,
               COMPUTER-LINKS-SIZE(COMPUTER-IDX)
           ) TO IN-RIGHT

           CALL "SORT-LINKS" USING
               COMPUTERS(COMPUTER-IDX)

           MOVE LS-NEW-T-PARTY-COUNT TO RETURN-CODE
           .

       END PROGRAM ADD-PAIR.

      *> ===============================================================
      *> SORT-LINKS.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-LINKS.
       DATA DIVISION.
       LINKAGE SECTION.
       01  COMPUTER-LINKS-GRP.
           05  COMPUTER-NAME                         PIC X(2).
           05  COMPUtER-LINKS-SIZE                   PIC 9(3).
           05  COMPUTER-LINKS OCCURS 1 TO 999 TIMES
               DEPENDING ON COMPUTER-LINKS-SIZE
               ASCENDING KEY IS COMPUTER-LINK-NAME
               INDEXED BY COMPUTER-LINKS-IDX.
               10  COMPUTER-LINK-NAME                 PIC X(2).

       PROCEDURE DIVISION USING BY REFERENCE
           COMPUTER-LINKS-GRP.

           SORT COMPUTER-LINKS
           .

       END PROGRAM SORT-LINKS.


      *> ===============================================================
      *> DISPLAY-COMPUTERS.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-COMPUTERS.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "computer" IN "23".
       PROCEDURE DIVISION USING BY REFERENCE
           COMPUTER-GRP.

           DISPLAY "-------------------------"
           PERFORM VARYING COMPUTER-IDX FROM 1 BY 1
               UNTIL COMPUTER-IDX > COMPUTERS-SIZE
               DISPLAY COMPUTER-IDX ": "
                   COMPUTER-NAME(COMPUTER-IDX) ":" NO ADVANCING
               PERFORM VARYING COMPUTER-LINKS-IDX FROM 1 BY 1
                   UNTIL COMPUTER-LINKS-IDX >
                       COMPUTER-LINKS-SIZE(COMPUTER-IDX)
                   DISPLAY COMPUTER-LINK-NAME(
                       COMPUTER-IDX,
                       COMPUTER-LINKS-IDX
                   ) "," NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM
           DISPLAY "-------------------------"
           .
       END PROGRAM DISPLAY-COMPUTERS.

      *> ===============================================================
      *> DISPLAY-PARTIES.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-PARTIES.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "party" IN "23".
       PROCEDURE DIVISION USING BY REFERENCE
           PARTIES-GRP.

           DISPLAY "-------------------------"
           PERFORM VARYING PARTIES-IDX FROM 1 BY 1
               UNTIL PARTIES-IDX > PARTIES-SIZE
               DISPLAY PARTY(PARTIES-IDX)
           END-PERFORM
           DISPLAY "-------------------------"
           .
       END PROGRAM DISPLAY-PARTIES.
