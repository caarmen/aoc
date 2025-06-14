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
       01  LS-COMPUTER-IDX                             PIC 9(3).
       01  LS-PARTY                                    PIC X(50).
       01  LS-BIGGEST-PARTY                            PIC X(50) VALUE
                                                           SPACES.
       01  LS-STR-PTR                                  PIC 9(2).
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

      *> Part 1: parties of 3 computers, with a computer starting with
      *> t:
           DISPLAY LS-T-PARTY-COUNT " parties with the t computer"

      *> Part 2: biggest parties:
      *> Go through our computer table, finding the biggest party
      *> for the computers in each row. Keep track of the biggest
      *> party overall.
           PERFORM VARYING LS-COMPUTER-IDX FROM 1 BY 1
               UNTIL LS-COMPUTER-IDX > COMPUTERS-SIZE
               CALL "FIND-BIGGEST-PARTY" USING
                   COMPUTER-GRP
                   LS-COMPUTER-IDX
                   LS-PARTY
               IF LENGTH OF FUNCTION TRIM(LS-PARTY) > LENGTH OF FUNCTION
                   TRIM(LS-BIGGEST-PARTY)
                   SET LS-BIGGEST-PARTY TO LS-PARTY
               END-IF
           END-PERFORM

           DISPLAY "Biggest party: " NO ADVANCING
           PERFORM VARYING LS-STR-PTR FROM 1 BY 1 UNTIL
               LS-STR-PTR > LENGTH OF FUNCTION TRIM(LS-BIGGEST-PARTY)
               DISPLAY LS-BIGGEST-PARTY(LS-STR-PTR:1) NO ADVANCING
               IF FUNCTION MOD(LS-STR-PTR, 2) = 0
                   DISPLAY "," NO ADVANCING
               END-IF
           END-PERFORM
           DISPLAY SPACE
           .
       END PROGRAM PARSE-FILE.

      *> ===============================================================
      *> ADD-PAIR.
      *> Returns the number of new parties (of 3 computers) discovered
      *> with a computer whose name starts with t.
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
      *> FIND-BIGGEST-PARTY.
      *> 
      *> The given row in the computer table at the given index contains
      *> the list of all computers linked to the one with COMPUTER-NAME.
      *> 
      *> Take this COMPUTER-NAME, and all the computers in the
      *> COMPUTER-LINKS, and find the biggest subset of all these
      *> computers which are linked together.
      *>
      *> Return this in OUT-BIGGEST-PARTY.
      *> 
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-BIGGEST-PARTY.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "potentialparty" IN "23".
       01  LS-PARTY                                    PIC X(50).
       01  LS-IS-PARTY                                 PIC 9(1).
       01  LS-VARIATION-COUNT                          PIC 9(18) COMP.
       01  LS-VARIATION                                PIC 9(18) COMP.
       01  LS-VARIATION-BIT-STR                        PIC X(14).
       LINKAGE SECTION.
       COPY "computer" in "23".
       01  IN-COMPUTER-IDX                             PIC 9(3).
       01  OUT-BIGGEST-PARTY                           PIC X(50).
       PROCEDURE DIVISION USING BY REFERENCE
           COMPUTER-GRP
           IN-COMPUTER-IDX
           OUT-BIGGEST-PARTY.

      *> Create a "potential party": a sorted table containing
      *> the COMPUTER-NAME and all linked computers for the given row.
           COMPUTE POTENTIAL-PARTY-SIZE =
               COMPUTER-LINKS-SIZE(IN-COMPUTER-IDX) + 1
           PERFORM VARYING COMPUTER-LINKS-IDX FROM 1 BY 1
               UNTIL COMPUTER-LINKS-IDX >
                   COMPUTER-LINKS-SIZE(IN-COMPUTER-IDX)
                   SET PARTY-COMPUTER-NAME(COMPUTER-LINKS-IDX) TO
                   COMPUTER-LINK-NAME(
                       IN-COMPUTER-IDX,
                       COMPUTER-LINKS-IDX
                   )
           END-PERFORM

           SET PARTY-COMPUTER-NAME(POTENTIAL-PARTY-SIZE) TO
               COMPUTER-NAME(IN-COMPUTER-IDX)
           SORT POTENTIAL-PARTY

      *> Use a bitmask to go through all the combinations of the
      *> different computer names.
      *> If there are a total of 5 computers, we have 2**5 = 32
      *> different combinations The combinations are composed by
      *> including or excluding computers at a given index, based
      *> on the 1, or 0, value of the bit string.
           COMPUTE LS-VARIATION-COUNT = (2**POTENTIAL-PARTY-SIZE) - 1
      *> We start with the value with all 1s, to find the biggest
      *> party first. If there are 5 total computers, this is 11111
      *> (31).
           PERFORM VARYING LS-VARIATION FROM LS-VARIATION-COUNT
               BY -1 UNTIL LS-VARIATION = 0

      *> Get our bitmask representation of which computers to include:
               CALL "TO-BINARY-STRING" USING
                   LS-VARIATION
                   POTENTIAL-PARTY-SIZE
                   LS-VARIATION-BIT-STR

      *> Construct a party string (sequence of computer names)
      *> based on this bitmask.
      *> Ex: if our 5 total computers are ax,bd,ed,ge,qs
      *> and the bitmask is 13 (01101), we create a party string of
      *> bdedqs.
               SET LS-PARTY TO SPACE
               PERFORM VARYING POTENTIAL-PARTY-IDX FROM 1 BY 1
                   UNTIL POTENTIAL-PARTY-IDX > POTENTIAL-PARTY-SIZE
                   IF LS-VARIATION-BIT-STR(POTENTIAL-PARTY-IDX:1) = "1"
                       STRING FUNCTION TRIM(LS-PARTY) 
                           PARTY-COMPUTER-NAME(POTENTIAL-PARTY-IDX)
                           INTO LS-PARTY
                       END-STRING
                   END-IF
               END-PERFORM

      *> If we've confirmed that this is a party (all these computers
      *> are connected to each other), return now. Any future parties
      *> will be smaller (or the same size).
               CALL "IS-PARTY" USING
                   COMPUTER-GRP
                   LS-PARTY
               IF RETURN-CODE = 0
                   SET OUT-BIGGEST-PARTY TO LS-PARTY
                   GOBACK
               END-IF
           END-PERFORM


           SET OUT-BIGGEST-PARTY TO SPACE

           .
       END PROGRAM FIND-BIGGEST-PARTY.

      *> ===============================================================
      *> IS-PARTY.
      *> Return 0 if all computers in the potential party are connected
      *> to each other, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-PARTY.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-PARTY-SIZE                               PIC 9(2).
       01  LS-IDX-1                                    PIC 9(3).
       01  LS-IDX-2                                    PIC 9(3).
       01  LS-LEFT                                     PIC X(2).
       01  LS-RIGHT                                    PIC X(2).
       LINKAGE SECTION.
       COPY "computer" IN "23".
       01  IN-PARTY                                    PIC X(50).

       PROCEDURE DIVISION USING BY REFERENCE
           COMPUTER-GRP
           IN-PARTY.

           COMPUTE LS-PARTY-SIZE = (LENGTH OF FUNCTION TRIM(IN-PARTY))
               / 2

      *> Iterate over all pairs. We call the pair items LS-LEFT and
      *> LS-RIGHT.
           PERFORM VARYING LS-IDX-1 FROM 1 BY 1 UNTIL
               LS-IDX-1 > LS-PARTY-SIZE

               SET LS-LEFT TO IN-PARTY((LS-IDX-1 - 1) * 2 + 1:2)

               COMPUTE LS-IDX-2 = LS-IDX-1 + 1
               PERFORM VARYING LS-IDX-2 FrOM LS-IDX-2 BY 1
                   UNTIL LS-IDX-2 > LS-PARTY-SIZE
                   SET LS-RIGHT TO IN-PARTY((LS-IDX-2 - 1) * 2 + 1:2)

      *> For this given pair, check if they are linked.
      *> To do this: Look up the row in the computer table where
      *> the COMPUTER-NAME is LS-LEFT, and check if LS-RIGHT
      *> is in the COMPUTER-LINKS.
                   SET COMPUTER-IDX TO 1
                   SEARCH ALL COMPUTERS
                       WHEN COMPUTER-NAME(COMPUTER-IDX) = LS-LEFT
                           SET COMPUTER-LINKS-IDX TO 1
                           SEARCH COMPUTER-LINKS
                               VARYING COMPUTER-LINKS-IDX
                               AT END
                                   SET RETURN-CODE TO 1
                                   GOBACK
                               WHEN COMPUTER-LINK-NAME(
                                   COMPUTER-IDX,
                                   COMPUTER-LINKS-IDX
                               ) = LS-RIGHT
                                   CONTINUE
                           END-SEARCH
               END-PERFORM
           END-PERFORM

           SET RETURN-CODE TO 0
           .
       END PROGRAM IS-PARTY.

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
