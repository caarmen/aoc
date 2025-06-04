      *> ===============================================================
      *> GET-FROM-CACHE
      *> Read an item from the cache.
      *> Return 0 if the item was found, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-FROM-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "21/src".
       01  IN-CACHE-KEY                    PIC X(100).
       01  OUT-CACHE-VALUE                 PIC 9(16).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           OUT-CACHE-VALUE
           OUT-RESULT.

           SEARCH ALL CACHE-CALCS
               AT END
                   MOVE 1 TO OUT-RESULT
               WHEN CACHE-KEY(CACHE-INDEX) = IN-CACHE-KEY
                   MOVE CACHE-VALUE(CACHE-INDEX) TO OUT-CACHE-VALUE
                   MOVE 0 TO OUT-RESULT

           GOBACK.
       END PROGRAM GET-FROM-CACHE.

      *> ===============================================================
      *> ADD-TO-CACHE
      *> Add an item to the cache
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-TO-CACHE.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cache" IN "21/src".
       01  IN-CACHE-KEY                    PIC X(100).
       01  IN-CACHE-VALUE                  PIC 9(16).
       01  OUT-RESULT                      PIC 9(1).

       PROCEDURE DIVISION USING
           BY REFERENCE
           CACHE-GRP
           IN-CACHE-KEY
           IN-CACHE-VALUE
           OUT-RESULT.

           ADD 1 TO CACHE-SIZE
           SET CACHE-VALUE(CACHE-SIZE) TO IN-CACHE-VALUE
           SET CACHE-KEY(CACHE-SIZE) TO IN-CACHE-KEY

           SORT CACHE-CALCS
           MOVE 0 TO OUT-RESULT
           GOBACK.
       END PROGRAM ADD-TO-CACHE.
