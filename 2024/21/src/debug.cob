      *> ===============================================================
      *> DISPLAY-KEYPAD.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-KEYPAD.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-KEY-SEQUENCE-IDX       PIC 9(3).
       LINKAGE SECTION.
       COPY "keypad" IN "21/src".
       01  IN-KP-IDX                 PIC 9(1) VALUE 1.

       PROCEDURE DIVISION USING BY REFERENCE
           KP-GRP
           IN-KP-IDX.
           DISPLAY "Keypad #" IN-KP-IDX
           DISPLAY "@" KP-KEY(
               IN-KP-IDX,
               KP-CUR-ROW(IN-KP-IDX),
               KP-CUR-COL(IN-KP-IDX)
           )
           DISPLAY "Hit " KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX) " keys."
           PERFORM VARYING LS-KEY-SEQUENCE-IDX FROM 1 BY 1
               UNTIL LS-KEY-SEQUENCE-IDX >
                   KP-KEY-SEQUENCE-LENGTH(IN-KP-IDX)
               DISPLAY KP-KEY-SEQUENCE-KEY(
                   IN-KP-IDX, LS-KEY-SEQUENCE-IDX
               ) NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           DISPLAY "---"
           .

       END PROGRAM DISPLAY-KEYPAD.

