       01  PROG-GRP.
           05  PROG-REG-A            PIC 9(16) COMP.
           05  PROG-REG-B            PIC 9(16) COMP.
           05  PROG-REG-C            PIC 9(16) COMP.
           05  PROG-SIZE             PIC 9(2).
           05  PROG-INSTR-PTR        PIC 9(16).
           05  PROG-ITEMS OCCURS 1 TO 24 TIMES
               DEPENDING ON PROG-SIZE.
               10  PROG-ITEM         PIC 9(1).
