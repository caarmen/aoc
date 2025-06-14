       01  WIRE-GRP.
           05  WIRE-SIZE                    PIC 9(3) VALUE 0.
           05  WIRE-INPUT-BIT-SIZE          PIC 9(2).
           05  WIRES OCCURS 1 TO 999 TIMES
               DEPENDING ON WIRE-SIZE
               ASCENDING KEY IS WIRE-NAME
               INDEXED BY WIRE-IDX
               .
               10  WIRE-NAME                PIC X(3).
               10  WIRE-INPUT-1             PIC X(3).
               10  WIRE-INPUT-2             PIC X(3).
               10  WIRE-OUTPUT              PIC 9(1).
               10  WIRE-GATE                PIC 9(1).
                
