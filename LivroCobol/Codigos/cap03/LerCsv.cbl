       IDENTIFICATION DIVISION.
           PROGRAM-ID. LerCsv.
           AUTHOR. Fernando Anselmo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CSV ASSIGN TO "entrada.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CSV.
       01 REGISTRO-CSV         PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-FIM-DO-ARQUIVO    PIC X(01) VALUE 'N'.
       01 WS-CONTADOR          PIC 9(05) VALUE ZEROS.
       01 WS-REGISTRO          PIC 9(05) VALUE ZEROS.

       01 WS-DETALHE.
           05 WS-DADO          PIC X(50) OCCURS 3 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT ARQ-CSV
           PERFORM LER-CABECALHO
           PERFORM PROCESSAR-REGISTROS UNTIL WS-FIM-DO-ARQUIVO = 'S'
           CLOSE ARQ-CSV
           STOP RUN.

       LER-CABECALHO.
           READ ARQ-CSV INTO REGISTRO-CSV
               NOT AT END
                   PERFORM SEPARAR-CAMPOS THRU FIM-SEPARAR-CAMPOS
                   DISPLAY "Cabeçalho:"
                   PERFORM EXIBIR THRU FIM-EXIBIR
           END-READ.

       SEPARAR-CAMPOS.
           MOVE SPACES TO WS-DETALHE
           UNSTRING REGISTRO-CSV DELIMITED BY ','
               INTO WS-DADO(1) WS-DADO(2) WS-DADO(3)
           END-UNSTRING.
       FIM-SEPARAR-CAMPOS.
           EXIT.

       PROCESSAR-REGISTROS.
           READ ARQ-CSV INTO REGISTRO-CSV
               AT END
                   MOVE 'S' TO WS-FIM-DO-ARQUIVO
               NOT AT END
                   ADD 1 TO WS-REGISTRO
                   PERFORM SEPARAR-CAMPOS THRU FIM-SEPARAR-CAMPOS
                   DISPLAY "Registro #" WS-REGISTRO ":"
                   PERFORM EXIBIR THRU FIM-EXIBIR
           END-READ.

       EXIBIR.
           PERFORM VARYING WS-CONTADOR FROM 1 BY 1 UNTIL WS-CONTADOR > 3
               DISPLAY "Dado " WS-CONTADOR ": " WS-DADO(WS-CONTADOR)
           END-PERFORM.
       FIM-EXIBIR.
           EXIT.
           