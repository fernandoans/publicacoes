       IDENTIFICATION DIVISION.
       PROGRAM-ID. ImportarAlunos.
       AUTHOR. Fernando Anselmo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-TEXTO ASSIGN TO "alunos.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQ-INDEXADO ASSIGN TO "alunos.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ALU-MATRICULA
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-TEXTO.
       01 REGISTRO-TEXTO PIC X(100).

       FD ARQ-INDEXADO.
       01 REGISTRO-INDEXADO.
           05 ALU-MATRICULA    PIC X(8).
           05 ALU-NOME         PIC X(30).
           05 ALU-CURSO        PIC X(20).
           05 ALU-DATA-INICIO  PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-STATUS   PIC X(2) VALUE "00".
       01 WS-FIM-ARQ  PIC X(1) VALUE "N".

       01 WS-DELIMITADOR  PIC X VALUE ";".
       01 WS-POS          PIC 9(2).

       01 WS-MATRICULA    PIC X(8).
       01 WS-NOME         PIC X(30).
       01 WS-CURSO        PIC X(20).
       01 WS-DATA-INICIO  PIC X(10).

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARQ-TEXTO.
           OPEN OUTPUT ARQ-INDEXADO.

           PERFORM UNTIL WS-FIM-ARQ = "S"
               READ ARQ-TEXTO INTO REGISTRO-TEXTO
                   AT END MOVE "S" TO WS-FIM-ARQ
                   NOT AT END
                       PERFORM PROCESSAR-REGISTRO
               END-READ
           END-PERFORM.

           CLOSE ARQ-TEXTO.
           CLOSE ARQ-INDEXADO.

           DISPLAY "Importação concluída com sucesso!".
           STOP RUN.

       PROCESSAR-REGISTRO.
           MOVE FUNCTION TRIM(REGISTRO-TEXTO) TO REGISTRO-TEXTO.

           UNSTRING REGISTRO-TEXTO DELIMITED BY WS-DELIMITADOR
               INTO WS-MATRICULA, WS-NOME, WS-CURSO, WS-DATA-INICIO.

           MOVE WS-MATRICULA TO ALU-MATRICULA.
           MOVE WS-NOME TO ALU-NOME.
           MOVE WS-CURSO TO ALU-CURSO.
           MOVE WS-DATA-INICIO TO ALU-DATA-INICIO.

           WRITE REGISTRO-INDEXADO
            INVALID KEY DISPLAY "Erro ao gravar o aluno: " WS-MATRICULA.
