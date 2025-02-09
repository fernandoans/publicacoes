       IDENTIFICATION DIVISION.
           PROGRAM-ID. ALUNO-RELATIVO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ALUNO ASSIGN TO "alunos.dat"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS WS-CHAVE-RELATIVA.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ALUNO.
       01 REGISTRO-ALUNO.
           05 WS-MATRICULA        PIC 9(3).
           05 WS-NOME             PIC X(30).
           05 WS-IDADE            PIC 9(2).

       WORKING-STORAGE SECTION.
       01 OPCAO                   PIC 9.
       01 WS-CHAVE-RELATIVA       PIC 9(3).
       01 WS-CHAVE-BUSCA          PIC 9(3).

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "---------------------".
           DISPLAY "Sistema de Alunos".
           DISPLAY "---------------------".
           DISPLAY "1 - Inserir Aluno".
           DISPLAY "2 - Buscar Aluno".
           DISPLAY "3 - Sair".
           ACCEPT OPCAO.

           EVALUATE OPCAO
               WHEN 1
                   PERFORM INSERIR-ALUNO
               WHEN 2
                   PERFORM BUSCAR-ALUNO
               WHEN 3
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opção Inválida!"
                   PERFORM INICIO
           END-EVALUATE.

       INSERIR-ALUNO.
           DISPLAY "---------------------".
           DISPLAY "Cadastrar Aluno".
           DISPLAY "---------------------".
           DISPLAY "Matrícula do Aluno:".
           ACCEPT WS-CHAVE-RELATIVA.
           MOVE WS-CHAVE-RELATIVA TO WS-MATRICULA.
           DISPLAY "Nome do Aluno:".
           ACCEPT WS-NOME.
           DISPLAY "Idade do Aluno:".
           ACCEPT WS-IDADE.

           OPEN I-O ARQUIVO-ALUNO.
           WRITE REGISTRO-ALUNO INVALID KEY
               DISPLAY "Erro ao gravar registro!".
           CLOSE ARQUIVO-ALUNO.

           DISPLAY "Aluno gravado com sucesso!".
           PERFORM INICIO.

       BUSCAR-ALUNO.
           DISPLAY "---------------------".
           DISPLAY " Buscar Aluno".
           DISPLAY "---------------------".
           DISPLAY "Qual matricula deseja obter?".
           ACCEPT WS-CHAVE-BUSCA.

           OPEN INPUT ARQUIVO-ALUNO.
           MOVE WS-CHAVE-BUSCA TO WS-CHAVE-RELATIVA.
           READ ARQUIVO-ALUNO INVALID KEY
               DISPLAY "Registro não encontrado!"
           NOT INVALID KEY
               DISPLAY "---------------------"
               DISPLAY "Aluno Encontrado:"
               DISPLAY "Matrícula: " WS-MATRICULA
               DISPLAY "Nome: " WS-NOME
               DISPLAY "Idade: " WS-IDADE
           END-READ.
           CLOSE ARQUIVO-ALUNO.

           PERFORM INICIO.
           
