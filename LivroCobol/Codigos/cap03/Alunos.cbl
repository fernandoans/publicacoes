       IDENTIFICATION DIVISION.
           PROGRAM-ID. Alunos.
           AUTHOR. Fernando Anselmo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ALUNOS ASSIGN TO "alunos.data"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ALUNOS.
       01 REGISTRO-ALUNO.
           05 NOME-ALUNO    PIC X(20).
           05 NOTA-1        PIC 9(3)V9(2).
           05 NOTA-2        PIC 9(3)V9(2).
           05 MEDIA         PIC 9(3)V9(2).
           05 RESULTADO     PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-NOME-ALUNO     PIC X(20).
       01 WS-NOTA-1         PIC 9(3)V9(2).
       01 WS-NOTA-2         PIC 9(3)V9(2).
       01 WS-MEDIA          PIC 9(3)V9(2).
       01 WS-RESULTADO      PIC X(20).
       01 OPCAO             PIC X VALUE SPACE.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "============================".
           DISPLAY "Cadastro de Alunos e Notas".
           DISPLAY "============================".
       
           OPEN OUTPUT ARQUIVO-ALUNOS.
           PERFORM ATE-FINALIZAR.
           CLOSE ARQUIVO-ALUNOS.

           DISPLAY "==================================================".
           DISPLAY "Processo Finalizado. Arquivo 'alunos.data' criado.".
           DISPLAY "==================================================".

           STOP RUN.

       ATE-FINALIZAR.
           PERFORM UNTIL OPCAO = "N" OR OPCAO = "n"
               PERFORM SOLICITAR-DADOS
               PERFORM CALCULAR-MEDIA
               PERFORM GRAVAR-ARQUIVO

               DISPLAY "Deseja cadastrar outro aluno? (S/N): " 
                 WITH NO ADVANCING
               ACCEPT OPCAO
           END-PERFORM.  

       SOLICITAR-DADOS.
           DISPLAY "Informe o nome do aluno: " WITH NO ADVANCING.
           ACCEPT WS-NOME-ALUNO.

           DISPLAY "Informe a primeira nota: " WITH NO ADVANCING.
           ACCEPT WS-NOTA-1.

           DISPLAY "Informe a segunda nota: " WITH NO ADVANCING.
           ACCEPT WS-NOTA-2.

       CALCULAR-MEDIA.
           COMPUTE WS-MEDIA = (WS-NOTA-1 + WS-NOTA-2) / 2.
           IF WS-MEDIA >= 5.00
               MOVE "Aprovado" TO WS-RESULTADO
           ELSE
               MOVE "Reprovado" TO WS-RESULTADO
           END-IF.

       GRAVAR-ARQUIVO.
           MOVE WS-NOME-ALUNO TO NOME-ALUNO.
           MOVE WS-NOTA-1 TO NOTA-1.
           MOVE WS-NOTA-2 TO NOTA-2.
           MOVE WS-MEDIA TO MEDIA.
           MOVE WS-RESULTADO TO RESULTADO.
           WRITE REGISTRO-ALUNO.
