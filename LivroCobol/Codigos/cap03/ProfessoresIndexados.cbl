       IDENTIFICATION DIVISION.
           PROGRAM-ID. ProfessoresIndexados.
           AUTHOR. Fernando Anselmo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PROFESSOR ASSIGN TO "professores.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRO-MATRICULA
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-PROFESSOR.
       01 REG-PROFESSOR.
           05 PRO-MATRICULA    PIC X(8).
           05 PRO-NOME         PIC X(30).

       WORKING-STORAGE SECTION.
       01 WS-OPCAO             PIC 9.
       01 WS-STATUS            PIC X(2).

       01 WS-PROFESSOR.
           05 WS-MATRICULA     PIC X(8).
           05 WS-NOME          PIC X(30).
               
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "--------------------------------".
           DISPLAY "Menu do Professor".
           DISPLAY "--------------------------------".
           DISPLAY " 1 - Cadastrar Professor".
           DISPLAY " 2 - Mostrar Professores".
           DISPLAY " 3 - Modificar Professor".
           DISPLAY " 4 - Excluir Professor".
           DISPLAY " 5 - Sair".
           DISPLAY "--------------------------------".
           DISPLAY "Escolha uma opção: ".
           ACCEPT WS-OPCAO.

           EVALUATE WS-OPCAO
               WHEN 1
                   PERFORM CADASTRAR
               WHEN 2
                   PERFORM MOSTRAR
               WHEN 3
                   PERFORM MODIFICAR
               WHEN 4
                   PERFORM EXCLUIR
               WHEN 5
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opção inválida!"
                   PERFORM INICIO
           END-EVALUATE.
           
       ABRIR-ARQUIVO.
           OPEN I-O ARQ-PROFESSOR.
           IF WS-STATUS = "35"
               OPEN OUTPUT ARQ-PROFESSOR
               CLOSE ARQ-PROFESSOR
               OPEN I-O ARQ-PROFESSOR.
       
       FECHAR-ARQUIVO.
           CLOSE ARQ-PROFESSOR.

       CADASTRAR.
           DISPLAY "--------------------------------".
           DISPLAY "Criar Professor".
           DISPLAY "--------------------------------".
           DISPLAY "Matrícula: ".
           ACCEPT WS-MATRICULA.
           DISPLAY "Nome: ".
           ACCEPT WS-NOME

           MOVE WS-MATRICULA TO PRO-MATRICULA.
           MOVE WS-NOME TO PRO-NOME.

           PERFORM ABRIR-ARQUIVO.
           WRITE REG-PROFESSOR
               INVALID KEY
                   DISPLAY "Erro: Matrícula já existe!".
           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.

       MOSTRAR.
           DISPLAY "--------------------------------".
           DISPLAY "Listar Professor".
           DISPLAY "--------------------------------".
      
           PERFORM ABRIR-ARQUIVO.
           MOVE "00" TO WS-STATUS.

           PERFORM UNTIL WS-STATUS = "10"
               READ ARQ-PROFESSOR NEXT RECORD
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       DISPLAY "Matrícula: " PRO-MATRICULA
                       DISPLAY "Nome:" PRO-NOME
                       DISPLAY "--------------------------------"
               END-READ
           END-PERFORM.

           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.
           
       MODIFICAR.
           DISPLAY "--------------------------------".
           DISPLAY "Modificar Professor".
           DISPLAY "--------------------------------".
           DISPLAY "Informe a Matrícula: ".
           ACCEPT WS-MATRICULA.

           PERFORM ABRIR-ARQUIVO
           MOVE WS-MATRICULA TO PRO-MATRICULA.
           READ ARQ-PROFESSOR KEY IS PRO-MATRICULA
               INVALID KEY
                   DISPLAY "Matrícula não encontrada!"
               NOT INVALID KEY
                   DISPLAY "Novo nome:"
                   ACCEPT WS-NOME
                   MOVE WS-NOME TO PRO-NOME
                   REWRITE REG-PROFESSOR
                       DISPLAY "Registro atualizado!".
            
           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.

       EXCLUIR.
           DISPLAY "--------------------------------".
           DISPLAY "Eliminar Professor".
           DISPLAY "--------------------------------".
           DISPLAY "Informe a Matrícula: ".
           ACCEPT WS-MATRICULA.

           PERFORM ABRIR-ARQUIVO
           MOVE WS-MATRICULA TO PRO-MATRICULA.
           READ ARQ-PROFESSOR KEY IS PRO-MATRICULA
               INVALID KEY
                   DISPLAY "Matrícula não encontrada!"
               NOT INVALID KEY
                   DELETE ARQ-PROFESSOR
                       DISPLAY "Registro atualizado!".
            
           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.   
                   