000001 IDENTIFICATION DIVISION.
000002     PROGRAM-ID. COMOVAI.
000003     AUTHOR. Fernando Anselmo.
000004
000005 ENVIRONMENT DIVISION.
000006
000007 DATA DIVISION.
000008 WORKING-STORAGE SECTION. 
000009 
000010 01 NOME   PIC A(020).
000011
000012 01 DATA-ATUAL.
000013    05 ANO-ATUAL PIC 9(004).
000014    05 MES-ATUAL PIC 9(002).
000015    05 DIA-ATUAL PIC 9(002).
000016
000017 PROCEDURE DIVISION.
000018 PRINCIPAL.
000019     DISPLAY "Entre com seu Nome: ".
000020     ACCEPT NOME.
000021     ACCEPT DATA-ATUAL FROM DATE YYYYMMDD.
000022
000023     DISPLAY "Bem vindo " NOME.
000024     DISPLAY "Sabia que hoje é " DIA-ATUAL "/" MES-ATUAL "/" 
            ANO-ATUAL
000025
000026 STOP RUN.
000027 END PROGRAM COMOVAI.