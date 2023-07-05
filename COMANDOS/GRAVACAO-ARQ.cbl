      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 05/07/2023
      * Purpose: GRAVAR ARQUIVO SEQUENCIAL E GERAR ARQUIVO .TXT
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAVACAO-ARQ.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT CLIENTE ASSIGN TO 'Área de Trabalho'


           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS IS WS-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD  CLIENTE.
       01  REG-CLIENTE.
           05 COD-CLIENTE          PIC 9(003).
           05 NM-CLIENTE           PIC X(040).
           05 TEL-CLIENTE          PIC X(009).

       WORKING-STORAGE SECTION.
       77  WS-STATUS               PIC 99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "******** CADASTRO DE CLIENTE ********"
            SET WS-STATUS          TO 0.

            OPEN EXTEND CLIENTE

            IF WS-STATUS EQUAL 35 THEN
                OPEN OUTPUT CLIENTE
            END-IF

            IF WS-STATUS EQUAL ZEROS
               DISPLAY "INFORME O COD. DO CLIENTE"
               ACCEPT COD-CLIENTE
               DISPLAY "INFORME O NOME DO CLIENTE"
               ACCEPT NM-CLIENTE
               DISPLAY "INFORME O TELEFONE DO CLIENTE"
               ACCEPT TEL-CLIENTE
               WRITE REG-CLIENTE
               IF WS-STATUS NOT EQUAL ZEROS
                   DISPLAY "NAO FOI POSSIVEL GRAVAR O REGISTRO!"
                   DISPLAY "FILE STATUS: " WS-STATUS
               ELSE
                   DISPLAY "REGISTRO GRAVADO "
                   DISPLAY " "
                   DISPLAY "STATUS: " WS-STATUS
               END-IF
            ELSE
                DISPLAY "ERRO AO CRIAR O ARQUIVO."
                DISPLAY "FILE STATUS: " WS-STATUS
            END-IF

            CLOSE CLIENTE

            STOP RUN.
       END PROGRAM GRAVACAO-ARQ.
