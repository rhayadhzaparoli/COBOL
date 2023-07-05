      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 05/07/2023
      * Purpose: LER ARQUIVO SEQUENCIAL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEITURA-ARQ.

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
       77  WS-EOF                  PIC 99.
       01  WS-DATA.
           05 WS-COD-CLIENTE       PIC 9(003).
           05 WS-NM-CLIENTE        PIC X(040).
           05 WS-TEL-CLIENTE       PIC X(009).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "******** LISTAGEM DE CLIENTE ********"
            SET WS-STATUS          TO 0.
            SET WS-EOF          TO 0.

            OPEN INPUT CLIENTE

            PERFORM UNTIL WS-EOF EQUAL 1

            READ CLIENTE INTO WS-DATA
               AT END
                   MOVE 1 TO WS-EOF
               NOT AT END
                   DISPLAY 'CODIGO:...' WS-COD-CLIENTE
                   DISPLAY 'NOME:.....' WS-NM-CLIENTE
                   DISPLAY 'TELEFONE:.' WS-TEL-CLIENTE
                   DISPLAY " "

            END-READ
            END-PERFORM
                   DISPLAY "POR NAO SER UM ARQUIVO INDEXADO, "
                           "ELE REPETE O CODIGO"
            CLOSE CLIENTE

            STOP RUN.
       END PROGRAM LEITURA-ARQ.
