       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALIAUX.
      *AUTHOR. RHAYADH ZAPAROLI.
      *DATE-WRITTEN. 01/07/2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REG-FILE ASSIGN TO 'REGISTRO'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD REG-FILE.
       01 PRS-DATA.
         05 NAMES       PIC X(50) VALUE SPACES.
         05 RG          PIC 9(10) VALUE 0.
         05 CPF         PIC 9(11) VALUE 0.
         05 ADRESS      PIC X(80) VALUE SPACES.
         05 MEDIA       PIC 99V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS    PIC 99.
       01 WS-EOF       PIC 99.
       01 WS-PRS-DATA.
         05 WS-NAME    PIC X(50) VALUE SPACES.
         05 WS-RG      PIC 9(10) VALUE 0.
         05 WS-CPF     PIC 9(11) VALUE 0.
         05 WS-ADDRESS PIC X(80) VALUE SPACES.
         05 WS-MEDIA   PIC 99V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "******** DADOS ********"
            SET WS-STATUS TO 0.
            SET WS-EOF TO 0.

            OPEN INPUT REG-FILE

            PERFORM UNTIL WS-EOF = 1
            READ REG-FILE INTO WS-PRS-DATA
                AT END
                MOVE 1 TO WS-EOF
             NOT AT END
            DISPLAY 'NOME:......' WS-NAME
            DISPLAY 'RG:........' WS-RG
            DISPLAY 'CPF:.......' WS-CPF
            DISPLAY 'ENDERECO:..' WS-ADDRESS
           END-READ
            END-PERFORM

           CLOSE REG-FILE

           DISPLAY "POR NAO SER UM ARQUIVO INDEXADO, "
           "ELE REPETE O CODIGO"

           STOP RUN.
       END PROGRAM ALIAUX.
