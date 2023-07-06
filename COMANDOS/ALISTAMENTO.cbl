      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 01/07/2023
      * Purpose: FAZ O ALISTAMENTO MILITAR
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALISTAMENTO-MILITAR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-DOB.
        05 WS-DOB-YY                   PIC 9(004) VALUE 0.
        05 WS-DOB-MM                   PIC 99 VALUE 0.
        05 WS-DOB-DD                   PIC 99 VALUE 0.

       01 WS-CURR-DATE.
        05 WS-CURR-YY                   PIC 9(004) VALUE 0.
        05 WS-CURR-MM                   PIC 99 VALUE 0.
        05 WS-CURR-DD                   PIC 99 VALUE 0.

       01 WS-PRS-DATA.
         05 WS-NAME                    PIC X(050) VALUE SPACES.
         05 WS-RG                      PIC 9(010) VALUE 0.
         05 WS-CPF                     PIC 9(011) VALUE 0.
         05 WS-ADRESS                  PIC X(080) VALUE SPACES.

       77 WS-KEY                       PIC A(001) VALUE SPACES.
       77 WS-CALC-AGE                  PIC 99.
       77 WS-MAT                       PIC 9(010) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       P100-VERIFYER.

            DISPLAY ' ************************************************ '
            DISPLAY ' *                                              * '
            DISPLAY ' ******* ALISTAMENTO MILITAR OBRIGATORIO ******** '
            DISPLAY ' *                                              * '
            DISPLAY ' ************************************************ '
            DISPLAY ' '
            DISPLAY ' '

            DISPLAY ' INSIRA A DATA DE NASCIMENTO DO CANDIDATO '
            DISPLAY '----- O FORMATO DEVE SER (AAAAMMDD) ------'
            ACCEPT WS-DOB

            MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE
            COMPUTE WS-CALC-AGE = WS-DOB-YY - WS-CURR-YY
            DISPLAY ' IDADE DO CANDIDATO (A): 'WS-CALC-AGE


            IF WS-CALC-AGE >= 18 THEN
                 PERFORM P200-REG
            ELSE
                 DISPLAY " IDADE NAO PERMITIDA "
               PERFORM P900-TERMINAL
            END-IF.




       P200-REG.

            DISPLAY ' ************************************************ '
            DISPLAY ' *                                              * '
            DISPLAY ' ********** CADASTRO DO CANDIDATO (A) *********** '
            DISPLAY ' *                                              * '
            DISPLAY ' ************************************************ '
            DISPLAY ' '
            DISPLAY ' '
            DISPLAY " INFORME O NOME DO CANDIDATO(A) "
            ACCEPT WS-NAME
            DISPLAY " INFORME O RG DO CANDIDATO(A) "
            ACCEPT WS-RG
            DISPLAY " INFORME O CPF DO CANDIDATO(A) "
            ACCEPT WS-CPF
            DISPLAY " INFORME O ENDERECO DO CANDIDATO(A) "
            ACCEPT WS-ADRESS
            DISPLAY " ************************************************ "
            DISPLAY ' ************************************************ '
            DISPLAY ' *                                              * '
            DISPLAY ' ********* CONFIRMA OS DADOS ABAIXO ************* '
            DISPLAY ' *                                              * '
            DISPLAY ' ************************************************ '
            DISPLAY ' '
            DISPLAY " NOME.........: "  WS-NAME
            DISPLAY " RG...........: "  WS-RG
            DISPLAY " CPF..........: "  WS-CPF
            DISPLAY " ENDERECO.....: "  WS-ADRESS
            DISPLAY ' '
            DISPLAY ' DESEJA ALTERAR ALGUM DADO ? '
            ACCEPT WS-KEY

            IF WS-KEY = 'S' THEN
                DISPLAY " *****  DIGITE  ****** "
                DISPLAY "  01 PARA ALTERAR NOME "
                DISPLAY "   02 PARA ALTERAR RG  "
                DISPLAY "  03 PARA ALTERAR CPF  "
                DISPLAY " 04 PARA ALTERAR ENDERECO "
                EVALUATE WS-PRS-DATA
                WHEN '01'
                DISPLAY " INFORME O NOME DO CANDIDATO(A) "
                ACCEPT WS-NAME

                WHEN '02'
                DISPLAY " INFORME O RG DO CANDIDATO(A) "
                ACCEPT WS-RG

                WHEN '03'
                DISPLAY " INFORME O CPF DO CANDIDATO(A) "
                ACCEPT WS-CPF

                WHEN '04'
                DISPLAY " INFORME O ENDERECO DO CANDIDATO(A) "
                ACCEPT WS-ADRESS
                END-EVALUATE
            ELSE
                PERFORM P400-MAT

           END-IF.

      *P300-VERIFY.



            DISPLAY " GERANDO MATRICULA ........ "
            PERFORM P400-MAT.

       P400-MAT.
            DISPLAY ' ************************************************ '
            DISPLAY ' ************************************************ '
            DISPLAY ' *                                              * '
            DISPLAY ' ********* MATRICULA MILITAR GERADA ************* '
            DISPLAY ' *                                              * '
            DISPLAY ' ************************************************ '

            DISPLAY '           'FUNCTION RANDOM(WS-MAT)
            DISPLAY ' '





            DISPLAY "DESEJA SAIR ?"
            DISPLAY "DIGITE S OU N ?"
            DISPLAY " "
            ACCEPT WS-KEY
            IF WS-KEY = "N" THEN
               PERFORM P100-VERIFYER

            END-IF.

       P900-TERMINAL.
           STOP RUN.


       END PROGRAM ALISTAMENTO-MILITAR.
