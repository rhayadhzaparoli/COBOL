      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 02/07/2023
      * Purpose: CALCULADORA PARA USAR PARAGRAFOS, PERFORM E EVALUATE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-CALC.
        05  WS-N1                    PIC  9(02) VALUE 0.
        05  WS-N2                    PIC  9(02) VALUE 0.
        05  WS-RES                   PIC S9(02) VALUE 0.
       77  WS-CHOICE                 PIC  9(02).
       77  WS-KEY                    PIC  A.

       PROCEDURE DIVISION.
       P100-INIT.
           DISPLAY "DIGITE UM NUMERO PARA ESCOLHER A OPERACAO"
           DISPLAY " "
           DISPLAY "*****************************************"
           DISPLAY " "
           DISPLAY "01 - SOMA"
           DISPLAY "02 - SUBTRACAO"
           DISPLAY "03 - MULTIPLICACAO"
           DISPLAY "04 - DIVISAO"
           DISPLAY "05 - EXPONENCIACAO"
           DISPLAY "00 - SAIR DO PROGRAMA"
           DISPLAY "99 - PARA AJUDA"
           DISPLAY " "
           ACCEPT WS-CHOICE
           DISPLAY " "




           PERFORM P200-CHK              THRU P200-CHK-END.
           PERFORM P300-LOOP             THRU P300-LOOP-END.
           PERFORM P900-TERMINAL         THRU P900-TERMINAL-END.

       P100-INIT-END.

       P200-CHK.
              EVALUATE WS-CHOICE
               WHEN 01
               DISPLAY "DIGITE O PRIMEIRO NUMERO INTEIRO"
               ACCEPT WS-N1
               DISPLAY "DIGITE O SEGUNDO NUMERO INTEIRO"
               ACCEPT WS-N2

               COMPUTE WS-RES = WS-N1 + WS-N2

               DISPLAY  WS-N1 " + " WS-N2 " = " WS-RES

               WHEN 02
               DISPLAY "DIGITE O PRIMEIRO NUMERO INTEIRO"
               ACCEPT WS-N1
               DISPLAY "DIGITE O SEGUNDO NUMERO INTEIRO"
               ACCEPT WS-N2

               COMPUTE WS-RES = WS-N1 - WS-N2

               DISPLAY  WS-N1 " - " WS-N2 " = " WS-RES

               WHEN 03
               DISPLAY "DIGITE O PRIMEIRO NUMERO INTEIRO"
               ACCEPT WS-N1
               DISPLAY "DIGITE O SEGUNDO NUMERO INTEIRO"
               ACCEPT WS-N2

               COMPUTE WS-RES = WS-N1 * WS-N2

               DISPLAY  WS-N1 " * " WS-N2 " = " WS-RES

               WHEN 04
               DISPLAY "DIGITE O PRIMEIRO NUMERO INTEIRO"
               ACCEPT WS-N1
               DISPLAY "DIGITE O SEGUNDO NUMERO INTEIRO"
               ACCEPT WS-N2

               COMPUTE WS-RES = WS-N1 / WS-N2

               DISPLAY  WS-N1 " / " WS-N2 " = " WS-RES

               WHEN 05
               DISPLAY "DIGITE O PRIMEIRO NUMERO INTEIRO"
               ACCEPT WS-N1
               DISPLAY "DIGITE O SEGUNDO NUMERO INTEIRO"
               ACCEPT WS-N2

               COMPUTE WS-RES = WS-N1 ** WS-N2

               DISPLAY  WS-N1 " ** " WS-N2 " = " WS-RES

               WHEN 99
               DISPLAY "ESTA E UMA CALCULADORA BASICA QUE FAZ 5 TIPOS"
               DISPLAY "DE OPERACOES MATEMATICAS: SOMA, SUBTRACAO, "
               "MULTIPLICACAO,"
               DISPLAY  "DIVISAO E EXPONENCIACAO. PARA UTILIZA-LA "
               "BASTA INSERIR 2"
               DISPLAY "NUMEROS INTEIROS, CADA UM DEVE CONTER 2 "
               "POSICOES SOMENTE."
               DISPLAY " "

               WHEN 00
               EXIT



            END-EVALUATE.
       P200-CHK-END.

       P300-LOOP.
            DISPLAY "DESEJA SAIR ?"
            DISPLAY "DIGITE S OU N ?"
            DISPLAY " "
            ACCEPT WS-KEY
            IF WS-KEY = "N" THEN
               PERFORM P100-INIT           THRU P100-INIT-END

            END-IF.
       P300-LOOP-END.

       P900-TERMINAL.

            STOP RUN.

       P900-TERMINAL-END.

       END PROGRAM CALCULADORA.
