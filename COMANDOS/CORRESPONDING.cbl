      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 10/07/23
      * Purpose: ENTENDENDO A CLAUSULA CORRESPONDING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORRESP.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-REG-1.
         05 WS-NOME                        PIC X(50) VALUE SPACES.
         05 WS-TEL                         PIC 9(09) VALUE 0.
         05 WS-UF                          PIC A(02) VALUE SPACES.
         05 WS-SALARIO                     PIC 9(04)V99 VALUE 0.

       01 WS-REG-2.
         05 WS-NOME                        PIC X(50) VALUE SPACES.
         05 WS-SALARIO                     PIC 9(04)V99 VALUE 0.
         05 WS-TEL                         PIC 9(09) VALUE 0.
         05 WS-UF                          PIC A(02) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY ' SEM MOVE '
            MOVE 'RHAYADH'                 TO WS-NOME    OF WS-REG-1
            MOVE 991056042                 TO WS-TEL     OF WS-REG-1
            MOVE 'SP'                      TO WS-UF      OF WS-REG-1
            MOVE 2150.00                   TO WS-SALARIO OF WS-REG-1


            DISPLAY '*** WS-REG-1 ***'
            DISPLAY WS-REG-1
            DISPLAY ' '
            DISPLAY WS-NOME                OF WS-REG-1
            DISPLAY WS-TEL                 OF WS-REG-1
            DISPLAY WS-UF                  OF WS-REG-1
            DISPLAY WS-SALARIO             OF WS-REG-1
            DISPLAY ' '
            INITIALISE WS-REG-2
            DISPLAY '*** WS-REG-2 ***'
            DISPLAY WS-REG-2
            DISPLAY ' '
            DISPLAY WS-NOME                OF WS-REG-2
            DISPLAY WS-TEL                 OF WS-REG-2
            DISPLAY WS-UF                  OF WS-REG-2
            DISPLAY WS-SALARIO             OF WS-REG-2
            DISPLAY ' '

      ********************* FAZENDO MOVIMENTACAO DIRETA ****************
      ******* DEVIDO AO FATO DAS VARIAVEIS NAO ESTAREM NA MESMA ORDEM **
      ************** A MOVIMENTACAO FICA INCOERENTE/BAGUNCADA **********

            DISPLAY ' COM MOVE SIMPLES '

            MOVE WS-REG-1                   TO WS-REG-2
            DISPLAY '*** WS-REG-1 ***'
            DISPLAY WS-REG-1
            DISPLAY ' '
            DISPLAY WS-NOME                OF WS-REG-1
            DISPLAY WS-TEL                 OF WS-REG-1
            DISPLAY WS-UF                  OF WS-REG-1
            DISPLAY WS-SALARIO             OF WS-REG-1
            DISPLAY ' '

            DISPLAY '*** WS-REG-2 ***'
            DISPLAY WS-REG-2
            DISPLAY ' '
            DISPLAY WS-NOME                OF WS-REG-2
            DISPLAY WS-TEL                 OF WS-REG-2
            DISPLAY WS-UF                  OF WS-REG-2
            DISPLAY WS-SALARIO             OF WS-REG-2
            DISPLAY ' '

      ********************* FAZENDO MOVIMENTACAO CORR ******************
      ****** INDEPENDENTE DA ORDEM DAS VARIAVEIS, COM CORR... O CAMPO **
      *************** SERA CORRESPONDIDO AO CAMPO RELACIONADO **********

            DISPLAY ' COM MOVE CORR '
            MOVE CORR WS-REG-1             TO WS-REG-2
            DISPLAY '*** WS-REG-1 ***'
            DISPLAY WS-REG-1
            DISPLAY ' '
            DISPLAY WS-NOME                OF WS-REG-1
            DISPLAY WS-TEL                 OF WS-REG-1
            DISPLAY WS-UF                  OF WS-REG-1
            DISPLAY WS-SALARIO             OF WS-REG-1
            DISPLAY ' '

            DISPLAY '*** WS-REG-2 ***'
            DISPLAY WS-REG-2
            DISPLAY ' '
            DISPLAY WS-NOME                OF WS-REG-2
            DISPLAY WS-TEL                 OF WS-REG-2
            DISPLAY WS-UF                  OF WS-REG-2
            DISPLAY WS-SALARIO             OF WS-REG-2
            DISPLAY ' '

            STOP RUN.
       END PROGRAM CORRESP.
