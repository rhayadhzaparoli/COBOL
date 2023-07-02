      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 30/06/23
      * Purpose: EXIBIR A SOMA DE 4 NUMEROS DIGITADOS PELO USUARIO
      * Tectonics: DESAFIO001
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDICIONAL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-N1            PIC 9(003) VALUE ZEROS.
       01  WS-N2            PIC 9(003) VALUE ZEROS.
       01  WS-N3            PIC 9(003) VALUE ZEROS.
       01  WS-N4            PIC 9(003) VALUE ZEROS.
       01  WS-RES           PIC 9(004) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "DIGITE O PRIMEIRO NUMERO: "
           ACCEPT WS-N1
           DISPLAY "DIGITE O SEGUNDO NUMERO: "
           ACCEPT WS-N2
           DISPLAY "DIGITE O TERCEIRO NUMERO: "
           ACCEPT WS-N3
           DISPLAY "DIGITE O QUARTO NUMERO: "
           ACCEPT WS-N4
           COMPUTE WS-RES = WS-N1 + WS-N2 + WS-N3 + WS-N4
           DISPLAY "A SOMA DOS QUATRO NUMEROS E: " WS-RES
           IF WS-RES > 1000 THEN
               DISPLAY WS-RES " E MAIOR QUE 1000 "
           ELSE
               DISPLAY WS-RES " E MENOR QUE 1000 "
           END-IF.

            STOP RUN.
       END PROGRAM CONDICIONAL.
