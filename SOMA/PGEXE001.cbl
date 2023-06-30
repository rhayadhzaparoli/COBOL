      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 06/30/2023
      * Purpose: DISPLAY SUM OF TWO RANDOM NUMBERS
      * Tectonics: PGEXE001
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGEXE001.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-N1        PIC 9(003)  VALUE ZEROS.
       01 WS-N2        PIC 9(003)  VALUE ZEROS.
       01 WS-RES       PIC 9(004)  VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "MOSTRA O RESULTADO DA SOMA DE 2 NUMEROS ALEATORIOS"
            DISPLAY "INFORME O PRIMEIRO NUMERO: "
            ACCEPT WS-N1
            DISPLAY "INFORME O SEGUNDO NUMERO: "
            ACCEPT WS-N2
            COMPUTE WS-RES = WS-N1 + WS-N2
            DISPLAY "O RESULTADO DA SOMA E: " WS-RES
            STOP RUN.
       END PROGRAM PGEXE001.
