      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 01/07/2023
      * Purpose: CALCULA MEDIA DE APROVAÇÃO, MEDIA DE APROVAÇÃO É 7
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCMEDIA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-A1           PIC 9(002)V99   VALUE ZEROS.
       01  WS-A2           PIC 9(002)V99   VALUE ZEROS.
       01  WS-T1           PIC 9(002)V99   VALUE ZEROS.
       01  WS-T2           PIC 9(002)V99   VALUE ZEROS.
       01  WS-MEDIA        PIC 9(002)V99   VALUE ZEROS.
       01  WS-ALUNO        PIC X(040)      VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            MOVE " RHAYADH" TO WS-ALUNO
            DISPLAY "PROGRAMA PARA CALCULAR A MEDIA DO WS-ALUNO"
            DISPLAY "DIGITE A NOTA DA A1: "
            ACCEPT WS-A1
            DISPLAY "DIGITE A NOTA DA A2: "
            ACCEPT WS-A2
            DISPLAY "DIGITE A NOTA DO T1: "
            ACCEPT WS-T1
            DISPLAY "DIGITE A NOTA DO T2: "
            ACCEPT WS-T2
            COMPUTE WS-MEDIA = (WS-A1 + WS-A2 + WS-T1 + WS-T2)/4
            DISPLAY " "
            IF WS-MEDIA >= 6 THEN
                DISPLAY " A MEDIA DO ALUNO" WS-ALUNO "E " WS-MEDIA
                "E O ALUNO FOI APROVADO"
            ELSE
                DISPLAY  " A MEDIA DO ALUNO" WS-ALUNO "E " WS-MEDIA
                "E O ALUNO FOI REPROVADO"

            END-IF
            STOP RUN.
       END PROGRAM CALCMEDIA.
