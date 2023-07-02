      ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 01/07/2023
      * Purpose: MOSTRA MES REFERENTE A DATA DIGITADA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM_EVALUATE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-DATE             PIC X(010)  VALUE SPACES.
       01  WS-DATE-R           REDEFINES WS-DATE.
         05 WS-DATE-DD         PIC 9(002).
         05 FILLER             PIC X.
         05 WS-DATE-MM         PIC 9(002).
         05 FILLER             PIC X.
         05 WS-DATE-YY         PIC 9(004).
       77  WS-MONTH-NAME       PIC X(015)  VALUE SPACES.
       77  WS-KEY              PIC X.

       PROCEDURE DIVISION.
      ******************************************************************
      *        INICIALIZA AS VARIAVEIS E CHAMA OS DEMAIS PARAGRAFOS
      ******************************************************************
       P100-INIT.
            DISPLAY "INICIO DO PROCESSAMENTO".

            MOVE SPACES        TO WS-DATE
                                  WS-MONTH-NAME
                                  WS-KEY.
            DISPLAY "INFORME UMA DATA: ".
            ACCEPT WS-DATE.

            PERFORM P200-MONTH-CHECK   THRU    P200-MONTH-CHECK-END.
            PERFORM P300-CHECK         THRU    P300-CHECK-END.
            PERFORM P900-TERMINAL      THRU    P900-TERMINAL-END.



       P100-INIT-END.

      ******************************************************************
      *                    VERIFICA O MES DIGITADO
      ******************************************************************

       P200-MONTH-CHECK.
            DISPLAY "VERIFICANDO O MES DA TATA INFORMADA..."

            EVALUATE WS-DATE-MM
            WHEN 01
                 MOVE "JANEIRO"        TO WS-MONTH-NAME
            WHEN 02
                 MOVE "FEVEREIRO"      TO WS-MONTH-NAME
            WHEN 03
                 MOVE "MARCO"          TO WS-MONTH-NAME
            WHEN 04
                 MOVE "ABRIL"          TO WS-MONTH-NAME
            WHEN 05
                 MOVE "MAIO"           TO WS-MONTH-NAME
            WHEN 06
                 MOVE "JUNHO"          TO WS-MONTH-NAME
            WHEN 07
                 MOVE "JULHO"          TO WS-MONTH-NAME
            WHEN 08
                 MOVE "AGOSTO"         TO WS-MONTH-NAME
            WHEN 09
                 MOVE "SETEMBRO"       TO WS-MONTH-NAME
            WHEN 10
                 MOVE "OUTUBRO"        TO WS-MONTH-NAME
            WHEN 11
                 MOVE "NOVEMBRO"       TO WS-MONTH-NAME
            WHEN 12
                 MOVE "DEZEMBRO"       TO WS-MONTH-NAME

            END-EVALUATE.

       P200-MONTH-CHECK-END.

      ******************************************************************
      *                       MOSTRA O RESULTADO
      ******************************************************************

       P300-CHECK.
            DISPLAY "EXIBINDO O RESULTADO DO PROCESSAMENTO".
            DISPLAY "O MES DA DATA E: " WS-MONTH-NAME

            DISPLAY "DESEJA CONTINUAR (S/N) ?"
            ACCEPT WS-KEY
            IF WS-KEY = "S" THEN
               PERFORM P100-INIT           THRU P100-INIT-END
            END-IF.


       P300-CHECK-END.

      ******************************************************************
      *                       FINALIZA O PROGRAMA
      ******************************************************************

       P900-TERMINAL.
            STOP RUN.
       P900-TERMINAL-END.
       END PROGRAM PERFORM_EVALUATE.
