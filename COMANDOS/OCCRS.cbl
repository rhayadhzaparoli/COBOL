      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCRS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  DOW.
          05 FILLER            PIC X(9) VALUE '01DOMINGO'.
          05 FILLER            PIC X(9) VALUE '02SEGUNDA'.
          05 FILLER            PIC X(9) VALUE '03TERCA'.
          05 FILLER            PIC X(9) VALUE '04QUARTA'.
          05 FILLER            PIC X(9) VALUE '05QUINTA'.
          05 FILLER            PIC X(9) VALUE '06SEXTA'.
          05 FILLER            PIC X(9) VALUE '07SABADO'.

      ******************************************************************
      ******* A REFINICAO OBRIGATORIAMENTE FICA APOS O GRUPO QUE *******
      ******* DESEJA REFENINIR.                                  *******
      ******************************************************************

       01  FILLER REDEFINES DOW  OCCURS 7 TIMES.
          05 DAY-NMB           PIC 99.
          05 DAY-NM            PIC X(7).

      ******************************************************************
      ****** A VARIAVEL ABAIXO AFETA A APRESENTACAO DOS DOIS PRIMEIROS *
      ****** PARAGRAFOS. CASO QUEIRA APRESENTA-LOS, COMENTE-A          *
      ******************************************************************

          05 EVT OCCURS 4 TIMES.
             10 NUM-EVT        PIC 99.
             10 DSC-EVT        PIC X(99).

      ******************************************************************
      ******************************************************************


       01  REG.
          05 REG-FIN.
             10 NUM-CONT       PIC 9(5).
             10 NUM-PREST      PIC 9(4)V99.

      ******************************************************************
      ******  FORMATACAO DE MASCARA PARA APRESENTACAO EM DOLAR  ********
      ******  AQUI NO CASO ESTA SENDO REDEFINIDA A VARIAVEL POR ********
      ******  CONTA DA CLAUSULA OCCURS COMO MOSTRADO ABAIXO     ********
      ******************************************************************

             10 PREST OCCURS 1 TO 420 TIMES
                      DEPENDING ON NUM-PREST PIC $,$$$,$$$.$$.

      ******************************************************************
      ******************************************************************

       77  IND                  PIC 999.
       77  IND-EVT              PIC 999.
       77  PREST-AUX            PIC $,$$$,$$$.$$.
       77  EXT                  PIC X.

       PROCEDURE DIVISION.
       001MAIN.

       P100-SPL-ARRAY.
      ******************************************************************
      *      FORMA UTILIZADA PARA EXIBICAO PONTUAL, PERCORRE O INDICE  *
      *      MAS PONTUA O ELEMENTO QUE ESTA DENTRO DO ( ).             *
      *      RESUMIDAMENTE, FAZ UM APONTAMENTO PARA O VALOR DO INDICE  *
      ******************************************************************
           DISPLAY ' --------- PROGRAMA DE ARRAY ----------- '
           DISPLAY ' ------ FORMA DE EXIBICAO PONTUAL ------ '

           DISPLAY ' NUMERO:        NOME: '
           DISPLAY '  ' DAY-NMB(1) '           ' DAY-NM(1)
           DISPLAY '  ' DAY-NMB(4) '           ' DAY-NM(4)
           DISPLAY '  ' DAY-NMB(7) '           ' DAY-NM(7).





       P200-BST-ARRAY.
      ******************************************************************
      *      FORMA UTILIZADA PARA EXIBICAO DE TODOS OS INDICES.        *
      *                                                                *
      *      PERCORRE O VETOR, AQUI CITADO COMO IND                    *
      ******************************************************************

           DISPLAY ' --------- PROGRAMA DE ARRAY ----------- '
           DISPLAY ' ------- FORMA DE EXIBICAO GERAL ------- '
           DISPLAY ' ----- UTILIZANDO PERFORM VARYING ------ '


           MOVE ZEROS              TO IND

           DISPLAY ' NUMERO:        NOME: '

           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND GREATER 7
               DISPLAY '    ' DAY-NMB(IND) '         ' DAY-NM(IND)
           END-PERFORM.





       P300-LOOP-ARRAY.
      ******************************************************************
      *     FORMA UTILIZANDO LOOPING QUE PERCORRE O VETOR, AQUI        *
      *     CITADO COMO IND E SENDO REFERENCIADO EM PREST | PREST(IND) *
      ******************************************************************
           DISPLAY ' --------- PROGRAMA DE ARRAY ----------- '
           DISPLAY ' --------- FORMA DE EXIBICAO  ---------- '
           DISPLAY ' --------- UTILIZANDO LOOPING ---------- '
           DISPLAY ' '
           DISPLAY ' INFORME O NUMERO DE PRESTACOES '
           ACCEPT NUM-PREST
           DISPLAY ' INFORME O VALOR DAS PRESTACOES '
           ACCEPT PREST-AUX

      ******************************************************************
      *                   ALIMENTADOR DO ARRAY                         *
      ******************************************************************
               IF NUM-PREST GREATER THAN 420 THEN
                   DISPLAY NUM-PREST ' NUMERO INVALIDO! '
               ELSE
                   MOVE ZEROS TO IND
                   PERFORM VARYING IND FROM  1 BY 1 UNTIL IND GREATER
                                                            NUM-PREST
                   MOVE PREST-AUX TO PREST(IND)
                   END-PERFORM
               END-IF

      ******************************************************************
      *                        LEITOR DO ARRAY                         *
      ******************************************************************
               MOVE ZEROS TO IND
               PERFORM VARYING IND FROM  1 BY 1 UNTIL IND GREATER
                                                           NUM-PREST
               MOVE PREST-AUX TO PREST(IND)
               DISPLAY ' PRESTACAO: ' IND ' VALOR ' PREST(IND)
               END-PERFORM.

       P400-BI-ARRAY.

      ******************************************************************
      *    FORMA UTILIZANDO LOOPING QUE PERCORRE O VETOR BIDIMENSIONAL *
      *     , AQUI CITADO COMO IND E IND-EVT E SENDO REFERENCIADO EM   *
      *      NUM-EVT E DCS-EVT                                         *                                                     *
      ******************************************************************
           DISPLAY ' -- PROGRAMA DE ARRAY  BIDIMENSIONAL --- '
           DISPLAY ' ------- FORMA DE EXIBICAO GERAL ------- '
           DISPLAY ' ----- UTILIZANDO PERFORM VARYING ------ '
           MOVE ZEROS              TO IND
               DISPLAY ' NUMERO:        NOME: '

           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND GREATER 7
               DISPLAY '    ' DAY-NMB(IND) '         ' DAY-NM(IND)
           END-PERFORM



      ******************************************************************
      *                   ALIMENTADOR DO ARRAY                         *
      ******************************************************************
           MOVE SPACES             TO EXT
           MOVE ZEROS              TO IND-EVT
           MOVE ZEROS              TO IND

           PERFORM UNTIL EXT = 'S' OR 's'
               ADD 1              TO IND-EVT
               DISPLAY ' INFORME O NUMERO DO DIA DA SEMANA '
               ACCEPT IND
               DISPLAY ' INFORME O NUMERO DO EVENTO '
               ACCEPT NUM-EVT(IND, IND-EVT)
               DISPLAY ' INFORME A DESCRICAO DO EVENTO '
               ACCEPT DSC-EVT(IND, IND-EVT)

           END-PERFORM

      ******************************************************************
      *                        LEITOR DO ARRAY                         *
      ******************************************************************



            DISPLAY ' NUMERO:        NOME: '

           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND GREATER 7
           PERFORM VARYING IND-EVT FROM 1 BY 1 UNTIL IND-EVT GREATER 4
               IF NUM-EVT(IND, IND-EVT) > 0 THEN
                   DISPLAY DAY-NMB(IND) ' - '
                           DAY-NM(IND) ' EVENTO: '
                   DISPLAY NUM-EVT(IND, IND-EVT)
                   DISPLAY ' - '
                   DISPLAY DSC-EVT(IND, IND-EVT)
           END-PERFORM
           END-PERFORM


            STOP RUN.
       END PROGRAM OCCRS.
