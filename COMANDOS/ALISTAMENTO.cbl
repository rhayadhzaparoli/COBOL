       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALISTAMENTO-MILITAR.
      * AUTHOR. RHAYADH ZAPAROLI.
      * DATE-WRITTEN. 01/07/2023.
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
         05 ADRESS     PIC X(80) VALUE SPACES.
         05 MEDIA       PIC 99V99.

       WORKING-STORAGE SECTION.
       01 WS-DOB.
         05 WS-DOB-YY  PIC 9(04) VALUE 0.

       01 WS-CURR-DATE.
         05 WS-CURR-YY PIC 9(04) VALUE 0.
         05 WS-CURR-MM PIC 99 VALUE 0.
         05 WS-CURR-DD PIC 99 VALUE 0.

       01 WS-PRS-DATA.
         05 WS-NAME    PIC X(50) VALUE SPACES.
         05 WS-RG      PIC 9(10) VALUE 0.
         05 WS-CPF     PIC 9(11) VALUE 0.
         05 WS-ADDRESS PIC X(80) VALUE SPACES.

       01 WS-N1        PIC 9(02)V99 VALUE 0.
       01 WS-N2        PIC 9(02)V99 VALUE 0.
       01 WS-N3        PIC 9(02)V99 VALUE 0.
       01 WS-N4        PIC 9(02)V99 VALUE 0.
       01 WS-MEDIA     PIC 99V99.
       01 WS-KEY       PIC X.
       01 WS-KEY-1     PIC A(01).
       01 WS-STATUS    PIC 99.
       01 WS-CALC-AGE  PIC 99 VALUE 0.
       01 WS-CHC       PIC 9(02) VALUE 0.
       01 WS-MAT       PIC 9(05).
       01 WS-MAT-ESP   PIC 9(03).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "CADASTRAMENTO MILITAR"
           DISPLAY "INFORME O ANO DE NASCIMENTO"
           ACCEPT WS-DOB-YY
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE
           COMPUTE WS-CALC-AGE = FUNCTION NUMVAL(WS-CURR-YY) -
                                 FUNCTION NUMVAL(WS-DOB-YY)
           DISPLAY "IDADE: " WS-CALC-AGE

           IF WS-CALC-AGE >= 18 THEN
           DISPLAY "IDADE PERMITIDA"
           PERFORM P200-VALIDATION
           ELSE
           DISPLAY "IDADE NAO PERMITIDA"
           GO TO P900-END
           END-IF.

       P200-VALIDATION.
           DISPLAY "DIGITE O NOME"
           ACCEPT WS-NAME
           DISPLAY "DIGITE O RG"
           ACCEPT WS-RG
           DISPLAY "DIGITE O CPF"
           ACCEPT WS-CPF
           DISPLAY "DIGITE O ENDERECO"
           ACCEPT WS-ADDRESS

           DISPLAY "VERIFIQUE OS DADOS"
           DISPLAY WS-NAME
           DISPLAY WS-RG
           DISPLAY WS-CPF
           DISPLAY WS-ADDRESS
           DISPLAY "DESEJA ALTERAR DADOS? (S/N)"
           ACCEPT WS-KEY

           IF WS-KEY = 'S' OR WS-KEY = 's' THEN
           PERFORM P300-CHC
           ELSE
           GO TO P400-QI
           END-IF.
       P250-VERIFY.
           DISPLAY "VERIFIQUE OS DADOS"
           DISPLAY WS-NAME
           DISPLAY WS-RG
           DISPLAY WS-CPF
           DISPLAY WS-ADDRESS
           DISPLAY "DESEJA ALTERAR DADOS ?"
           ACCEPT WS-KEY
           IF WS-KEY = 'S' OR WS-KEY = 's' THEN
              GO TO P300-CHC
           ELSE
               GO TO P400-QI.
       P300-CHC.
           DISPLAY "1 PARA NOME"
           DISPLAY "2 PARA RG"
           DISPLAY "3 PARA CPF"
           DISPLAY "4 PARA ENDERECO"
           DISPLAY "0 PARA SAIR"
           ACCEPT WS-CHC.

           EVALUATE WS-CHC
           WHEN 1
               DISPLAY "DIGITE O NOME"
               ACCEPT WS-NAME
           WHEN 2
               DISPLAY "DIGITE O RG"
               ACCEPT WS-RG
           WHEN 3
               DISPLAY "DIGITE O CPF"
               ACCEPT WS-CPF
           WHEN 4
               DISPLAY "DIGITE O ENDERECO"
               ACCEPT WS-ADDRESS
           WHEN 0
               GO TO P900-END
           END-EVALUATE.

           GO TO P250-VERIFY.

       P400-QI.
           DISPLAY "DIGITE A NOTA DOS 4 ULTIMOS BIMESTRES"
           DISPLAY "NOTA 1"
           ACCEPT WS-N1
           DISPLAY "NOTA 2"
           ACCEPT WS-N2
           DISPLAY "NOTA 3"
           ACCEPT WS-N3
           DISPLAY "NOTA 4"
           ACCEPT WS-N4

           COMPUTE WS-MEDIA = (WS-N1 + WS-N2 + WS-N3 + WS-N4) / 4
           DISPLAY WS-MEDIA

           IF WS-MEDIA >= 9.5 THEN
           DISPLAY "Q.I. AVANCADO"
           PERFORM P600-SESSION
           ELSE
           PERFORM P600-SESSION
           END-IF.

       P600-SESSION.
           IF WS-MEDIA >= 9.5 AND WS-MEDIA <= 10.00 THEN
           DISPLAY "SETOR ESPECIAL"
           DISPLAY WS-NAME
           DISPLAY WS-RG
           DISPLAY WS-CPF
           DISPLAY WS-ADDRESS
           PERFORM P550-MAT-ESP
           GO TO P900-END
           ELSE
           DISPLAY "SETOR COMUM"
           DISPLAY WS-NAME
           DISPLAY WS-RG
           DISPLAY WS-CPF
           DISPLAY WS-ADDRESS
           PERFORM P500-MAT
           GO TO P900-END
           END-IF.

       P500-MAT.
           DISPLAY "GERANDO A MATRICULA....."
           MOVE FUNCTION RANDOM(1) TO WS-MAT
           DISPLAY WS-MAT.

           P550-MAT-ESP.
           DISPLAY "GERANDO A MATRICULA ESPECIAL....."
           MOVE FUNCTION RANDOM(1) TO WS-MAT-ESP
           DISPLAY WS-MAT-ESP.

       P900-END.
           OPEN EXTEND REG-FILE
           IF WS-STATUS = 0 THEN
           MOVE WS-NAME TO NAMES
           MOVE WS-MEDIA TO MEDIA
           MOVE WS-PRS-DATA TO PRS-DATA
           WRITE PRS-DATA
           DISPLAY "REGISTRO GRAVADO"
           DISPLAY "STATUS: " WS-STATUS
           ELSE
           DISPLAY "NAO FOI POSSIVEL GRAVAR O REGISTRO!"
           DISPLAY "FILE STATUS: " WS-STATUS
           END-IF.

           CLOSE REG-FILE
           DISPLAY "FINALIZANDO O PROGRAMA..."
           STOP RUN.
       END PROGRAM ALISTAMENTO-MILITAR.
