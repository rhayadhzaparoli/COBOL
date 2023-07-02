     ******************************************************************
      * Author: RHAYADH ZAPAROLI
      * Date: 02/07/2023
      * Purpose: EXEMPLO DE UNSTRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNSTRINGED.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-POINTER          PIC 9(02)   VALUE 0.
       77  WS-COUNTER-1        PIC 9(02)   VALUE 0.
       77  WS-COUNTER-2        PIC 9(02)   VALUE 0.
       77  WS-COUNTER-3        PIC 9(02)   VALUE 0.
       77  WS-TOTAL-FIELDS     PIC 9(02)   VALUE 0.
       77  WS-COMP-NAME        PIC X(60)   VALUE SPACES.
       77  WS-FIRST-NAME       PIC X(20)   VALUE SPACES.
       77  WS-MIDDLE-NAME      PIC X(20)   VALUE SPACES.
       77  WS-LAST-NAME        PIC X(20)   VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            INITIALISE         WS-POINTER
                               WS-COUNTER-1
                               WS-COUNTER-2
                               WS-COUNTER-3
                               WS-TOTAL-FIELDS
                               WS-FIRST-NAME
                               WS-MIDDLE-NAME
                               WS-LAST-NAME

      *      MOVE 'RHAYADH TAYNAN ZAPAROLI'          TO WS-COMP-NAME

      *      UNSTRING WS-COMP-NAME
      *               DELIMITED BY SPACES
      *               INTO WS-FIRST-NAME
      *                    WS-MIDDLE-NAME
      *                    WS-LAST-NAME
      *      END-UNSTRING.

      *      MOVE ' RHAYADH TAYNAN ZAPAROLI '          TO WS-COMP-NAME
      *      MOVE  1                                 TO WS-POINTER
      *         UNSTRING WS-COMP-NAME
      *                 DELIMITED BY SPACES
      *                 INTO WS-FIRST-NAME
      *                     WS-MIDDLE-NAME
      *                     WS-LAST-NAME
      *                  WITH POINTER WS-POINTER
      *                  TALLYING IN WS-TOTAL-FIELDS
      *         END-UNSTRING.


      *      DISPLAY WS-FIRST-NAME
      *      DISPLAY WS-MIDDLE-NAME
      *      DISPLAY WS-LAST-NAME
      *      DISPLAY "******************************************"
      *      DISPLAY WS-COMP-NAME
      *      DISPLAY "******************************************"
      *      DISPLAY WS-POINTER
      *      DISPLAY WS-TOTAL-FIELDS

      *      MOVE 'RHAYADH*TAYNAN*ZAPAROLI*'          TO WS-COMP-NAME
      *      MOVE  1                                  TO WS-POINTER
      *         UNSTRING WS-COMP-NAME
      *                 DELIMITED BY '*'
      *                 INTO WS-FIRST-NAME
      *                     WS-MIDDLE-NAME
      *                     WS-LAST-NAME
      *                  WITH POINTER WS-POINTER
      *                  TALLYING IN WS-TOTAL-FIELDS
      *         END-UNSTRING.


      *      DISPLAY WS-FIRST-NAME
      *      DISPLAY WS-MIDDLE-NAME
      *      DISPLAY WS-LAST-NAME
      *      DISPLAY "******************************************"
      *      DISPLAY WS-COMP-NAME
      *      DISPLAY "******************************************"
      *      DISPLAY WS-POINTER
      *      DISPLAY WS-TOTAL-FIELDS

            MOVE 'RHAYADH*TAYNAN;;;;;;;;ZAPAROLI*'   TO WS-COMP-NAME
            MOVE  1                                  TO WS-POINTER
               UNSTRING WS-COMP-NAME
                       DELIMITED BY '*' OR ALL ';'
                       INTO WS-FIRST-NAME      COUNT IN WS-COUNTER-1
                            WS-MIDDLE-NAME     COUNT IN WS-COUNTER-2
                            WS-LAST-NAME       COUNT IN WS-COUNTER-3
                        WITH POINTER WS-POINTER
                        TALLYING IN WS-TOTAL-FIELDS
               END-UNSTRING.


            DISPLAY WS-FIRST-NAME
            DISPLAY WS-MIDDLE-NAME
            DISPLAY WS-LAST-NAME
            DISPLAY "******************************************"
            DISPLAY WS-COMP-NAME
            DISPLAY "******************************************"
            DISPLAY WS-POINTER
            DISPLAY WS-TOTAL-FIELDS
            DISPLAY "******************************************"
            DISPLAY WS-COUNTER-1
            DISPLAY WS-COUNTER-2
            DISPLAY WS-COUNTER-3
            STOP RUN.
       END PROGRAM UNSTRINGED.
