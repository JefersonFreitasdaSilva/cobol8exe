       identification division.
       PROGRAM-ID. A.
       AUTHOR. JEFERSON.
       INSTALLATION. FATEC.
       DATE-WRITTEN. 27/02/2023.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBMPC.
       OBJECT-COMPUTER. IBMPC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       data division.

       SCREEN SECTION.
       01 TELA.
         02 BLANK SCREEN.
         02 LINE 3 COL 10 VALUE "Trabalho de ILP507A (EM01)".
         02 LINE 4 COL 10 VALUE "Prof.: HIROMASA NAGATA".

       01 TELA-PRINCIPAL.
         02 LINE 6 COL 10 VALUE 'Integrantes:'.
         02 LINE 6 COL 40 VALUE 'RA:'.
         02 LINE 7 COL 10 VALUE 'Jeferson Freitas da silva'.
         02 LINE 7 COL 40 VALUE '21106881'.
         02 LINE 7 COL 10 VALUE 'Jeferson Freitas da silva'.
         02 LINE 7 COL 40 VALUE '21106881'.



       PROCEDURE DIVISION.
         DISPLAY ERASE.
         DISPLAY TELA.
         DISPLAY TELA-PRINCIPAL.

