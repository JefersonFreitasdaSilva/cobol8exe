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

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77 AA PIC 99.
       77 BB PIC 99.
       77 XX PIC 99V99.

       SCREEN SECTION.

       01 TELA.
         02 LINE 3 COL 10 VALUE "CALCULAR EQUACAO DO 1 GRAU".
         02 LINE 4 COL 10 VALUE "Ax + B (O VALOR DE A != DE ZERO)".

       01 TELA-PRINCIPAL.
         02 LINE 6 COL 10 VALUE "VALOR DE A:".
         02 LINE 7 COL 10 VALUE "VALOR DE B:".
         02 LINE 9 COL 10 VALUE "VALOR DE X:".

       01 TELA-A.
         02 LINE 6 COL 22 PIC 99 TO AA.

       01 TELA-B.
         02 LINE 7 COL 22 PIC 99 TO BB.

       01 TELA-ERROR.
         02 LINE 10 COL 10 VALUE "VALOR INVALIDO A=0".

       01 TELA-RESULT.
         02 LINE 9 COL 22 PIC 99,99 FROM XX REVERSE-VIDEO.

       PROCEDURE DIVISION.
           DISPLAY ERASE.
           DISPLAY TELA.
           DISPLAY TELA-PRINCIPAL.
           PERFORM PRINCIPAL.

           PRINCIPAL.
             ACCEPT TELA-A.
             ACCEPT TELA-B.

             IF AA NOT = 0
               PERFORM CALCULAR
             ELSE
               PERFORM ERRORR.

           ERRORR.
            DISPLAY TELA-ERROR.

           CALCULAR.
            COMPUTE XX= -BB/AA.
            DISPLAY TELA-RESULT.
