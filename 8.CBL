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
       77 LADOA PIC 99.
       77 LADOB PIC 99.
       77 LADOC PIC 99.

       SCREEN SECTION.
       01 TELA.
         02 BLANK SCREEN.
         02 LINE 3 COL 10 VALUE "TRIANGULO".

       01 TELA-PRINCIPAL.
         02 LINE 5 COL 10 VALUE 'DIGITE LADO A:'.
         02 LINE 6 COL 10 VALUE 'DIGITE LADO B:'.
         02 LINE 7 COL 10 VALUE 'DIGITE LADO C:'.

       01 TELA-A.
         02 LINE 5 COL 25 PIC 99 TO LADOA.

       01 TELA-B.
         02 LINE 6 COL 25 PIC 99 TO LADOB.

       01 TELA-C.
         02 LINE 7 COL 25 PIC 99 TO LADOC.

       01 TELA-EQUI.
         02 LINE 9 COL 10 VALUE 'E UM TRIANGULO EQUILATERO'.

       01 TELA-ESC.
         02 LINE 9 COL 10 VALUE 'E UM TRIANGULO ESCALENO'.

       01 TELA-ISO.
         02 LINE 9 COL 10 VALUE 'E UM TRIANGULO ISOSCELES'.

       01 TELA-NAO.
         02 LINE 9 COL 10 VALUE 'NAO E UM TRIANGULO'.

       PROCEDURE DIVISION.
         DISPLAY ERASE.
         DISPLAY TELA.
         DISPLAY TELA-PRINCIPAL.
         PERFORM PRINCIPAL.


       PRINCIPAL.
         ACCEPT TELA-A.
         ACCEPT TELA-B.
         ACCEPT TELA-C.
         IF LADOA + LADOB > LADOC AND LADOA + LADOC > LADOB AND
         LADOB + LADOC > LADOA
            IF LADOA = LADOB AND LADOB = LADOC
              DISPLAY TELA-EQUI
            ELSE
              IF LADOA NOT = LADOB AND LADOA NOT = LADOC AND
              LADOB NOT = LADOC
                DISPLAY TELA-ESC
              ELSE
                display tela-iso
         ELSE
            DISPLAY TELA-NAO.

