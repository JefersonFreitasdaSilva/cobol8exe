       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EMEX01.
       AUTHOR.           DANIEL LISBOA.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     17/09/2022.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS. LE CADSOC1 REALIZA PROCEDIMENTOS
      * E IMPRIME UM RELATORIO.
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. IBM-PC.
             OBJECT-COMPUTER. IBM-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.


       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CAD-SOC1 ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT CAD-SOC2 ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT REL-ATR ASSIGN TO DISK. 

       DATA DIVISION.
       FILE SECTION.

       FD CAD-SOC1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-SOC1.DAT".

       01 REG-SOC1.
           02 CODPAG-SOC1     PIC 9(2).
           02 NUMSOC-SOC1     PIC 9(6).
           02 NAME-SOC1       PIC X(30).
           02 VALORPAG-SOC1   PIC 9(9)V9(2).

       FD REL-ATR
           LABEL RECORD IS OMITTED.
       01 REG-ATR            PIC X(80).

       FD CAD-SOC2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-SOC2.DAT".

       01 REG-SOC2.
           02 NUMSOC-SOC2      PIC 9(6).
           02 NOME-SOC2        PIC X(30).
           02 VALORPAG-SOC2    PIC 9(9)V9(2).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ      PIC X(3)       VALUE "NAO".
       77 CT-LIN       PIC X(5)       VALUE "PAG. ".
       77 CT-PAG       PIC 9(2)       VALUE ZERO.
       77 REG-COUNT    PIC 9(2)       VALUE 30.
       77 TOTALSOC-A   PIC 9(5)       VALUE ZEROES.
       77 TOTALVAL-A   PIC 9(12)V9(2)  VALUE ZEROES.

       01 CAB-01.
          02 FILLER    PIC X(70) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG PIC 99.
          02 FILLER    PIC X(03) VALUE SPACES.

       01  CAB-02.
           02 FILLER   PIC X(26)    VALUE SPACES.
           02 FILLER   PIC X(27)    VALUE "RELACAO DE SOCIOS ATRASADOS".
           02 FILLER   PIC X(27)    VALUE SPACES.

       01  CAB-03.
           02 FILLER  PIC X(2)    VALUE SPACES.
           02 FILLER  PIC X(15)  VALUE "NUMERO DO SOCIO".
           02 FILLER  PIC X(15)    VALUE SPACES.
           02 FILLER  PIC X(13)  VALUE "NOME DO SOCIO".
           02 FILLER  PIC X(15)    VALUE SPACES.
           02 FILLER  PIC X(18)  VALUE "VALOR DO PAGAMENTO".
           02 FILLER  PIC X(2)    VALUE SPACES.

       01  CA-DADOS.
           02 FILLER  PIC X(2)    VALUE SPACES.
           02 NUMSOC  PIC 9(6).
           02 FILLER  PIC X(24)   VALUE SPACES.
           02 NOME    PIC X(30).
           02 FILLER  PIC X(2)    VALUE SPACES.
           02 VALORPAG PIC 9(9)V9(2).
           02 FILLER  PIC X(4)    VALUE SPACES.

       01 CA-TOTALSOC.
           02 FILLER PIC X(28)  VALUE "Total de Socios atrasados : ".
           02 TOTALSOC PIC 9(05).
           02 FILLER  PIC X(47)    VALUE SPACES.

       01 CA-TOTALSOC2.
           02 FILLER PIC X(28)  VALUE "Valor Total atrasado      : ".
           02 TOTALPAG PIC 9(12)V9(2).
           02 FILLER  PIC X(37)    VALUE SPACES.
          
        
       01 PULALINHA.
           02 FILLER PIC X(80) VALUE SPACES.

       PROCEDURE DIVISION.

       PGM-EMEX01.
           PERFORM INICIO.
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIMIMPRI.
           PERFORM FIM.
           STOP RUN.
       

       INICIO.
           OPEN 
                INPUT   CAD-SOC1
                OUTPUT  CAD-SOC2
                OUTPUT  REL-ATR.
       
       PERFORM LEITURA.
       
       LEITURA.
           READ CAD-SOC1 AT END MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           PERFORM VALIDA.
           PERFORM LEITURA.
       
       VALIDA.
           
           IF CODPAG-SOC1 EQUALS 2
               ADD 1 TO TOTALSOC-A;
               ADD VALORPAG-SOC1 TO TOTALVAL-A      
               PERFORM IMPRESSAO
           ELSE
               PERFORM GRAVACAO.

       GRAVACAO.
           MOVE NUMSOC-SOC1 TO NUMSOC-SOC2.
           MOVE NAME-SOC1   TO NOME-SOC2.
           MOVE VALORPAG-SOC1 TO VALORPAG-SOC2.
           WRITE REG-SOC2.
           
       IMPRESSAO.
           
           IF REG-COUNT EQUALS 30
               PERFORM CABECALHO.
           PERFORM IMPDET.
       
       
       CABECALHO.
           MOVE ZEROES TO REG-COUNT.
           ADD 1 TO CT-PAG.
           MOVE CT-PAG TO VAR-PAG.
           MOVE SPACES TO REG-ATR.
           WRITE REG-ATR AFTER ADVANCING PAGE.
           WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 2 LINES.

       IMPDET.           
           MOVE NUMSOC-SOC1 TO NUMSOC.
           MOVE NAME-SOC1 TO NOME.
           MOVE VALORPAG-SOC1 TO VALORPAG.
           WRITE REG-ATR FROM CA-DADOS AFTER ADVANCING 1 LINE.
           ADD 1 TO REG-COUNT.

       FIMIMPRI.
           WRITE REG-ATR FROM PULALINHA AFTER ADVANCING 1 LINE.
           MOVE TOTALSOC-A TO TOTALSOC.
           MOVE TOTALVAL-A TO TOTALPAG.
           WRITE REG-ATR FROM CA-TOTALSOC AFTER ADVANCING 1 LINE.
           WRITE REG-ATR FROM CA-TOTALSOC2 AFTER ADVANCING 1 LINE.
           

           
       FIM.
           CLOSE   CAD-SOC1
                   CAD-SOC1.