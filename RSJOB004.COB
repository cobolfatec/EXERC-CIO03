       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   RSPRG001.
      * AUTOR .........: ALVARO PEREIRA DO NASCIMENTO.
      * DATA DE CRIACAO: 19/03/2019.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       FILE-CONTROL.
      *==> LOCAL PARA O SELECT DOS ARQUVOS

       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
      *==> LOCAL PARA A FD (DESCRICAO DOS ARQUIVOS)

       WORKING-STORAGE SECTION.
      *-----------------------*

      *-----> AREA AUXILIAR
       77  AS-FIM                 PIC X(01) VALUE "N".
       77  AS-CEP                 PIC 9(8).
       77  AS-HORA                PIC 9(8).
       77  AS-FRENTE              PIC 9(3)V99.
       77  AS-FUNDO               PIC 9(3)V99.
       77  AS-VALOR-METRO         PIC 9(5)V99.
       77  AS-VAL-VENDA           PIC 9(7)V99.
       77  AS-COMISSAO            PIC 9(6)V99.
       77  AS-HORA-ATUAL          PIC 99.99.9999.

      *-----> DADOS DE SAIDA VIA SYSOUT

       01  WS-REG-SYSOUT.
           05 WS-CEP              PIC 9(8).
           05 FILLER              PIC X(1).
           05 WS-FRENTE           PIC 9(3)V99.
           05 FILLER              PIC X(2)        VALUE "M ".
           05 WS-FUNDO            PIC 9(3)V99.
           05 FILLER              PIC X(2)        VALUE "M ".
           05 WS-VAL-METRO        PIC 9(5)V99.
           05 FILLER              PIC X(2)        VALUE SPACE.
           05 WS-VAL-VENDA        PIC 9(7)V99.
           05 FILLER              PIC X(2)        VALUE SPACE.
           05 WS-COMISSAO         PIC 9(6)V99.
           05 FILLER              PIC X(2)        VALUE SPACE.
           05 WS-MENSSAGEM        PIC X(25).

       LINKAGE SECTION.
      *----------------*
       01  LK-PARAMETROS.
           05 LK-NR-DPTO             PIC 9(04).
           05 LK-NOME-DPTO           PIC X(15).
           05 LK-COD-RETORNO         PIC 99.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL                                        *
      *--------------------------------------------------------------*

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "** ATIVIDADE 3**"
           DISPLAY " ALVARO PEREIRA DO NASCIMENTO"
           DISPLAY " LUCAS YOSHIKAWA"
           DISPLAY "CALCULO DO PRECO DE VENDA DE UM TERRENO RETANGULAR"
           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.

           ACCEPT WS-HORA-ATUAL FROM TIME
           DISPLAY "HORA DO CALCULO: " WS-DATA-ATUAL
           MOVE '09000300 ' TO   WS-CEP
           MOVE  12,35      TO   AS-FRENTE
           MOVE  52,00      TO   AS-FUNDO
           MOVE  2315,000   TO   AS-VAL-METRO
           COMPUTE AS-VAL-VENDA = (AS-FRENTE*AS-FUNDO)*AS-VAL-METRO
           IF AS-VAL-VENDA > 1500000,00
            WS-COMISSAO = AS-VALVENDA*o.04
            WS-MESSAGEM  TO "CASA DE ALTO PADRÃO"
          ELSE
          END-IF.
           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '** FIM **'

           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA RSPRG001"
           .
      *---------------> FIM DO PROGRAMA RSPRG001 <-------------------*
