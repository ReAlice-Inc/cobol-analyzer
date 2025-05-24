       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-REPORT-SYSTEM.
       AUTHOR. PM Office Hino.
      ******************************************************************
      * プログラム名: 販売実績集計プログラム
      * 機能概要    : 店舗マスター、商品マスター、販売実績データを読み込み、
      * ユーザーの選択により指定された帳票ファイルを出力する。
      * - 店舗別月間売上実績表(TENREP.TXT)
      * - 商品別販売実績表(SHOREP.TXT)
      * 作成者      : PM Office Hino
      * 作成日      : 2025-05-15
      * 修正日      : 2025-05-16 (行長規制対応)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AIX. *> 例: 環境に合わせて変更
       OBJECT-COMPUTER. IBM-AIX. *> 例: 環境に合わせて変更
       SPECIAL-NAMES.
           CONSOLE IS SYSCONSOLE. *> DISPLAY/ACCEPT用 (環境依存)

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TENPO-MASTER-FILE ASSIGN TO "TENMAS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TENMAS.
           SELECT SHOHIN-MASTER-FILE ASSIGN TO "SHOMAS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SHOMAS.
           SELECT URIAGE-JISSEKI-FILE ASSIGN TO "URIDAT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-URIAGE.
           SELECT TENPO-REPORT-FILE ASSIGN TO "TENREP.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TENREP.
           SELECT SHOHIN-REPORT-FILE ASSIGN TO "SHOREP.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SHOREP.

       DATA DIVISION.
       FILE SECTION.
       FD  TENPO-MASTER-FILE.
       01  TENPO-MASTER-RECORD.
           05 TM-CODE         PIC 9(03).
           05 TM-NAME         PIC X(20).

       FD  SHOHIN-MASTER-FILE.
       01  SHOHIN-MASTER-RECORD.
           05 SM-CODE         PIC X(04).
           05 SM-NAME         PIC X(30).
           05 SM-PRICE        PIC 9(05).

       FD  URIAGE-JISSEKI-FILE.
       01  URIAGE-JISSEKI-RECORD.
           05 UJ-DATE         PIC 9(08).
           05 UJ-TENPO-CODE   PIC 9(03).
           05 UJ-SHOHIN-CODE  PIC X(04).
           05 UJ-QUANTITY     PIC 9(03).
           05 UJ-AMOUNT       PIC 9(07).

       FD  TENPO-REPORT-FILE.
       01  TENPO-REPORT-LINE   PIC X(80).

       FD  SHOHIN-REPORT-FILE.
       01  SHOHIN-REPORT-LINE  PIC X(120).

       WORKING-STORAGE SECTION.
      * File Status
       01  FILE-STATUS-FIELDS.
           05 FS-TENMAS       PIC XX.
           05 FS-SHOMAS       PIC XX.
           05 FS-URIAGE       PIC XX.
           05 FS-TENREP       PIC XX.
           05 FS-SHOREP       PIC XX.

      * Flags and Counters
       01  WS-EOF-FLAGS.
           05 WS-TENMAS-EOF-FLG   PIC X VALUE 'N'.
              88 TENMAS-EOF       VALUE 'Y'.
           05 WS-SHOMAS-EOF-FLG   PIC X VALUE 'N'.
              88 SHOMAS-EOF       VALUE 'Y'.
           05 WS-URIAGE-EOF-FLG   PIC X VALUE 'N'.
              88 URIAGE-EOF       VALUE 'Y'.

       01  WS-USER-CHOICE      PIC X(1).
       01  WS-REPORT-DATE.
           05 WS-REPORT-YYYY      PIC 9(04) VALUE 2025. *> デフォルト年
           05 WS-REPORT-MM        PIC 9(02) VALUE 04.   *> デフォルト月
       01  WS-REPORT-YYYYMM    PIC X(06) VALUE "202504". *> デフォルト年月

       01  WS-MAX-TENPO        PIC 9(02) VALUE 10. *> 10店舗まで
       01  WS-MAX-SHOHIN       PIC 9(02) VALUE 30. *> 30商品まで
       01  WS-LOADED-TENPO-COUNT PIC 9(02) VALUE 0.
       01  WS-LOADED-SHOHIN-COUNT PIC 9(02) VALUE 0.

      * Store Master Data
       01  WS-TENPO-MASTER-TABLE.
           05 WS-TENPO-ENTRIES OCCURS 10 TIMES *> 10店舗分のテーブル
                               INDEXED BY TM-IDX.
              10 WS-TM-CODE    PIC 9(03).
              10 WS-TM-NAME    PIC X(20).

       01  WS-SHOHIN-MASTER-TABLE.
           05 WS-SHOHIN-ENTRIES OCCURS 30 TIMES *> 30商品分のテーブル
                                INDEXED BY SM-IDX.
              10 WS-SM-CODE    PIC X(04).
              10 WS-SM-NAME    PIC X(30).
              10 WS-SM-PRICE   PIC 9(05).

      * For Tenpo Report
       01  WS-TENPO-SALES-TABLE.
           05 WS-TENPO-SALES-ENTRIES OCCURS 10 TIMES *> 10店舗分のテーブル
                                     INDEXED BY TS-IDX.
              10 WS-TS-TENPO-CODE   PIC 9(03).
              10 WS-TS-TENPO-NAME   PIC X(20).
              10 WS-TS-MONTHLY-SALES PIC 9(09) VALUE 0.
       01  WS-GRAND-TOTAL-SALES     PIC 9(10) VALUE 0.
       01  WS-EDITED-AMOUNT         PIC ZZZ,ZZZ,ZZ9.
       01  WS-EDITED-GRAND-TOTAL    PIC ZZZ,ZZZ,ZZZ,ZZ9.

      * For Shohin Report
       01  WS-SHOHIN-SALES-TABLE.
           05 WS-SHOHIN-SALES-ENTRIES OCCURS 30 TIMES *> 30商品分のテーブル
                                      INDEXED BY SS-IDX.
              10 WS-SS-SHOHIN-CODE  PIC X(04).
              10 WS-SS-SHOHIN-NAME  PIC X(30).
              10 WS-SS-TOTAL-QTY-ED PIC ZZZZ9.
              10 WS-SS-TOTAL-AMT-ED PIC ZZZ,ZZZ,ZZ9.
              10 WS-SS-NUM-TOTAL-AMT PIC 9(09) VALUE 0.
              10 WS-SS-NUM-TOTAL-QTY PIC 9(05) VALUE 0.
              10 WS-SS-TENPO-DETAILS OCCURS 10 TIMES *> 10店舗分のテーブル
                                     INDEXED BY SST-IDX.
                 15 WS-SST-TENPO-CODE PIC 9(03).
                 15 WS-SST-TENPO-NAME PIC X(20).
                 15 WS-SST-QTY-ED     PIC ZZZZ9.
                 15 WS-SST-AMT-ED     PIC ZZZ,ZZZ,ZZ9.
                 15 WS-SST-NUM-QTY    PIC 9(05) VALUE 0.
                 15 WS-SST-NUM-AMT    PIC 9(09) VALUE 0.

       01  WS-SHOHIN-GRAND-TOTAL-AMT-ED  PIC ZZZ,ZZZ,ZZZ,ZZ9.
       01  WS-SHOHIN-NUM-GRAND-TOTAL-AMT PIC 9(10) VALUE 0.

      * Report Line Definitions
       01  WS-PRINT-LINE               PIC X(120).
       01  WS-DASHES-50                PIC X(50) VALUE ALL "-".
       01  WS-DASHES-80                PIC X(80) VALUE ALL "-".
       01  WS-DASHES-100               PIC X(100) VALUE ALL "-".
       01  WS-DASHES-120               PIC X(120) VALUE ALL "-".

       01  WS-TENREP-HEADER1           PIC X(80).
       01  WS-TENREP-HEADER2.
           05 FILLER                  PIC X(12) VALUE "店舗コード 店舗名".
           05 FILLER                  PIC X(22) VALUE SPACES. *> Adjust
           05 FILLER                  PIC X(12) VALUE "月間売上金額".
       01  WS-TENREP-DETAIL-LINE.
           05 WS-TRD-CODE             PIC 9(03).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 WS-TRD-NAME             PIC X(20).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(01) VALUE "¥".
           05 WS-TRD-AMOUNT           PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                  PIC X(39) VALUE SPACES. *> 固定長のため
       01  WS-TENREP-TOTAL-LINE.
           05 FILLER                  PIC X(27) VALUE "合計".
           05 FILLER                  PIC X(01) VALUE "¥".
           05 WS-TRT-GRAND-TOTAL      PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 FILLER                  PIC X(38) VALUE SPACES. *> 固定長のため

       01  WS-SHOREP-HEADER1           PIC X(120).
       01  WS-SHOREP-HEADER2.
           05 FILLER PIC X(11) VALUE "商品コード 商品名".
           05 FILLER PIC X(40) VALUE SPACES.
           05 FILLER PIC X(10) VALUE "総販売数量".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "総販売金額".
       01  WS-SHOREP-HEADER3.
           05 FILLER PIC X(02) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "店舗コード 店舗名".
           05 FILLER PIC X(22) VALUE SPACES.
           05 FILLER PIC X(08) VALUE "販売数量".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(08) VALUE "販売金額".
       01  WS-SHOREP-DETAIL1-LINE.
           05 WS-SRD1-CODE            PIC X(04).
           05 FILLER                  PIC X(01) VALUE SPACE.
           05 WS-SRD1-NAME            PIC X(30).
           05 FILLER                  PIC X(06) VALUE SPACES.
           05 WS-SRD1-TOTAL-QTY       PIC ZZZZ9.
           05 FILLER                  PIC X(05) VALUE SPACES.
           05 FILLER                  PIC X(01) VALUE "¥".
           05 WS-SRD1-TOTAL-AMT       PIC ZZZ,ZZZ,ZZ9.
       01  WS-SHOREP-DETAIL2-LINE.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 WS-SRD2-TENPO-CODE      PIC 9(03).
           05 FILLER                  PIC X(01) VALUE SPACE.
           05 WS-SRD2-TENPO-NAME      PIC X(20).
           05 FILLER                  PIC X(03) VALUE SPACES.
           05 WS-SRD2-QTY             PIC ZZZZ9.
           05 FILLER                  PIC X(05) VALUE SPACES.
           05 FILLER                  PIC X(01) VALUE "¥".
           05 WS-SRD2-AMT             PIC ZZZ,ZZZ,ZZ9.
       01  WS-SHOREP-TOTAL-LINE.
           05 FILLER                  PIC X(57) VALUE
             "総合計".
           05 FILLER                  PIC X(05) VALUE SPACES.
           05 FILLER                  PIC X(01) VALUE "¥".
           05 WS-SRT-GRAND-TOTAL      PIC ZZZ,ZZZ,ZZZ,ZZ9.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZE-PROCESS.

           PERFORM UNTIL WS-USER-CHOICE = '9'
               PERFORM 2000-USER-INPUT-ROUTINE
               EVALUATE WS-USER-CHOICE
                   WHEN '1'
                       PERFORM 3000-CREATE-TENPO-REPORT
                   WHEN '2'
                       PERFORM 4000-CREATE-SHOHIN-REPORT
                   WHEN '9'
                       DISPLAY "処理を終了します。" UPON SYSCONSOLE
                   WHEN OTHER
                       DISPLAY "無効な選択です。再度入力してください。"
                           UPON SYSCONSOLE
               END-EVALUATE
           END-PERFORM.

           PERFORM 9000-TERMINATE-PROCESS.
           STOP RUN.

      ******************************************************************
       1000-INITIALIZE-PROCESS.
      ******************************************************************
           DISPLAY "処理を開始します。" UPON SYSCONSOLE.
           INITIALIZE WS-TENPO-MASTER-TABLE WS-SHOHIN-MASTER-TABLE.
           INITIALIZE WS-TENPO-SALES-TABLE WS-SHOHIN-SALES-TABLE.

           PERFORM 1100-LOAD-TENPO-MASTER.
           PERFORM 1200-LOAD-SHOHIN-MASTER.
           PERFORM 1300-PREPARE-SALES-TABLES.
           PERFORM 1400-DETERMINE-REPORT-MONTH.
           DISPLAY "初期処理完了。" UPON SYSCONSOLE.

      ******************************************************************
       1100-LOAD-TENPO-MASTER.
      ******************************************************************
           OPEN INPUT TENPO-MASTER-FILE.
           IF FS-TENMAS NOT = "00" AND FS-TENMAS NOT = "05"
      * 05: Optional file not found for some systems
               DISPLAY "エラー: 店舗マスターファイルを開けません。STATUS: "
                       FS-TENMAS UPON SYSCONSOLE
               PERFORM 9000-TERMINATE-PROCESS
               STOP RUN
           END-IF.

           SET TENMAS-EOF TO FALSE.
           MOVE 0 TO WS-LOADED-TENPO-COUNT.
           PERFORM VARYING TM-IDX FROM 1 BY 1
                   UNTIL TENMAS-EOF OR TM-IDX > WS-MAX-TENPO
               READ TENPO-MASTER-FILE
                   AT END
                       SET TENMAS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LOADED-TENPO-COUNT
                       MOVE TM-CODE TO WS-TM-CODE(TM-IDX)
                       MOVE TM-NAME TO WS-TM-NAME(TM-IDX)
               END-READ
           END-PERFORM.
           CLOSE TENPO-MASTER-FILE.

           IF WS-LOADED-TENPO-COUNT = 0
               DISPLAY "エラー: 店舗マスターが読み込めませんでした。"
                   UPON SYSCONSOLE
               PERFORM 9000-TERMINATE-PROCESS
               STOP RUN
           END-IF.
           DISPLAY "店舗マスター読み込み完了: " WS-LOADED-TENPO-COUNT
                   "件" UPON SYSCONSOLE.

      ******************************************************************
       1200-LOAD-SHOHIN-MASTER.
      ******************************************************************
           OPEN INPUT SHOHIN-MASTER-FILE.
           IF FS-SHOMAS NOT = "00" AND FS-SHOMAS NOT = "05"
               DISPLAY "エラー: 商品マスターファイルを開けません。STATUS: "
                       FS-SHOMAS UPON SYSCONSOLE
               PERFORM 9000-TERMINATE-PROCESS
               STOP RUN
           END-IF.

           SET SHOMAS-EOF TO FALSE.
           MOVE 0 to WS-LOADED-SHOHIN-COUNT.
           PERFORM VARYING SM-IDX FROM 1 BY 1
                   UNTIL SHOMAS-EOF OR SM-IDX > WS-MAX-SHOHIN
               READ SHOHIN-MASTER-FILE
                   AT END
                       SET SHOMAS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LOADED-SHOHIN-COUNT
                       MOVE SM-CODE TO WS-SM-CODE(SM-IDX)
                       MOVE SM-NAME TO WS-SM-NAME(SM-IDX)
                       MOVE SM-PRICE TO WS-SM-PRICE(SM-IDX)
               END-READ
           END-PERFORM.
           CLOSE SHOHIN-MASTER-FILE.

           IF WS-LOADED-SHOHIN-COUNT = 0
               DISPLAY "エラー: 商品マスターが読み込めませんでした。"
                   UPON SYSCONSOLE
               PERFORM 9000-TERMINATE-PROCESS
               STOP RUN
           END-IF.
           DISPLAY "商品マスター読み込み完了: " WS-LOADED-SHOHIN-COUNT
                   "件" UPON SYSCONSOLE.

      ******************************************************************
       1300-PREPARE-SALES-TABLES.
      ******************************************************************
      * Initialize Tenpo Sales Table with master data
           PERFORM VARYING TS-IDX FROM 1 BY 1
                   UNTIL TS-IDX > WS-LOADED-TENPO-COUNT
               MOVE WS-TM-CODE(TS-IDX) TO WS-TS-TENPO-CODE(TS-IDX)
               MOVE WS-TM-NAME(TS-IDX) TO WS-TS-TENPO-NAME(TS-IDX)
               MOVE 0 TO WS-TS-MONTHLY-SALES(TS-IDX)
           END-PERFORM.
           MOVE 0 TO WS-GRAND-TOTAL-SALES.

      * Initialize Shohin Sales Table with master data
           PERFORM VARYING SS-IDX FROM 1 BY 1
                   UNTIL SS-IDX > WS-LOADED-SHOHIN-COUNT
               MOVE WS-SM-CODE(SS-IDX) TO WS-SS-SHOHIN-CODE(SS-IDX)
               MOVE WS-SM-NAME(SS-IDX) TO WS-SS-SHOHIN-NAME(SS-IDX)
               MOVE 0 TO WS-SS-NUM-TOTAL-QTY(SS-IDX)
               MOVE 0 TO WS-SS-NUM-TOTAL-AMT(SS-IDX)
               PERFORM VARYING SST-IDX FROM 1 BY 1
                       UNTIL SST-IDX > WS-LOADED-TENPO-COUNT
                   MOVE WS-TM-CODE(SST-IDX)
                       TO WS-SST-TENPO-CODE(SS-IDX, SST-IDX)
                   MOVE WS-TM-NAME(SST-IDX)
                       TO WS-SST-TENPO-NAME(SS-IDX, SST-IDX)
                   MOVE 0 TO WS-SST-NUM-QTY(SS-IDX, SST-IDX)
                   MOVE 0 TO WS-SST-NUM-AMT(SS-IDX, SST-IDX)
               END-PERFORM
           END-PERFORM.
           MOVE 0 TO WS-SHOHIN-NUM-GRAND-TOTAL-AMT.

      ******************************************************************
       1400-DETERMINE-REPORT-MONTH.
      ******************************************************************
           OPEN INPUT URIAGE-JISSEKI-FILE.
           IF FS-URIAGE NOT = "00" AND FS-URIAGE NOT = "05"
               DISPLAY "エラー: 販売実績ファイルを開けません。STATUS: "
                       FS-URIAGE UPON SYSCONSOLE
               CLOSE URIAGE-JISSEKI-FILE *> Ensure closed
               DISPLAY "デフォルト年月 (" WS-REPORT-YYYYMM
                       ") を使用します。" UPON SYSCONSOLE
               EXIT PARAGRAPH
           END-IF.

           SET URIAGE-EOF TO FALSE.
           READ URIAGE-JISSEKI-FILE
               AT END
                   SET URIAGE-EOF TO TRUE
                   DISPLAY "エラー: 販売実績ファイルが空です。"
                       UPON SYSCONSOLE
                   DISPLAY "デフォルト年月 (" WS-REPORT-YYYYMM
                           ") を使用します。" UPON SYSCONSOLE
               NOT AT END
                   MOVE UJ-DATE(1:4) TO WS-REPORT-YYYY
                   MOVE UJ-DATE(5:2) TO WS-REPORT-MM
                   MOVE UJ-DATE(1:6) TO WS-REPORT-YYYYMM
           END-READ.
           CLOSE URIAGE-JISSEKI-FILE.
           DISPLAY "報告対象年月: " WS-REPORT-YYYY "年"
                   WS-REPORT-MM "月" UPON SYSCONSOLE.

      ******************************************************************
       2000-USER-INPUT-ROUTINE.
      ******************************************************************
           DISPLAY " " UPON SYSCONSOLE.
           DISPLAY "------------------------------------" UPON SYSCONSOLE.
           DISPLAY "帳票作成メニュー" UPON SYSCONSOLE.
           DISPLAY "1: 店舗別月間売上実績表" UPON SYSCONSOLE.
           DISPLAY "2: 商品別販売実績表" UPON SYSCONSOLE.
           DISPLAY "9: 終了" UPON SYSCONSOLE.
           DISPLAY "------------------------------------" UPON SYSCONSOLE.
           DISPLAY "番号を選択: " WITH NO ADVANCING UPON SYSCONSOLE.
           ACCEPT WS-USER-CHOICE FROM SYSCONSOLE.

      ******************************************************************
       3000-CREATE-TENPO-REPORT.
      ******************************************************************
           DISPLAY "店舗別月間売上実績表を作成します..." UPON SYSCONSOLE.
           PERFORM 3100-RESET-TENPO-SALES.
           PERFORM 3200-CALCULATE-TENPO-SALES.
           PERFORM 3300-PRINT-TENPO-REPORT.
           DISPLAY "店舗別月間売上実績表を TENREP.TXT に出力しました。"
               UPON SYSCONSOLE.

      ******************************************************************
       3100-RESET-TENPO-SALES.
      ******************************************************************
           PERFORM VARYING TS-IDX FROM 1 BY 1
                   UNTIL TS-IDX > WS-LOADED-TENPO-COUNT
               MOVE 0 TO WS-TS-MONTHLY-SALES(TS-IDX)
           END-PERFORM.
           MOVE 0 TO WS-GRAND-TOTAL-SALES.

      ******************************************************************
       3200-CALCULATE-TENPO-SALES.
      ******************************************************************
           OPEN INPUT URIAGE-JISSEKI-FILE.
           IF FS-URIAGE NOT = "00" AND FS-URIAGE NOT = "05"
               DISPLAY "エラー(集計用): 販売実績ファイルを開けません。FS: "
                       FS-URIAGE UPON SYSCONSOLE
               EXIT PARAGRAPH
           END-IF.

           SET URIAGE-EOF TO FALSE.
           READ URIAGE-JISSEKI-FILE AT END SET URIAGE-EOF TO TRUE 
                   END-READ.

           PERFORM UNTIL URIAGE-EOF
               PERFORM VARYING TS-IDX FROM 1 BY 1
                       UNTIL TS-IDX > WS-LOADED-TENPO-COUNT
                   IF UJ-TENPO-CODE = WS-TS-TENPO-CODE(TS-IDX)
                       ADD UJ-AMOUNT TO WS-TS-MONTHLY-SALES(TS-IDX)
                       ADD UJ-AMOUNT TO WS-GRAND-TOTAL-SALES
                       EXIT PERFORM *> Inner loop for current record
                   END-IF
               END-PERFORM
               READ URIAGE-JISSEKI-FILE
                   AT END SET URIAGE-EOF TO TRUE
               END-READ
           END-PERFORM.
           CLOSE URIAGE-JISSEKI-FILE.

      ******************************************************************
       3300-PRINT-TENPO-REPORT.
      ******************************************************************
           OPEN OUTPUT TENPO-REPORT-FILE.
           IF FS-TENREP NOT = "00"
               DISPLAY "エラー: 店舗別帳票ファイルを開けません。STATUS: "
                       FS-TENREP UPON SYSCONSOLE
               EXIT PARAGRAPH
           END-IF.

      * Header
           STRING "店舗別月間売上実績表 ("
                  WS-REPORT-YYYY "年"
                  WS-REPORT-MM "月)"
                  DELIMITED BY SIZE INTO WS-TENREP-HEADER1.
           WRITE TENPO-REPORT-LINE FROM WS-TENREP-HEADER1.
           WRITE TENPO-REPORT-LINE FROM WS-DASHES-50.
           WRITE TENPO-REPORT-LINE FROM WS-TENREP-HEADER2.
           WRITE TENPO-REPORT-LINE FROM WS-DASHES-50.

      * Detail
           PERFORM VARYING TS-IDX FROM 1 BY 1
                   UNTIL TS-IDX > WS-LOADED-TENPO-COUNT
               MOVE WS-TS-TENPO-CODE(TS-IDX)
                   TO WS-TRD-CODE
               MOVE WS-TS-TENPO-NAME(TS-IDX)
                   TO WS-TRD-NAME
               MOVE WS-TS-MONTHLY-SALES(TS-IDX)
                   TO WS-TRD-AMOUNT
               WRITE TENPO-REPORT-LINE FROM WS-TENREP-DETAIL-LINE
           END-PERFORM.

      * Footer
           WRITE TENPO-REPORT-LINE FROM WS-DASHES-50.
           MOVE WS-GRAND-TOTAL-SALES TO WS-TRT-GRAND-TOTAL.
           WRITE TENPO-REPORT-LINE FROM WS-TENREP-TOTAL-LINE.
           CLOSE TENPO-REPORT-FILE.

      ******************************************************************
       4000-CREATE-SHOHIN-REPORT.
      ******************************************************************
           DISPLAY "商品別販売実績表を作成します..." UPON SYSCONSOLE.
           PERFORM 4100-RESET-SHOHIN-SALES.
           PERFORM 4200-CALCULATE-SHOHIN-SALES.
           PERFORM 4300-PRINT-SHOHIN-REPORT.
           DISPLAY "商品別販売実績表を SHOREP.TXT に出力しました。"
               UPON SYSCONSOLE.

      ******************************************************************
       4100-RESET-SHOHIN-SALES.
      ******************************************************************
           PERFORM VARYING SS-IDX FROM 1 BY 1
                   UNTIL SS-IDX > WS-LOADED-SHOHIN-COUNT
               MOVE 0 TO WS-SS-NUM-TOTAL-QTY(SS-IDX)
               MOVE 0 TO WS-SS-NUM-TOTAL-AMT(SS-IDX)
               PERFORM VARYING SST-IDX FROM 1 BY 1
                       UNTIL SST-IDX > WS-LOADED-TENPO-COUNT
                   MOVE 0 TO WS-SST-NUM-QTY(SS-IDX, SST-IDX)
                   MOVE 0 TO WS-SST-NUM-AMT(SS-IDX, SST-IDX)
               END-PERFORM
           END-PERFORM.
           MOVE 0 TO WS-SHOHIN-NUM-GRAND-TOTAL-AMT.

      ******************************************************************
       4200-CALCULATE-SHOHIN-SALES.
      ******************************************************************
           OPEN INPUT URIAGE-JISSEKI-FILE.
           IF FS-URIAGE NOT = "00" AND FS-URIAGE NOT = "05"
               DISPLAY "エラー(集計用): 販売実績ファイルを開けません。FS: "
                       FS-URIAGE UPON SYSCONSOLE
               EXIT PARAGRAPH
           END-IF.

           SET URIAGE-EOF TO FALSE.
           READ URIAGE-JISSEKI-FILE AT END SET URIAGE-EOF TO TRUE 
           END-READ.

           PERFORM UNTIL URIAGE-EOF
               PERFORM VARYING SS-IDX FROM 1 BY 1
                       UNTIL SS-IDX > WS-LOADED-SHOHIN-COUNT
                 IF UJ-SHOHIN-CODE = WS-SS-SHOHIN-CODE(SS-IDX)
                   ADD UJ-QUANTITY TO WS-SS-NUM-TOTAL-QTY(SS-IDX)
                   ADD UJ-AMOUNT TO WS-SS-NUM-TOTAL-AMT(SS-IDX)
                   ADD UJ-AMOUNT TO WS-SHOHIN-NUM-GRAND-TOTAL-AMT

                   PERFORM VARYING SST-IDX FROM 1 BY 1
                           UNTIL SST-IDX > WS-LOADED-TENPO-COUNT
                     IF UJ-TENPO-CODE =
                        WS-SST-TENPO-CODE(SS-IDX, SST-IDX)
                       ADD UJ-QUANTITY
                           TO WS-SST-NUM-QTY(SS-IDX, SST-IDX)
                       ADD UJ-AMOUNT
                           TO WS-SST-NUM-AMT(SS-IDX, SST-IDX)
                       EXIT PERFORM *> Inner tenpo loop
                     END-IF
                   END-PERFORM
                   EXIT PERFORM *> Inner shohin loop
                 END-IF
               END-PERFORM
               READ URIAGE-JISSEKI-FILE
                   AT END SET URIAGE-EOF TO TRUE
               END-READ
           END-PERFORM.
           CLOSE URIAGE-JISSEKI-FILE.

      ******************************************************************
       4300-PRINT-SHOHIN-REPORT.
      ******************************************************************
           OPEN OUTPUT SHOHIN-REPORT-FILE.
           IF FS-SHOREP NOT = "00"
               DISPLAY "エラー: 商品別帳票ファイルを開けません。STATUS: "
                       FS-SHOREP UPON SYSCONSOLE
               EXIT PARAGRAPH
           END-IF.

      * Header
           STRING "商品別販売実績表 ("
                  WS-REPORT-YYYY "年"
                  WS-REPORT-MM "月)"
                  DELIMITED BY SIZE INTO WS-SHOREP-HEADER1.
           WRITE SHOHIN-REPORT-LINE FROM WS-SHOREP-HEADER1.
           WRITE SHOHIN-REPORT-LINE FROM WS-DASHES-100.
           WRITE SHOHIN-REPORT-LINE FROM WS-SHOREP-HEADER2.
           WRITE SHOHIN-REPORT-LINE FROM WS-SHOREP-HEADER3.
           WRITE SHOHIN-REPORT-LINE FROM WS-DASHES-100.

      * Detail
           PERFORM VARYING SS-IDX FROM 1 BY 1
                   UNTIL SS-IDX > WS-LOADED-SHOHIN-COUNT
             IF WS-SS-NUM-TOTAL-QTY(SS-IDX) > 0 OR
                WS-SS-NUM-TOTAL-AMT(SS-IDX) > 0

               MOVE WS-SS-SHOHIN-CODE(SS-IDX)
                   TO WS-SRD1-CODE
               MOVE WS-SS-SHOHIN-NAME(SS-IDX)
                   TO WS-SRD1-NAME
               MOVE WS-SS-NUM-TOTAL-QTY(SS-IDX)
                   TO WS-SRD1-TOTAL-QTY
               MOVE WS-SS-NUM-TOTAL-AMT(SS-IDX)
                   TO WS-SRD1-TOTAL-AMT
               WRITE SHOHIN-REPORT-LINE FROM WS-SHOREP-DETAIL1-LINE

               PERFORM VARYING SST-IDX FROM 1 BY 1
                       UNTIL SST-IDX > WS-LOADED-TENPO-COUNT
                 IF WS-SST-NUM-QTY(SS-IDX, SST-IDX) > 0 OR
                    WS-SST-NUM-AMT(SS-IDX, SST-IDX) > 0
                    MOVE WS-SST-TENPO-CODE(SS-IDX, SST-IDX)
                        TO WS-SRD2-TENPO-CODE
                    MOVE WS-SST-TENPO-NAME(SS-IDX, SST-IDX)
                        TO WS-SRD2-TENPO-NAME
                    MOVE WS-SST-NUM-QTY(SS-IDX, SST-IDX)
                        TO WS-SRD2-QTY
                    MOVE WS-SST-NUM-AMT(SS-IDX, SST-IDX)
                        TO WS-SRD2-AMT
                    WRITE SHOHIN-REPORT-LINE FROM WS-SHOREP-DETAIL2-LINE
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM.

      * Footer
           WRITE SHOHIN-REPORT-LINE FROM WS-DASHES-100.
           MOVE WS-SHOHIN-NUM-GRAND-TOTAL-AMT
               TO WS-SRT-GRAND-TOTAL.
           WRITE SHOHIN-REPORT-LINE FROM WS-SHOREP-TOTAL-LINE.
           CLOSE SHOHIN-REPORT-FILE.

      ******************************************************************
       9000-TERMINATE-PROCESS.
      ******************************************************************
           DISPLAY " " UPON SYSCONSOLE.
           CLOSE TENPO-MASTER-FILE SHOHIN-MASTER-FILE
                 URIAGE-JISSEKI-FILE
                 TENPO-REPORT-FILE SHOHIN-REPORT-FILE.
      * Implicit close if files are not open, but good practice

       END PROGRAM SALES-REPORT-SYSTEM.