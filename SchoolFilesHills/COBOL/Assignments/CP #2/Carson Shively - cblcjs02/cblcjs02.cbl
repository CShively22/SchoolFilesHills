       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLCJS02.
       DATE-WRITTEN. 01/06/2022.
       AUTHOR. CARSON SHIVELY.
       DATE-COMPILED.

      *****************************************************************
      * PROGRAM WILL TAKE BOAT INPUT FROM CBLBOAT1.DAT. THEN WILL
      * ORGANIZE AND OUTPUT THE DATA ON BOATRPT1.PRT. USES MAJOR CONTROL
      * BREAKS AND GRAND TOTALS WITH ACCUMULATORS.
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOAT-INPUT
               ASSIGN TO 'C:\COBOL\CBLBOAT1.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BOAT-PRT
               ASSIGN TO 'C:\COBOL\BOATRPT1.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD BOAT-INPUT
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 42 CHARACTERS
           DATA RECORD IS I-REC.

       01 I-REC.
           05  I-LAST-NAME         PIC X(15).
           05  I-STATE             PIC X(2).
           05  I-BOAT-COST         PIC 9(6)V99.
           05  I-PURCHASE-DATE.
               10  I-PURCHASE-YY   PIC 9(4).
               10  I-PURCHASE-MM   PIC 99.
               10  I-PURCHASE-DD   PIC 99.
           05  I-BOAT-TYPE         PIC X.
           05  I-ACC-PCK           PIC 9.
           05  I-PREP-DEL-COST     PIC 9(5)V99.

       FD BOAT-PRT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 25 WITH FOOTING 20
           DATA RECORD IS PRTLINE.

       01 PRTLINE                  PIC X(132).

       WORKING-STORAGE SECTION.

       01 I-DATE-TIME.
           05 I-DATE.
               10 I-YEAR           PIC 9(4).
               10 I-MONTH          PIC 99.
               10 I-DAY            PIC 99.
           05 I-TIME               PIC X(11).

       01 WORK-AREA.
      *        C-SWITHC FOR FIRST DETAIL LINE.
      *  0 - TO PRINT EXTRA SPACE |   1 - TO PRINT NO EXTRA SPACE
           05  C-SWITCH            PIC 9           VALUE   0.

           05  MORE-RECS           PIC XXX         VALUE   "YES".
           05  H-BOAT-TYPE         PIC X.
      *        CONVERTED ACCESSORY PCK STRING
           05  C-ACC-PCK           PIC X(15).
      *            ACCUMULATORS
           05  C-PG-CTR            PIC 99          VALUE   ZEROES.
      *            CALCULATIONS    
           05  C-TOTAL-COST        PIC 9(7)V99.
       
      *            MAJOR SUBTOTAL
           05  C-MJ-NUM-SOLD       PIC 9(4)        VALUE   ZEROES.
           05  C-MJ-TOT-COST       PIC 9(9)V99     VALUE   ZEROES.

      *            GRAND TOTAL
           05  C-GT-NUM-SOLD       PIC 9(5)        VALUE   ZEROES.
           05  C-GT-TOT-COST       PIC 9(11)V99    VALUE   ZEROES.

       01  COMPANY-TITLE.
           05  FILLER              PIC X(6)    VALUE   "DATE: ".
           05  O-MONTH             PIC 99.
           05  FILLER              PIC X       VALUE   "/".
           05  O-DAY               PIC 99.
           05  FILLER              PIC X       VALUE   "/".
           05  O-YEAR              PIC 9(4).
           05  FILLER              PIC X(37)   VALUE  SPACES.
           05  FILLER              PIC X(22)   VALUE "C SHIVELY'S BOATS 
      -                                        "INC.".
           05  FILLER              PIC X(49)   VALUE  SPACES.
           05  FILLER              PIC X(6)    VALUE  "PAGE: ".
           05  O-PG-CTR            PIC Z9.

       01  COL-HDG-1.
           05  FILLER              PIC X(8)    VALUE   "CUSTOMER".
           05  FILLER              PIC X(36)   VALUE   SPACES.
           05  FILLER              PIC X(4)    VALUE   "BOAT".
           05  FILLER              PIC X(9)    VALUE   SPACES.
           05  FILLER              PIC X(8)    VALUE   "PURCHASE".
           05  FILLER              PIC X(11)   VALUE   SPACES.
           05  FILLER              PIC X(9)    VALUE   "ACCESSORY".
           05  FILLER              PIC X(21)   VALUE   SPACES.
           05  FILLER              PIC X(4)    VALUE   "PREP".
           05  FILLER              PIC X(17)   VALUE   SPACES.
           05  FILLER              PIC X(5)    VALUE   "VALUE".

       01 COL-HDG-2.
           05  FILLER              PIC X(9)    VALUE   "LAST NAME".
           05  FILLER              PIC X(14)   VALUE   SPACES.
           05  FILLER              PIC X(5)    VALUE   "STATE".
           05  FILLER              PIC X(16)   VALUE   SPACES.
           05  FILLER              PIC X(4)    VALUE   "COST".
           05  FILLER              PIC X(9)    VALUE   SPACES.
           05  FILLER              PIC X(4)    VALUE   "DATE".
           05  FILLER              PIC X(15)   VALUE   SPACES.
           05  FILLER              PIC X(7)    VALUE   "PACKAGE".
           05  FILLER              PIC X(23)   VALUE   SPACES.
           05  FILLER              PIC X(4)    VALUE   "COST".
           05  FILLER              PIC X(18)   VALUE   SPACES.
           05  FILLER              PIC X(4)    VALUE   "COST".

       01  MAJOR-HEADINGS.
           05  FILLER              PIC X(11)   VALUE   "BOAT TYPE: ".
           05  O-BOAT-TYPE         PIC X(13).

       01  DETAIL-LINE.
           05  O-LAST-NAME         PIC X(16).
           05  FILLER              PIC X(8)    VALUE   SPACES.
           05  O-STATE             PIC XX.
           05  FILLER              PIC X(12)   VALUE   SPACES.
           05  O-BOAT-COST         PIC ZZZ,ZZZ.99.
           05  FILLER              PIC X(9)    VALUE   SPACES.
           05  O-PURCHASE-MM       PIC 99.
           05  FILLER              PIC X       VALUE   "/".
           05  O-PURCHASE-DD       PIC 99.
           05  FILLER              PIC X       VALUE   "/".
           05  O-PURCHASE-YY       PIC 99.
           05  FILLER              PIC X(11)   VALUE   SPACES.
           05  O-ACC-PCK           PIC X(15).
           05  FILLER              PIC X(9)    VALUE   SPACES.
           05  O-PREP-COST         PIC ZZZ,ZZZ.99.
           05  FILLER              PIC X(10)   VALUE   SPACES.
           05  O-TOTAL-COST        PIC Z,ZZZ,ZZZ.99.

       01  MAJOR-SUBTOTAL-LINE.
           05  FILLER              PIC X(23)   VALUE   SPACES.
           05  FILLER              PIC X(14)   VALUE   "SUBTOTALS FOR ".
           05  O-BOAT-TYPE1        PIC X(13).
           05  FILLER              PIC X(10)   VALUE   SPACES.
           05  FILLER              PIC X(14)   VALUE   "NUMBER SOLD:  ".
           05  O-MJ-NUM-SOLD       PIC Z,ZZ9.
           05  FILLER              PIC X(38)   VALUE   SPACES.
           05  O-MJ-TOT-COST       PIC $$$$,$$$,$$$.99.

       01  GRAND-TOTAL-LINE.
           05  FILLER              PIC X(23)   VALUE   SPACES.
           05  FILLER              PIC X(12)   VALUE   "GRAND TOTALS".
           05  FILLER              PIC X(25)   VALUE   SPACES.
           05  FILLER              PIC X(13)   VALUE   "NUMBER SOLD: ".
           05  O-GT-NUM-SOLD       PIC ZZ,ZZ9.
           05  FILLER              PIC X(35)   VALUE   SPACES.
           05  O-GT-TOT-COST       PIC $$$,$$$,$$$,$$$.99.

       01 BLANK-LINE.
           05  FILLER              PIC X(132)  VALUE   SPACES.
       

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.
       1000-INIT.
           OPEN INPUT BOAT-INPUT.
           OPEN OUTPUT BOAT-PRT.

           MOVE FUNCTION CURRENT-DATE TO I-DATE-TIME.
           MOVE I-DAY TO O-DAY.
           MOVE I-MONTH TO O-MONTH.
           MOVE I-YEAR TO O-YEAR.

      *    CALL THE READ TO GET THE DATA TO ORGANIZE MAJORS
           PERFORM 9000-READ.
      *    CALL HEADINGS AFTER READ
           PERFORM 9200-HEADINGS.
      *    MOVE THE DATA TO HOLD FIELD FOR MAJORS  
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
      *    CONVERT BOAT TYPE TO PROPER STRING FORM
           PERFORM 9300-CONVERT-BOAT-TYPE.
      *    JUST PRINT MAJOR HEADING USED TO ORGANIZE MAJORS.
           WRITE PRTLINE
               FROM MAJOR-HEADINGS
                   BEFORE ADVANCING 1 LINES.


       2000-MAINLINE.
           IF H-BOAT-TYPE IS NOT = I-BOAT-TYPE
               PERFORM 9100-MAJOR-SUBTOTALS.

           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           ADD I-BOAT-COST TO I-PREP-DEL-COST GIVING C-TOTAL-COST.

      *    DO MAJORS CALCULATIONS
           ADD C-TOTAL-COST TO C-MJ-TOT-COST.
           ADD 1 TO C-MJ-NUM-SOLD.

       2200-OUTPUT.
      *    CONVERTS THE INT ACCESSORY TO PROPER STRING THEN MOVES IT TO 
      *  C-ACC-PCK. USED FOR ORGANIZATION PURPOSES.
           PERFORM 2210-CONVERT-ACC-PCK.

           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-PURCHASE-MM TO O-PURCHASE-MM.
           MOVE I-PURCHASE-DD TO O-PURCHASE-DD.
           MOVE I-PURCHASE-YY TO O-PURCHASE-YY.
           MOVE C-ACC-PCK TO O-ACC-PCK.
           MOVE I-PREP-DEL-COST TO O-PREP-COST.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.

           WRITE PRTLINE
               FROM DETAIL-LINE
                    AFTER ADVANCING 1 LINES
                       AT EOP
                           PERFORM 9200-HEADINGS.

       2210-CONVERT-ACC-PCK.
           IF I-ACC-PCK = 1
                   MOVE "ELECTRONICS" TO C-ACC-PCK
           ELSE
               IF I-ACC-PCK = 2
                       MOVE "SKI PACKAGE" TO C-ACC-PCK
               ELSE
                       MOVE "FISHING PACKAGE" TO C-ACC-PCK.


       3000-CLOSING.
           PERFORM 9100-MAJOR-SUBTOTALS.
           PERFORM 3100-GRAND-TOTALS.
           
           CLOSE BOAT-INPUT.
           CLOSE BOAT-PRT.

       3100-GRAND-TOTALS.
      * MOVE GT VARIABLES TO O-FIELDS
           MOVE C-GT-NUM-SOLD TO O-GT-NUM-SOLD.
           MOVE C-GT-TOT-COST TO O-GT-TOT-COST.

           WRITE PRTLINE
               FROM GRAND-TOTAL-LINE
                   AFTER ADVANCING 3 LINES.

       9000-READ.
           READ BOAT-INPUT
               AT END
                   MOVE "NO" TO MORE-RECS.
               
       9100-MAJOR-SUBTOTALS.
           MOVE C-MJ-TOT-COST TO O-MJ-TOT-COST.
           MOVE C-MJ-NUM-SOLD  TO O-MJ-NUM-SOLD.

           WRITE PRTLINE
               FROM MAJOR-SUBTOTAL-LINE
                   AFTER ADVANCING 2 LINES
                       AT EOP
                           PERFORM 9200-HEADINGS.

           ADD C-MJ-NUM-SOLD TO C-GT-NUM-SOLD.
           ADD C-MJ-TOT-COST TO C-GT-TOT-COST.

           MOVE 0 TO C-MJ-NUM-SOLD.
           MOVE 0 TO C-MJ-TOT-COST.

           IF MORE-RECS IS NOT = 'NO'
               MOVE I-BOAT-TYPE TO H-BOAT-TYPE
               PERFORM 9300-CONVERT-BOAT-TYPE
               PERFORM 9400-PRINT-MAJOR-BOAT-LINES.

       9200-HEADINGS.
           ADD 1 TO C-PG-CTR.

           MOVE C-PG-CTR TO O-PG-CTR.
           
           WRITE PRTLINE
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.
      *    SKIP A LINE 
           WRITE PRTLINE
               FROM COL-HDG-1
                   AFTER ADVANCING 2 LINES.
          WRITE PRTLINE
               FROM COL-HDG-2
                   AFTER ADVANCING 1 LINES.
           WRITE PRTLINE
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.
      *   SKIP A LINE THEN PRINT OUT THE MAJOR BOAT TYPE.
           

       9300-CONVERT-BOAT-TYPE.
           EVALUATE I-BOAT-TYPE
               WHEN "B"
                   MOVE "BASS BOAT" TO O-BOAT-TYPE
                   MOVE "BASS BOAT" TO O-BOAT-TYPE1
               WHEN "P"
                   MOVE "PONTOON" TO O-BOAT-TYPE
                   MOVE "PONTOON" TO O-BOAT-TYPE1
               WHEN "S"
                   MOVE "SKI BOAT" TO O-BOAT-TYPE
                   MOVE "SKI BOAT" TO O-BOAT-TYPE1
               WHEN "J"
                   MOVE "JOHN BOAT" TO O-BOAT-TYPE
                   MOVE "JOHN BOAT" TO O-BOAT-TYPE1
               WHEN "C"
                   MOVE "CANOE" TO O-BOAT-TYPE
                   MOVE "CANOE" TO O-BOAT-TYPE1
               WHEN OTHER
                   MOVE "CABIN CRUISER" TO O-BOAT-TYPE
                   MOVE "CABIN CRUISER" TO O-BOAT-TYPE1
           END-EVALUATE.

       9400-PRINT-MAJOR-BOAT-LINES.
           WRITE PRTLINE
               FROM MAJOR-HEADINGS
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.
           

