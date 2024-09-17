       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. INSB610.                                                    
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
      *                                                                         
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PARMFILE ASSIGN TO PARMFILE
              FILE STATUS IS PARMFILE-STATUS.

           SELECT MISF999A ASSIGN TO MISF999A
              FILE STATUS IS MISF999A-STATUS.

           SELECT PRCF610B ASSIGN TO PRCF610B
              FILE STATUS IS PRCF610B0-STATUS.
         
       DATA DIVISION.
       FILE SECTION.
  
       FD  PARMFILE
           BLOCK CONTAINS 0 RECORDS.
       01  PARMFILE-RECORD                PIC X(80).

       FD  MISF999A
           BLOCK CONTAINS 0 RECORDS.
       01  MISF999A-RECORD                PIC X(80).

       FD  PRCF610B
           BLOCK CONTAINS 0 RECORDS.
       01  PRCF610B-RECORD                PIC X(68).
                                                                  
       WORKING-STORAGE SECTION.                                                 

       77  FILLER                         PIC X(36) VALUE
                   'INSB610 WORKING STORAGE BEGINS HERE'.

       01  WS-AREA.
           05  PARMFILE-STATUS            PIC XX  VALUE '00'.
               88  PARMFILE-VALID-STATUS          VALUE '00'.
           05  MISF999A-STATUS            PIC XX  VALUE '00'.
               88  MISF999A-VALID-STATUS          VALUE '00' '10'.
           05  PRCF610-STATUS             PIC XX  VALUE '00'.
               88 PRCF610B-VALID-STATUS           VALUE '00'.

       01  HOST-VARIABLES.

           COPY T119.
           COPY T2565.
           COPY T2567.
           COPY T3377.
           COPY T3422.
           COPY T3375.
           COPY T165.
           COPY ADV101.
           COPY ADV102.
           COPY ADV107.
           COPY SQLCA.

           EXEC SQL
              DECLARE T165A CURSOR FOR
               SELECT A.T164_LCT_GRP_NBR
                     ,B.MER_ARV_DT
                 FROM T165_LCT_CGY_GRP A
                     ,T063A_LCT               B
                 WHERE A.T224_LCT_CGY_CD  = 200
                   AND A.T158_LCT_GRP_CD  = 10
                   AND A.T162_CGY_SHT_NME = 'VALRET'
                   AND A.T162_CGY_ONR_ID  = 'CORPRAT'
                   AND A.T164_LCT_GRP_NBR = B.T063_LCT_NBR
                   AND B.LCT_STS_CD       = '1'
                   AND B.T1989_CRY_CD     =  1
                   AND B.OPN_BUS_IDC      = 'Y'
                   AND B.CTA_YRD_IDC      = 'N'
                   AND B.RTL_LCT_IDC      = 'Y'
                   AND B.T1276_LCT_SBO_CD =  0
                   AND B.ARA_NBR          >  0
                   AND B.RGN_NBR          >  0
                   AND (B.MER_ARV_DT -7 DAYS) < CURRENT DATE
                   AND (CURRRENT DATE < (B.CSE_DT - 2 DAYS)
                        OR B.CSE_DT IS NULL)
             ORDER BY A.T164_LCT_GRP_NBR
              WITH UR
           END-EXEC.

           EXEC SQL
              DECLARE T2567A CURSOR WITH HOLD FOR
               SELECT T2567.T024_ITM_NBR
                 FROM T2567_LBR_CGY_ITM T2567,
                      T2565_LBR_CGY_TYP T2565
                 WHERE T2565.T2565_LBR_CGY_CD NOT IN (0,54)
                   AND T2565.T2565_LBR_CGY_CD = T2567.T2565_LBR_CGY_CD
                   AND T2567.T024_ITM_NBR > :T2567-REC.T024-ITM-NBR
             ORDER BY T2567.T024_ITM_NBR
              WITH UR
           END-EXEC.


           EXEC SQL
              DECLAE T3422A CURSOR WITH HOLD FOR
               SELECT COALESCE (L.T024_ITM_NBR, R.T024_ITM_NBR)
                            AS T024_ITM_NBR,
                      COALESCE (L.T164_LCT_GRP_NBR, R.T164_LCT_GRP_NBR)
                            AS T164_LCT_GRP_NBR,
                      COALESCE (L.T162_CGY_ONR_ID, R.T162_CGY_ONR_ID0
                            AS T162_CGY_ONR_ID,
                      COALESCE (L.NEW_CST_AMT, R.NEW_CST_AMT)
                            AS NEW_CST_AMT
                      COALESCE (L.NEW_RTL_PRC_AMT, R.NEW_RTL_PRC_AMT)
                            AS NEW_RTL_PRC_AMT,
                      COALESCE (L.NEW_MRG_PCT, R.NEW_MRG_PCT)
                            AS NEW_PRG_PCT,
                      COALESCE (L.T3422_CNG_BGN_DT, R.T3422-CNG_BGN_DT)
                            AS T3422_CNG_BGN_DT
               FROM
                (SELECT T3422.T024_ITM_NBR,
                        T165.T164_LCT_GRP_NBR,
                        T165.T162_CGY_ONR_ID,
                        T3422.NEW_CST_AMT,
                        T3422.NEW_RTL_PRC-AMT,
                        T3422.NEW_MRG_PCT,
                        T3422.T3422_CNG_BGN_DT
                   FROM T3422_LBR_PRC_MNT         T3422,
                        T165_LCT_CGY_GRP          T165
                  WHERE T3422.T224_LCT_CGY_CD   = T165.T224_LCT_CGY_CD
                    AND T3422.T162_CGY_SHT_NME  = T165.T162_CGY_SHT_NME
                    AND T3422.T162_CGY_ONR_ID   = T165.T162_CGY_ONR_ID
                    AND T3422.T3422_CNG_BGN_DT <= :H-TOMORROW
                    AND T3422.CNG_END-DT       >= "H-TOMORROW
                    AND T165.T158_LC_GRP_CD     = 10
                    AND T165.T224_LCT_CGY_CD    = 300
                    AND T165.T162_CGY_ONR_ID    = 'LBRST') L
               FULL OUTER JOIN
                (SELECT T3422.T024_ITM_NBR,
                        T165.T164_LCT_GRP_NBR,
                        T165.T162_CGY_ONR_ID,
                        T3422.NEW_CST_AMT,
                        T3422.NEW_RTL_PRC_AMT,
                        T3422.NEW_MRG_PCT,
                        T3422.T3422_CNG_BGN_DT
                   FROM T3422_LBR_PRC_MNT         T3422,
                        T165_LCT_CGY_GRP          T165
                  WHERE T3422.T224_LCT_CGY_CD   = T165.T224_LCT_CGY_CD
                    AND T3422_CGY_SHT_NME       = T165.T162_CGY_SHT_NME
                    AND T3422.T162.CGY_ONR_ID   = T165.T162_CGY_ONR_ID
                    AND T3422.T3422_CNG_BGN_DT <= :H-TOMORROW
                    AND T3422.CNG_END_DT       >= :H-TOMORROW
                    AND T165.T158_LCT_GRP_CD    = 10
                    AND T165.T224_LCT_CGY_CD    = 300
                    AND T165.T162_CGY_ONR_ID    = 'LBRPZ') R
                ON L.T024_ITM_NBR     = R.T024_ITM_NBR
               AND L.T164_LCT_GRP_NBR = R.T164_LCT_GRP_NBR
               ORDER BY T024_ITM_NBR,
                        T164_LCT_GRP_NBR,
                        NEW_RTL_PRC_AMT
              WITH UR
           END-EXEC.

           EXEC SQL
              DECLARE T3377A CURSOR WITH HOLD FOR
               SELECT DISTINCT
                      T024_LBR_ITM_NBR,
                      LBR_ITM_CST_AMT,
                      LBR_ITM_MRG_PCT,
                      LBR_ITM_PRC_AMT,
                      T3377_PRC_BGN_DT
                 FROM T3377_CRP_LBR_PRC
                WHERE T3377_PRC_BGN_DT <= :H-TOMORROW
                  AND PRC_END_DT       >= :H-TOMORROW
             ORDER BY T024_LBR_ITM_NBR,
                      LBR_ITM_PRC_AMT
               WITH UR
           END-EXEC.

           EXEC SQL
              DECLARE ADV107A CURSOR WITH HOLD FOR
               SELECT ADV107.ITEM_NUMBER,
                      ADV102.LOCATION,
                      ADV102.DROP_DATE,
                      ADV101.GOODTHRU_DATE,
                      ADV107.SALE_PRICE
                 FROM ADV101 ADV101,
                      ADV102 ADV102,
                      ADV107 ADV107,
                      (SELECT T024_ITM_NBR
                         FROM T2567_LBR_CGY_ITM A,
                              T2565_LBR_CGY_TYP B
                        WHERE A.T2565_LBR_CGY_CD = B.T2565_LBR_CGY_CD
                          AND B.T2565_LBR_CGY_CD NOT IN (0,54)) T2567
                WHERE ADV101.EVENT_ID        = ADV102.EVEBT_ID
                  AND ADV102.EVENT_ID        = ADV107.EVENT_ID
                  AND ADV102.DROP_DATE      <= :H-TOMORROW
                  AND ADV101.GOODTHRU_DATE  >= :H-TOMORROW
                  AND ADV102.ADV_AREA_NUMBER = ADV107.ADV_AREA_NUMBER
                  AND ADV107.ITEM_NUMBER     = T2567.T024_ITM_NBR
             ORDER BY ADV107.ITEM_NUMBER,
                      ADV102.LOCATION,
                      ADV107.SALE_PRICE
              WITH UR
           END-EXEC.


       PROCEDURE DIVISION.
       
       0000-MAINLINE.

           PERFORM 0100-INITIALIZATION
           PERFORM 0200-MAIN-PROCESS
           PERFORM 0900-CLOSE-FILES

           IF REFRESH-ALL
              MOVE LCT-COUNT  TO STR-COUNT
           END-IF

           GO BACK.
         

       0100-INITIALIZATION.
           MOVE ZERO  TO PREV-ITM-NBR.
           OPEN INPUT PARMFILE.
           IF (NOT PARMFILE-VALID-STATUS)
              DISPLAY 'PARMFILE OPEN ERROR'
              MOVE 'N'   TO WS-PARMFILE-SW
           END-IF.

           OPEN INPUT MISF999A.
           IF NOT MISF999A-VALID-STATUS
              MOVE 'MISF999A'      TO WS-FLE-NME
              MOVE MISF999A-STATUS TO WS-FLE-ST-CD
              MOVE 'OPEN-IN'       TO WS-FLE-FNC
              CALL 'JOBLOG3'    USING WS-FLE-ERROR
              PERFORM 0999-ABORT
           END-IF.

           READ MISF999A INTO MISF999A-REC
             AT END MOVE 'Y'        TO DOING-ALL-STORE-SW
                    DISPLAY '*COMLINK: *FILE IS EMPTY/NULL'
                            ' --PROCESSING ALL STORES'
           END-READ.

           IF NOT MISF999A-VALID-STATUS
              MOVE 'MISF999A'       TO WS-FLE-NME
              MOVE MISF999A-STATUS  TO WS-FLE-ST-CD
              MOVE 'READ-IN'        TO WS-FLE-FNC
              CALL 'JOBLOG3'       USING WS-FLE-ERROR
              PERFORM 0999-ABORT
           END-IF.

           OPEN OUTPUT PRCF610B
           IF (NOT PRCF610B-VALID-STATUS)
              MOVE 'PRCF610B'       TO WS-FLE-NME
              MOVE PRCF610B-STATUS  TO WS-FLE-ST-CD
              MOVE 'OPEN-OUT'       TO WS-FLE-FNC
              CALL 'JOBLOG3'       USING WS-FLE-ERROR
              PERFORM 0999-ABORT
           END-IF.

           CALL 'GETJOB3'  USING JOB-USE-ID.

           PERFORM 0130-CHECK-RESTART

           IF PARMFILE-OK
              PERFORM 0110-LOAD-PARM-LCT
           END-IF

           DISPLAY 'CHECKPOINT COMMIT RATE :' COMMIT-POINT.

           EXEC SQL
             SET :H-TODAY   = CURRENT_DATE
           END-EXEC.

           EXEC SQL
             SET :H-TOMORROW = CURRENT_DATE + 1 DAY
           END-EXEC.

           IF H-PRX-EFF-DATE = SPACE OR 'CCYY-MM-DD'
              NEXT SENTENCE
           ELSE
              MOVE H-PRC-EFF-DTE  TO H-TOMORROW
              EXEC SQL
                SET :H-TODAY =
                     DATE(:H-TOMORROW) - 1 DAY
              END-EXEC
           END-IF.

           MOVE H-TOMORROW        TO WS-TOMORROW
           MOVE WS-TOMORROW-MM    TO WS-MM-TOMORROW
           MOVE WS-TOMORROW-DD    TO WS-DD-TOMORROW
           MOVE WS-TOMORROW-YY    TO WS-YY-TOMORROW
 
           IF H-AUTOREFRESH-FLG = 'Y'
              EXEC SQL
               SET :H-AUTOREFRESH-DTE =
                    DATE(:H-TOMORROW) + :H-AUTOREFRESH-NBR DAYS
              END-EXEC
              DISPLAY '**AUTO-REFRESH FLAG IS : ON'
              DISPLAY '**AUTO-REFRESH DATE IS : ' H-AUTOREFRESH-DTE
           ELSE
              DISPLAY '**AUTO-REFRESH FLAG IS : OFF'
           END-IF.

           IF PARMFILE-NF
              DISPLAY 'LOADING ACTIVE STORE LOCATIONS...'
              PERFORM 0120-LOAD-T165-LCT
           END-IF.

           INITIALIZE T3375-REC, T3377-REC, T3422-REC,
                     ADV101-REC, ADV102-REC, ADV107-REC.

       0110-LOAD-PARM-LCT.
           
           MOVE ZERO   TO LCT-IDX
           PERFORM 0800-PROCESS-PARMFILE
                   UNTIL PARMFILE-EOF OR PARMFILE-NF
                   OR  LCT-IDX = LCT-MAX
                   OR  STR-IDX = STR-MAX
           MOVE LCT-IDX  TO LCT-END, LCT-COUNT
           MOVE STR-IDX  TO STR-END, STR-COUNT
   
           IF LCT-END = ZERO
              MOVE 'N' TO WS-PARMFILE-SW
           END-IF

           MOVE 9999  TO LCT-NBR(LCT-END + 1)
                         STR-NBR(STR-END + 1).

       0120-LOAD-T165-LCT.

           MOVE ZERO TO LCT-IDX
           MOVE 'N'  TO T165A-EOF-SW

           PERFORM 0510-OPEN-T165A
           PERFORM 0520-FETCH-T165A

           PERFORM UNTIL (T165A-EOF OR LCT-IDX > LCT-MAX)
              ADD 1 TO LCT-IDX
              MOVE T164-LCT-GRP-NBR OF T165-REC TO LCT-NBR(LCT-IDX)
                                       T063-LCT-NBR OF T3375-REC

              PERFORM 0700-CHECK-NEW-LCT
              MOVE NEW-LCT-IDC    TO LCT-NEW(LCT-IDX)
              IF NEW-LCT
                 DISPLAY '*--FOUND NEW STORE --> ' LCT-NBR(LCT-IDX)
              END-IF
              IF H-AUTOREFRESH-FLG = 'Y' AND
                 H-AUTOREFRESH-DTE = H-MER-ARV-DT
                 MOVE 'Y'      TO LCT-REF(LCT-IDX)
                 ADD 1         TO AUTO-CONNECT
                 DISPLAY '*-REFESH NEW STORE --> ' LCT-NBR(LCT-IDX)
              ELSE
                 MOVE 'N'      TO LCT-REF(LCT-IDX)
              END-IF

              PERFORM 0520-FETCH-T165A
           END-PERFORM

           MOVE LCT-IDX  TO LCT-END
           MOVE 9999 TO LCT-NBR(LCT-IDX + 1)

           PERFORM 0920-CLOSE-T165A.

       0130-CHECK-RESTART.
                  
           EXEC SQL
             SELECT CP_ARA_TXT,
                    CMI_FRQ_QTY,
                    CMI_QTY,
                    CP_DM
                 INTO :CP-ARA,
                      :CMI-FRQ-QTY,
                      :CMI-QTY,
                      :CP-DM
               FROM T119_CP_RSA_CTL
              WHERE T119_PGM_ID = :PGM-ID
                AND T119_USE_ID = :JOB-USE-ID
               WITH UR
           END-EXEC.

           IF SQL-OK
              IF CMI-QTY OF T119-REC > 0
                 MOVE 'Y'        TO PGM-RESTART-IDC
                 MOVE CP-ITEM    TO RSA-ITM-NBR
                 DISPLAY '**RESTART REQUIRED - STARTING AT '
                         'LAST CHECKPOINT - ITEM# ' CP-ITEM
                 MOVE CP-ITEM-COUNT TO ITEM-COUNT
                 MOVE CP-LCT-COUNT  TO LCT-COUNT
                 MOVE CP-STR-COUNT  TO STR-COUNT
                 MOVE CP-AUTO-COUNT TO AUTO-COUNT
                 MOVE CP-IREF-COUNT TO IREF-COUNT
                 MOVE CP-IMAT-COUNT TO IMAT-COUNT
                 MOVE CP-NEW-COUNT  TO NEW-COUNT
                 MOVE CP-NEW-ITEMS  TO NEW-ITEMS
                 MOVE CP-PROMO-COUNT TO PROMO-COUNT
                 MOVE CP-LBRST-COUNT TO LBRST-COUNT
                 MOVE CP-LBRPZ-COUNT TO LBRPZ-COUNT
                 MOVE CP-CORP-COUNT TO CORP-COUNT
                 MOVE CP-PROMO-PC   TO PROMO-PC
                 MOVE CP-LBRST-PC   TO LBRST-PC
                 MOVE CP-LBRPZ-PC   TO LBRPZ-PC
                 MOVE CP-CORP-PC    TO CORP-PC
                 MOVE CP-PRCF610B-COUNT TO PRCF610B-COUNT
                 MOVE CP-T3375-INS-COUNT TO T3375-INS-COUNT
                 MOVE CP-T3375-INS-803   TO T3375-INS-803
              END-IF
           ELSE
              IF SQL-NOT-FND
                 PERFORM 0140-INSERT-T119
              ELSE
                 MOVE 'SELT119-RSA' TO WS-EXE-IDF
                 MOVE SQLCODE TO WS-SQL-CODE
                 MOVE '** PROGRAM ABENDED ** SQL SELECT FAILED !!!'
                   TO WS-ABORT-MESSAGE
                 CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
                 CALL 'JOBLOG3'  USING WS-SQL-ERROR
                 PERFORM 0999-ABORT
              END-IF
           END-IF.


       0140-INSERT-T119.
           
           MOVE ZERO TO CMI-QTY OF T119-REC
           MOVE CP-ARA-INT TO CP-ARA-TXT OF T119-REC

           EXEC SQL
             INSERT INTO T119_CP_RSA_CTL
               (T119_PGM_ID, T119_USE_ID, CMI_FRQ_QTY, CMI_QTY,
                CP_DM, CP_ARA_TXT, ADD_DT, UPD_DM, UPD_ID,
                FIL_FLD1_TXT, FIL_FLD2_TXT, FIL_FLD3_TXT,
                FIL_FLD4_TXT, FIL_FLD5_TXT, FIL_FLD6_TXT, FIL_FLD7_TXT)
             VALUES
               (:PGM-ID,
                :JOB-USE-ID,
                :COMMIT-POINT,
                :CMI-QTY,
                CURRENT TIMESTAMP,
                :CP-ARA-TXT,
                CURRENT DATE,
                CURRENT TIMESTAMP,
                :PGM-ID,
                ' ',' ',' ',' ',' ',' ',' ')
           END-EXEC.

           IF SQL-OK
              CONTINUE
           ELSE
              MOVE 'INST119-RSA' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL SELECT FAILED !!!'
                TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0150-UPDATE-T119.

           EXEC SQL
             UPDATE T119_CP_RSA_CTL
                SET CP_ARA_TXT = :CP-ARA,
                    CMI_FRQ_QTY = :COMMIT-POINT,
                    CMI_QTY = :CMI-QTY,
                    CP_DM = CURRENT TIMESTAMP
               WHERE T119_PGM_ID = :PGM-ID
                 AND T119_USE_ID = :JOB-USE-ID
           END-EXEC.

           IF SQL-OK
              CONTINUE
           ELSE
              MOVE 'UPDT110-RSA' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL SELECT FAILED !!!'
                 TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0200-MAIN-PROCESS.

           IF PGM-RESTART
              MOVE CP-ITEM TO T024-ITM-NBR OF T2567-REC
           ELSE
              MOVE ZERO TO T024-ITM-NBR OF T2567-REC
           END-IF
           PERFORM 0530-OPEN-T2567A
           PERFORM 0540-FETCH-T2567A
           PERFORM 0550-OPEN-T3422A
           PERFORM 0560-OPEN-T3377A
           PERFORM 0580-OPEN-ADV107A
           PERFORM 0210-PROCESS-ALL-ITEMS
           PERFOMR 0930-CLOSE-T2567A
           PERFORM 0940-CLOSE-T3422A
           PERFORM 0950-CLOSE-T3377A
           PERFORM 0960-CLOSE-ADV107A

           MOVE ZERO TO CMI-QTY OF T119-REC
           MOVE CP-ARA-INIT TO CP-ARA
           PERFORM 0150-UPDATE-T119
           EXED SQL COMMIT END-EXEC
           .

       0210-PROCESS-ALL-ITEMS.

           PERFORM UNTIL (T2567A-EOF)
              ADD 1 TO ITEM-COUNT
              MOVE 'N' TO NEW-ITM-IDC

              PERFORM 0710-CHECK-NEW-ITEM
              MOVE 1 TO LCT-IDX
              PERFORM UNTIL (LCT-IDX > LCT-END)
                 MOVE LCT-NBR(LCT-IDX) TO T164-LCT-GRP-NBR OF T165-REC
                 MOVE LCT-NEW(LCT-IDX) TO NEW-LCT-IDC
                 MOVE LCT-REF(LCT-IDX) TO REF-LCT-IDC

                 IF STR-END > 0
                    PERFORM 0720-CHECK-STORE-UPD
                 END-IF

                 IF REFRESH-ALL
                    MOVE 'Y' TO STR-UPD-IDC
                 END-IF

                 INITIALIZE T3375-REC
                 MOVE '0' TO ITM-CHANGE-TYP
                 MOVE 'N' TO ADV107A-FETCH-SW
                 PERFORM 0581-FETCH-MARKDOWN
                    UNTIL (ADV107A-FETCHED OR ADV107A-EOF)

                 MOVE 'N' TO T3422A-FETCH-SW
                 PERFORM 0551-FETCH-T3422A
                    UNTIL (T3422A-FETCHED OR T3422A-EOF)
                 IF T3422-FND
                    PERFORM 0555-MOVE-T3422-DATA
                 ELSE
                    MOVE 'N' TO T3377A-FETCH-SW
                    PERFORM 0561-FETCH-T3377A
                       UNTIL (T3377A-FETCHED OR T3377A-EOF)
                    IF T3377-FND
                       PERFORM 0565-MOVE-T3377-DATA
                    END-IF
                 END-IF

                 IF ITEM-MARKDOWN
                    IF (T3422-FND OR T3377-FND)
                       IF H-PRM-RTL-PRC < RTL-PRC-AMT
                          PERFORM 0585-MOVE-TMRKDN-DATA
                       END-IF
                    ELSE
                       PERFORM 0585-MOVE-TMRKDN-DATA
                    END-IF
                 END-IF

                 IF MARKDOWN-PRICE
                    ADD 1 TO PROMO-PC
                 END-IF
                 IF LBRST-PRICE
                    ADD 1 TO LBRST-COUNT
                 END-IF
                 IF LBRPZ-PRICE
                    ADD 1 TO LBRPZ-COUNT
                 END-IF
                 IF CORP-PRICE AND DIFF-CORP-ITM
                    ADD 1 TO CORP-COUNT
                 END-IF

                 MOVE 'N' TO REF-ITM-IDC
                 IF MARKDOWN-PRICE
                    MOVE ZERO TO H-PME-DT-IND
                    PERFORM 0600-INSERT-T3375
                 ELSE
                    IF LBRST-PRICE OR LBRPZ-PRICE OR CORP-PRICE
                       MOVE -1 TO H-PME-DT-IND
                       IF H-AUTOREFRESH-FLG = 'Y'
                          PERFORM 0730-CK-4-PRC-CHG
                       END-IF
                       PERFORM 0600-INSERT-T3375

                       IF ((LBRST-PRICE OR LBRPZ-PRICE) AND
                           (H-CNG-BGN-DT = H-TOMORROW OR
                            REF-LCT OR REF-ITM OR
                            NEW-LCT OR NEW-ITM OR STR-UPD))
                           PERFORM 0850-FORMAT-PRCF610B
                       ELSE
                          IF (CORP-PRICE AND
                              (H-PRC-BGN-DT = H-TOMORROW OR
                               REF-LCT OR REF-ITM OR
                               NEW-LCT OR NEW-ITM OR STR-UPD))
                             PERFORM 0850-FORMAT-PRCF610B
                          END-IF
                       END-IF
                    END-IF
                END-IF
                ADD 1 TO LCT-IDX
              END-PERFORM

              PERFORM 0220-CHECKPOINT
              PERFORM 0540-FETCH-T2567A
           END-PERFORM.

       0220-CHECKPOINT.

           ADD 1 TO ITEMS-PROCESSED
           IF ITEMS-PROCESSED = COMMIT-POINT

              MOVE T024-ITM-NBR OF T2567-REC TO CP-ITEM
              COMPUTE CMI-QTY = CMI-QTY + 1


               MOVE ITEM-COUNT   TO CP-ITEM-COUNT
               MOVE LCT-COUNT    TO CP-LCT-COUNT 
               MOVE STR-COUNT    TO CP-STR-COUNT
               MOVE AUTO-COUNT   TO CP-AUTO-COUNT
               MOVE IREF-COUNT   TO CP-IREF-COUNT
               MOVE IMAT-COUNT   TO CP-IMAT-COUNT
               MOVE NEW-COUNT    TO CP-NEW-COUNT
               MOVE NEW-ITEMS    TO CP-NEW-ITEMS
               MOVE PROMO-COUNT  TO CP-PROMO-COUNT
               MOVE LBRST-COUNT  TO CP-LBRST-COUNT
               MOVE LBRPZ-COUNT  TO CP-LBRPZ-COUNT
               MOVE CORP-COUNT   TO CP-CORP-COUNT
               MOVE PROMO-PC     TO CP-PROMO-PC
               MOVE LBRST-PC     TO CP-LBRST-PC
               MOVE LBRPZ-PC     TO CP-LBRPZ-PC
               MOVE COPR-PC      TO CP-CORP-PC
               MOVE PRCF610B-COUNT TO CP-PRCF610B-COUNT
               MOVE T3375-INS-COUNT TO CP-T3375-INS-COUNT
               MOVE T3375-INS-803 TO CP-T3375-INS-803
               PERFORM 0150-UPDATE-T119

               EXEC SQL COMMIT END-EXEC
               MOVE ZERO TO ITEMS-PROCESSED
           END-IF.

       0510-OPEN-T165A.

           EXEC SQL
              OPEN T165A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'OPNT165A' TO WS-EXE-IDF
              MOVE SQLCODE    TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL OPEN FAILED T165A'
                              TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0520-FETCH-T165A.

           EXEC SQL
              FETCH T165A
              INTO :T164-LCT-GRP-NBR
                  ,:H-MER-ARV-DT
           END-EXEC.

           IF SQL-OK
              PERFORM 0845-READ-MISF999A
              IF MIS-STORE-NF
                 GO TO 0520-FETCH-T165A
              END-IF
              IF COUNT-LCT
                 ADD 1 TO LCT-COUNT
              END-IF
           ELSE
              IF SQL-EOF
                 MOVE 'Y' TO T165A-EOF-SW
              ELSE
                 MOVE 'FET165A' TO WS-EXE-IDF
                 MOVE SQLCODE TO WS-SQL-CODE
                 MOVE '** PROGRAM ABENDED ** SQL FETCH FAILED T165A'
                         TO WS-ABORT-MESSAGE
                 CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
                 CALL 'JOBLOG3' USING WS-SQL-ERROR
                 PERFORM 0999-ABORT
              END-IF
           END-IF.

       0530-OPEN-T2567A.
           EXEC SQL
              OPEN T2567A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'OPNT2567A' TO WS-EXE-IDF
              MOVE SQLCODE     TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL OPEN FAILED T2567A'
                               TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT.

       0540-FETCH-T2567A.
           EXEC SQL
              FETCH T2567A
              INTO :T2567-REC.T024-ITM-NBR
           END-EXEC.

           IF SQL-OK
              NEXT SENTENCE
           ELSE
              IF SQL-EOF
                 MOVE 'Y' TO T2567A-EOF-SW
              ELSE
                 MOVE 'FET2567A' TO WS-EXE-IDF
                 MOVE SQLCODE TOWS-SQL-CODE
                 MOVE '** PROGRAM ABENDED ** SQL FETCH FAILED T2567A'
                          TO WS-ABORT-MESSAGE
                 CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
                 CALL 'JOBLOG3' USING WS-SQL-ERROR
                 PERFORM 0999-ABORT
              END-IF
           END-IF.


       0550-OPEN-T3422A.

           EXEC SQL
              OPEN T3422A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'OPNT3422A' TO WS-EXE-IDF
              MOVE SQLCODE     TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL OPEN FAILED T3422A'
                               TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT.

       0551-FETCH-T3422A.

           MOVE 'N' TO T3422-FND-SW.
           MOVE WS-INIT-DATE TO H-CNG-BGN-DT.
           IF NOT T3422A-EOF
              IF H-T024-ITM-NBR       < T024-ITM-NBR OF T2567-REC
               OR (H-T024-ITM-NBR     = T024-ITM-NBR OF T2567-REC AND
                   H-T164-LCT-GRP-NBR < T164-LCT-GRP-NBR OF T165-REC)

                   EXEC SQL
                      FETCH T3422A
                       INTO :H-T024-ITM-NBR,
                            :H-T164-LCT-GRP-NBR,
                            :H-T162-CGY-ONR-ID,
                            :NEW-CST-AMT,
                            :NEW-RTL-PRC-AMT,
                            :NEW-MRG-PCT,
                            :T3422-CNG-BGN-DT
                   END-EXEC

                   IF SQL-OK
                      NEXT SENTENCE
                   ELSE
                      IF SQL-EOF
                         MOVE 'Y' TO T3422A-EOF-SW
                      ELSE
                         MOVE 'FET3422A' TO WS-EXE-IDF
                         MOVE SQLCODE    TO WS-SQL-CODE
                         MOVE
                        '** PROGRAM ABENDED ** SQL FETCH FAILED T3422A'
                                      TO WS-ABORT-MESSAGE
                         CALL 'SQLMSGB3' USING
                                         WS-PROG-ID, WS-EXE-IDF, SQLCA
                         CALL 'JOBLOG3' USING WS-SQL-ERROR
                         PERFORM 0999-ABORT
                      END-IF
                   END-IF
              ELSE
                 MOVE 'Y' TO T3422A-FETCH-SW
                 IF H-T024-ITM-NBR   = T024-ITM-NBR OF T2567-REC AND
                    H-T164-LCT-GRP-NBR = T164-LCT-GRP-NBR 0F T165-REC
                    MOVE 'Y' TO T3422-FND-SW
                    MOVE T3422-CNG-BGN-DT TO H-CNG-BGN-DT
                    IF H-T162-CGY-ONR-ID = 'LBRST'
                       MOVE '2' TO ITM-CHANGE-TYP
                    END-IF
                    IF H-T162-CGY-ONR-ID = 'LBRPZ'
                       MOVE '3' TO ITM-CHANGE-TYP
                    END-IF
                 END-IF
              END-IF
           END-IF.

       0555-MOVE-T3422-DATA.

           MOVE T024-ITM-NBR OF T2567-REC OF TO T024-ITM-NBR OF T3375-REC
           MOVE NEW-CST-AMT TO CST-AMT
           MOVE NEW-MRG-PCT TO MRG-PCT
           IF MRG-PCT = ZERO
              MOVE MIN-MRG-PCT TO MRG-PCT
           END-IF.

       0560-OPEN-T3377A.

           EXEC SQL
              OPEN T3377A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'OPNT3377A' TO WS-EXE-IDF
              MOVE  SQLCODE    TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL OPEN FAILED T3377A'
                               TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3'  USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3'   USING WS-SQL-ERROR
              PERFORM 0999-ABORT.

       0561-FETCH-T3377A.

           MOVE 'N' TO T3377-FND-SW.
           MOVE WS-INIT-DATE TO H-PRC-BGN-DT.

           IF NOT T3377A-EOF
              IF T024-LBR-ITM-NBR < T024-ITM-NBR OF T2567-REC
                  EXEC SQL
                     FETCH T3377A
                      INTO :T024-LBR-ITM-NBR,
                           :LBR-ITM-CST-AMT,
                           :LBR-ITM-MRG-PCT,
                           :LBR-ITM-PRC-AMT,
                           :T3377-PRC-BGN-DT
                  END-EXEC

                  IF SQL-OK
                     NEXT SENETENCE
                  ELSE
                     IF SQL-EOF
                        MOVE 'Y' TO T3377A-EOF-SW
                     ELSE
                        MOVE 'FET3377A' TO WS-EXE-IDF
                        MOVE SQLCODE  TO WS-SQL-CODE
                        MOVE
                       '** PROGRAM ABENDED ** SQL FETCH FAILED T3377A'
                                      TO WS-ABORT-MESSAGE
                        CALL 'SQLMSGB3' USING
                                        WS-PROG-ID, WS-EXE-IDF, SQLCA
                        CALL 'JOBLOG3'  USING WS-SQL-ERROR
                        PERFORM 0999-ABORT
                     END-IF
                  END-IF
              ELSE
                 MOVE 'Y' TO T3377A-FETCH-SW
                 IF T024-LBR-ITM-NBR = T024-ITM-NBR OF T2567-REC
                    MOVE 'Y' TO T3377-FND-SW
                    MOVE '4' TO ITM-CHANGE-TYP
                    MOVE T3377-PRC-BGN-DT TO H-PRC-BGN-DT
                    IF T024-LBR-ITM-NBR = H-T024-LBR-ITM-NBR
                       MOVE 'N' TO DIFF-CORP-ITM-SW
                    ELSE
                       MOVE 'Y' TO DIFF-CORP-ITM-SW
                       MOVE T024-LBR-ITM-NBR TO H-T024-LBR-ITM-NBR
                    END-IF
                   END-IF
                  END-IF
                 END-IF.

       0565-MOVE-T3377-DATA.

           MOVE T024-ITM-NBR OF T2567-REC TO T024-ITM-NBR OF T3375-REC
           MOVE LBR-ITM-CST-AMT  TO CST-AMT
           MOVE LBR-ITM-PRC-AMT  TO RTL-PRC-AMT
           MOVE LBR-ITM-MRG-PCT  TO MRG-PCT.
           IF MRG-PCT = ZERO
              MOVE MIN-MRG-PCT TO MRG-PCT
           END-IF.

       0580-OPEN-ADV107A.

           EXEC SQL
              OPEN ADV107A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'OPN107A' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL OPEN FAILED ADV107A'
                           TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT.

       0581-FETCH-MARKDOWN.

           MOVE 'N' TO ITEM-MARKDOWN-SW.
           IF NOT ADV107A-EOF
              IF ITEM-NUMBER < T024-ITM-NBR OF T2567-RC
              OR (ITEM-NUMBER = T024-ITM-NBR OF T2567-REC AND
                  LOCATION < T164-LCT-GRP-NBR OF T165-REC)

                  EXEC SQL
                     FETCH ADV107A
                      INTO :ADV107-REC.ITEM-NUMBER,
                           :ADV102-REC.LOCATION,
                           :ADV102-REC.DROP-DATE,
                           :ADV101-REC.GOODTHRU-DATE,
                           :ADV107-REC.SALE-PRICE
                  END-EXEC

                  IF SQL-OK
                     NEXT SENTENCE
                  ELSE
                     IF SQL-EOF
                        MOVE 'Y' TO ADV107A-EOF-SW
                     ELSE
                        MOVE 'FET107A' TO WS-EXE-IDF
                        MOVE SQLCODE TO WS-SQL-CODE
                        MOVE
                       '** PROGRAM ABENDED ** SQL FETCH FAILED ADV107A'
                                     TO WS-ABORT-MESSAGE
                        CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
                        CALL 'JOBLOG3' USING WS-SQL-ERROR
                        PERFORM 0999-ABORT
                      END-IF
                   END-IF
              ELSE
                 MOVE 'Y' YO ADV107A-FETCH-SW
                 IF ITEM-NUMBER = T024-ITM-NBR  OF T2567-REC AND
                    LOCATION = T164-LCT-GRP-NBR OF T165-REC
                    MOVE 'Y' TO ITEM-MARKDOWN-SW
                    MOVE SALE-PRICE TO H-PRM-RTL-PRC
                 END-IF
              END-IF
           END-IF.


       0585-MOVE-TMRKDN-DATA.

           MOVE T024-ITM-NBR OF T2567-REC TO T024-ITM-NBR OF T3375-REC
           MOVE H-PRM-RTL-PRC TO RTL-PRC-AMT
           IF (RTL-PRC-AMT NOT = ZERO)
              COMPUTE MRG-PCT =
                 ((RTL-PRC-AMT - CST-AMT) * 100) /RTL-PRC-AMT
           ELSE
              MOVE ZERO TO MRG-PCT
           END-IF

           IF MRG-PCT = ZERO
               MOVE MIN-MRG-PCT TO MRG-PCT
           END-IF

           MOVE DROP-DATE OF ADV102-REC TO PME-BGN-EFC-DT
           MOVE GOODTHRU-DATE OF ADV101-REC TO PME-END-EFC-DT.
           MOVE '1' TO ITM-CHANGE-TYP
           IF ITEM-NUMBER NOT = PREV-ITM-NBR
              MOVE ITEM-NUMBER TO PREV-ITM-NBR
              ADD 1 TO PROMO-COUNT
           END-IF.

       0600-INSERT-T3375.
           IF MARKDOWN-PRICE
              MOVE 'INSB610M' TO H-USER-PC
           ELSE IF LBRST-PRICE
              MOVE 'INSB610S' TO H-USER-PC
           ELSE IF LBRPZ-PRICE
              MOVE 'INSB610P' TO H-USER-PC
           ELSE
              MOVE 'INSB610C' TO H-USER-PC
           END-IF.

           EXEC SQL
              INSERT INTO T3375_LBR_PRC_HST
                     (T024_ITM_NBR, T053_LCT_NBR,
                      T3375_PRC_DT, CST-AMT,
                      MRG_PCT,      RTL_PRC_AMT,
                      PME_BGN_EFC_DT,  PME_END_EFC_DT,
                      ADD_DT, UPD_DM, UPD_ID)
              VALUES (:T3375-REC.T024-ITM-NBR,
                      :T165-REC.T164-LCT-GRP-NBR,
                      :H-TOMORROW,
                      :CST-AMT,
                      :MRG-PCT,
                      :RTL-PRC-AMT,
                      :PME-BGN-EFC-DT :H-PME-DT-IND,
                      :PME-END-EFC-DT :H-PME-DT-IND,
                       CURRENT_DATE,
                       CURRENT_TIMESTAMP,
                      :H-USER-PC )
           END-EXEC.
            
           IF SQL-OK
              ADD 1 TO T3375-INS-COUNT
           ELSE
              IF SQLCODE = -803
                 DISPLAY 'DUPE KEY. ITM=' T024-ITM-NBR OF T3375-REC,
                     'LCT=' T164-LCT-GRP-NBR OF T165-REC
                 COMPUTE
                    T3375-INS-803 = T3375-INS-803 + 1
                 END-COMPUTE
              ELSE
                 MOVE 'INS3375' TO WS-EXE-IDF
                 MOVE SQLCODE TO WS-SQL-CODE
                 MOVE '** PROGRAM ABENDED ** SQL INSERT FAILED !!!'
                   TO WS-ABORT-MESSAGE
                 CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
                 CALL 'JOBLOG3' USING WS-SQL-ERROR
                 PERFORM 0999-ABORT
              END-IF
           END-IF.

       0700-CHECK-NEW-LCT.
           EXEC SQL
               SELECT 1 
               INTO :H-LCT-IDC
               FROM T3375_LBR_PRC_HST
               WHERE T063_LCT_NBR = :T3375-REC.T063-LCT-NBR
               FETCH FIRST ROW ONLY
            END-EXEC.
 
            IF SQL-OK
               MOVE 'N' TO NEW-LCT-IDC
            ELSE IF SQL-NOT-FND
               ADD 1 TO NEW-COUNT
               MOVE 'Y' TO NEW-LCT-IDC
            ELSE
               MOVE 'SEL3375-LCT' TO WS-EXE-IDF
               MOVE SQLCODE TO WS-SQL-CODE
               MOVE '** PROGRAM ABENDED ** SQL SELECT FAILED !!!'
                    TO WS-ABORT-MESSAGE
               CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
               CALL 'JOBLOG3' USING WS-SQL-ERROR
               PERFORM 0999-ABORT
           END-IF.

       0710-CHECK-NEW-ITEM.

           EXEC SQL
              SELECT 'Y'
              INTO :H-EXISTS
              FROM T3375_LBR_PRC-HST
              WHERE T024_ITM_NBR = :T2567-REC.T024-ITM-NBR
              FETCH FIRST ROW ONLY
              WITH UR
           END-EXEC.

           IF SQL-OK
              MOVE 'N' TO NEW-ITM-COUNT-IDC
           ELSE IF SQL-NOT-FND
              MOVE 'Y' TO NEW-ITM-IDC NEW-ITM-COUNT-IDC
           ELSE
              MOVE 'SEL3375-ITM' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABENDED ** SQL SELECT FAILED !!!'
                   TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0720-CHECK-STORE-UPD.

           MOVE 1 TO STR-IDX
           MOVE 'N' TO STR-UPD-IDC
           PERFORM UNTIL (STR-IDX > STR-END OR STR-UPD)
              IF LCT-NBR(LCT-IDX) = STR-NBR(STR-IDX)
                 MOVE 'Y' TO STR-UPD-IDC
              END-IF

              ADD 1 TO STR-IDX
           END-PERFORM.

       0730-CK-4-PRC-CHG.

          IF ((LBRST-PRICE OR LBRPZ-PRICE) AND
              (H-CNG-BGN-DT = H-TOMORROW OR
               REF-LCT OR
               NEW-LCT OR NEW-ITM OR STR-UPD))
               CONTINUE
           ELSE
              IF (CORP-PRICE AND
                  (H-PRC-BGN-DT = H-TOMORROW OR
                   REF-LCT OR
                   NEW-LCT OR NEW-ITM OR STR-UPD))
                   CONTINUE
              ELSE
                 PERFORM 0740-IS-PRICE-SAME
              END-IF
           END-IF.
       
       0740-IS-PRICE-SAME.

           EXEC SQL
             SELECT RTL_PRC_AMT
               INTO :CUR-RTL-PRC-AMT
               FROM T3375_LBR_PRC_HST
              WHERE T063_LCT_NBR = :T165-REC.T164-LCT-GRP-NBR
                AND T024_ITM_NBR = :T3375-REC.T024-ITM-NBR
                AND T3375_PRC_DT = :H-TODAY
               WITH UR
           END-EXEC

           IF SQL-OK
              IF RTL-PRC-AMT = CUR-RTL-PRC-AMT
                 ADD 1  TO IMAT-COUNT
              ELSE
                 ADD 1  TO IREF-COUNT
                 MOVE 'Y' TO REF-ITM-IDC
              END-IF
           ELSE
              IF SQL-NOT-FND
                 ADD 1  TO IREF-COUNT
                 MOVE 'Y' TO REF-ITM-IDC
                 DISPLAT '**PREVIOUS PRICE MISSING FOR STORE/ITEM'
                         ' ' T164-LCT-GRP-NBR OF T165-REC
                         ' ' T024-ITM-NBR     OF T3375-REC
              ELSE
                 MOVE '0740 PARAGRAPH' TO WS-EX4-IDF
                 MOVE SQLCODE TO WS-SQL-CODE
                 MOVE '** PROGRAM ABENDD ** SQL SELECT FAILED !!!'
                      TO WS-ABORT-MESSAGE
                 CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
                 CALL 'JOBLOG3' USING WS-SQL-ERROR
                 PERFORM 0999-ABORT
              END-IF
           END-IF.

        
       0800-PROCESS-PARMFILE.
           PERFORM 0840-READ-PARMFILE
           IF PARMFILE-OK
              IF PR-ACTION = PA-PRICECHANGE
                 MOVE 'P' TO WS-ACTION-SW
                 PERFORM 0840-READ-PARMFILE
              END-IF
              IF PR-ACTION = PA-STOREUPDATE
                 MOVE 'S' TO WS-ACTION-SW
                 PERFORM 0840-READ-PARMFILE
              END-IF
              IF PR-ACTION = PA-COMMITRATE
                 MOVE 'C' TO WS-ACTION-SW
                 PERFORM 0840-READ-PARMFILE
              END-IF
              IF PR-ACTION = PA-PRC-EFF-DTE
                 MOVE 'E' TO WS-ACTION-SW
                 PERFORM 0840-READ-PARMFILE
              END-IF
              IF PR-ACTION = PA-AUTOREFRESH
                 MOVE 'A' TO WS-ACTION-SW
                 PERFORM 0840-READ-PARMFILE
              END-IF
 
              IF PRICECHANGE-FND AND NOT PARMFILE-EOF
                 PERFORM 0810-LOAD-LCTS
              END-IF
              IF STOREUPDATE-FND AND NOT PARMFILE-EOF
                 PERFORM 0820-LOAD-STRS
              END-IF
              IF COMMITRATE-FND AND NOT PARMFILE-EOF
                 MOVE PARM-LCT(1) TO COMMIT-POINT           
              END-IF
              IF PRC-EFF-DTE-FND AND NOT PARMFILE-EOF
                 MOVE PR-PRC-EFF-DTE TO H-PRC-EFF-DTE
              END-IF
              IF AUTOREFRESH-FND AND NOT PARMFILE-EOF
                 MOVE PR-AUTOREFRESH-FLG TO H-AUTOREFRESH-FLG
                 IF PR-AUTOREFRESH-NBR NUMERIC AND
                    PR-AUTOREFRESH-NBR < 6
                    MOVE PR-AUTORESFRESH-NBR TO H-AUTOREFRESH-NBR
                 END-IF
              ELSE
                 IF PARM-COUNT= ZERO OR
                    (LCT-IDX = ZERO AND STR-IDX = ZERO)
                     MOVE 'N' TO WS-PARMFILE-SW
                 END-IF
              END-IF.

       0810-LOAD-LCTS.
           MOVE 1 TO PARM-IDX
           PERFORM UNTIL PARM-IDX > PARM-MAX OR
                         PARM-LCT(PARM-IDX) = SPACE OR
                         LCT-IDX = LCT-MAX
                 ADD 1 TO LCT-IDX
                 MOVE PARM-LCT(PARM-IDX) TO LCT-NBR(LCT-IDX)
                                            T063-LCT-NBR OF T3375-REC
                 PERFORM 0700-CHECK-NEW-LCT
                 MOVE NEW-LCT-IDC TO LCT-NEW(LCT-IDX)
                 ADD 1 TO PARM-IDX
           END-PERFORM.


       0820-LOAD-STRS.

           IF PARM-LCT-X(1) = 'ALL'
              MOVE 'Y' TO REFRESH-ALL-SW
              MOVE SPACE TO PARM-LCT-X(1)
           END-IF

           MOVE 1 TO PARM-IDX
           PERFORM UNTIL PARM-IDX > PARM-MAX OR
                         PARM-LCT(PARM-IDX) = SPACE OR
                         STR-IDX = STR-MAX
               ADD 1 TO STR-IDX
               MOVE PARM-LCT(PARM-IDX) TO STR-NBR(STR-IDX)
               ADD 1 TO PARM-IDX
           END-PERFORM.

       0840-READ-PARMFILE.
           READ PARMFILE INTO PARMFILE-REC
              AT END MOVE 'Y' TO WS-PARMFILE-SW
              NOT AT END DISPLAY 'PARM-REC: ' PARMFILE-REC
              ADD 1 TO PARM-COUNT
           END-READ.

       0845-READ-MISF999A.

           IF DOING-ALL-STORES
              MOVE T164-LCT-GRP-NBR TO MIS-STORE-NBR
           END-IF

           PERFORM UNTIL (T164-LCT-GRP-NBR NOT > MIS-STORE-NBR)
              READ MISF99A INTO MISF999A-REC
                AT END MOVE 9999 TO MIS-STORE-NBR
              END-READ
            
              IF NOT MISF999A-VALID-STATUS
                 MOVE 'MISF999A'  TO WS-FLE-NME
                 MOVE  MISF999A-STATUS TO WS-FLE-ST-CD
                 MOVE 'READ-IN' TO WS-FLE-FNC
                 CALL 'JOBLOG3' USING WS-FLE-ERROR
                 PERFORM 0999-ABORT
              END-IF

              IF T164-LCT-GRP-NBR = MIS-STORE-NBR
                 MOVE 'Y' TO WS-MIS-STORE-SW
              ELSE
                 MOVE 'N' TO WS-MIS-STORE-SW
                 DISPLAY '*------------ STORE --> ' T164-LCT-GRP-NBR
                         'BYPASSED --- NOT IN COMLINK FILE'
              END-IF.

       0850-FORMAT-PRCF610B.

           IF LBRST-PRICE
              ADD 1 TO LBRST-PC
           ELSE IF LBRPZ-PRICE
              ADD 1 TO LBRPZ-PC
           ELSE
              ADD 1 TO CORP-PC
           END-IF.

           IF NEW-ITM AND NEW-ITM-COUNT
              MOVE 'N' TO NEW-ITM-COUNT-IDC
              ADD 1 TO NEW-ITEMS
           END-IF

           MOVE T024-ITM-NBR OF T2567-REC TO PRCF610B-ITEM-NUM
           MOVE T164-LCT-GRP-NBR OF T165-REC TO PRCF610B-STORE
           MOVE 'INSB610'     TO PRCF610B-BUYER
           MOVE 'M'           TO PRCF610B-TCODE
           MOVE 'R'           TO PRCF610B-STATUS-CODE
           MOVE ' '           TO PRCF610B-STOCK-CODE
           MOVE '9'           TO PRCF610B-ELP-CODE
           MOVE CST-AMT       TO PRCF610B-COMP-NET
           MOVE CST-AMT       TO PRCF610B-STORE-NET
           MOVE RTL-PRC-AMT   TO PRCF610B-CONSUMER-SELL
           MOVE WS-FORM-TOMORROW TO PRCF610B-EFF-DATE
           MOVE 65            TO PRCF610B-PRIORITY-SEQ
           MOVE -0.01         TO PRCF610B-LMP-AMT
           MOVE 00000000      TO PRCF610B-LMP-EXP-DT
           MOVE '00000000'    TO PRCF610B-NLP-EXP-DT
                                 PRCF610B-NLP-EFC-DT

           WRITE PRCF610B-RECORD FROM PRCF610B-REC
           IF NOT PRCF610B-VALID-STATUS
              MOVE 'PRCF610B'  TO WS-FLE-NME
              MOVE PRCF610B-STATUS TO WS-FLE-ST-CD
              MOVE 'WRITE' TO WS-FLE-FNC
              CALL 'JOBLOG3' USING WS-FLE-ERROR
              PERFORM 0999-ABORT
           END-IF
           ADD 1 TO PRCF610B-COUNT.

       0900-CLOSE-FILES.
           IF PARMFILE-EOF
              CLOSE PARMFILE
              IF NOT PARMFILE-VALID-STATUS
                 MOVE 'PARMFILE' TO WS-FLE-NME
                 MOVE PARMFILE-STATUS TO WS-FLE-ST-CD 
                 MOVE 'CLOSE'  TO WS-FLE-FNC
                 CALL 'JOBLOG3' USING WS-FLE-ERROR
                 PERFORM 0999-ABORT
              END-IF
           END-IF.

           CLOSE MISF999A.
           IF NOT MISF999A-VALID-STATUS
              MOVE 'MISF999A'  TO WS-FLE-NME
              MOVE MISF999A-STATUS TO WS-FLE-ST-CD
              MOVE 'CLOSE' TO WS-FLE-FNC
              CALL 'JOBLOG3' USING WS-FLE-ERROR
              PERFORM 0999-ABORT
           END-IF.

           CLOSE PRDF610B.
           IF NOT PRDF610B-VALID-STATUS
              MOVE 'PRDF610B'  TO WS-FLE-NME
              MOVE PRDF610B-STATUS TO WS-FLE-ST-CD
              MOVE 'CLOSE' TO WS-FLE-FNC
              CALL 'JOBLOG3' USING WS-FLE-ERROR
              PERFORM 0999-ABORT
           END-IF.


       0920-CLOSE-T165A.

           MOVE 'N' TO COUNT-LCT-SW
           EXEC SQL
              CLOSE T165A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'CLOSE165A' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABEND**SQL CODE FAILED T165A '
                           TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0930-CLOSE-T2567A.
           EXEC SQL
              CLOSE T2567A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'CLS2567A' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABEND**SQL CODE FAILED T2567A '
                           TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0940-CLOSE-T3422A.

           EXEC SQL
              CLOSE T3422A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'CLS3422A' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABEND**SQL CODE FAILED T3422A '
                           TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0950-CLOSE-T3377A.

           EXEC SQL
              CLOSE T3377A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'CLS3377A' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABEND**SQL CODE FAILED T3377A '
                           TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

           
       0960-CLOSE-ADV107A.

           EXEC SQL
              CLOSE ADV107A
           END-EXEC.

           IF NOT SQL-OK
              MOVE 'CLS197A' TO WS-EXE-IDF
              MOVE SQLCODE TO WS-SQL-CODE
              MOVE '** PROGRAM ABEND**SQL CODE FAILED ADV107A '
                           TO WS-ABORT-MESSAGE
              CALL 'SQLMSGB3' USING WS-PROG-ID, WS-EXE-IDF, SQLCA
              CALL 'JOBLOG3' USING WS-SQL-ERROR
              PERFORM 0999-ABORT
           END-IF.

       0999-ABORT.

           DISPLAY '**** INSB610 A B O R T I N G ****'.
           CALL 'CANCEL3'.
