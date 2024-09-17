******** ************************************************************** 00010005
******** ************************************************************** 00020005
********    'START OF' COMPILE/ASSEMBLE OPTIONS FOR HRHSAO3C            00030005
********    ALL VALUES MUST BE IN ' '                                   00040005
******** ************************************************************** 00050005
******** 'ACCESS CODE   ' = 'JD7YJP'                                    00060005
******** 'COMPILE PARMS ' = 'AWO,F(I,W),FASTSRT,LIST,MAP,NOSEQ,LIB'     00070005
******** 'COMPILE PARMS ' = 'NOSSR,LC(59),TERM,RENT'                    00080005
******** 'SYMBOLIC PARMS' = 'AWO,F(I,W),FASTSRT,LIST,MAP,NOSEQ,LIB'     00090005
******** 'SYMBOLIC PARMS' = 'NOSSR,LC(59),TERM,RENT'                    00100005
******** 'LINK PARMS    ' = 'LIST,XREF,TERM,MAP,SIZE=(512K,36K)'        00110005
******** 'LINK PARMS    ' = 'AMOD=ANY,RMOD=24,RENT'                     00120005
******** 'ASSEMBLE      ' = 'N'    'ASSEMBLER H          ' = 'N'        00130005
******** 'COMPILE       ' = 'Y'    'VSII COBOL RELEASE   ' = 'MVS'      00140005
******** 'DB2           ' = 'N'                                         00150005
******** 'CICS RELEASE  ' = 'N'    'LISTER NAME          ' = 'HRHSAO3C' 00160005
******** 'BMS MAP       ' = 'N'    'BMS DSECT MEMBER NAME' = '       '  00170005
******** 'S0CK MAP      ' = 'N'    'S0C DSECT MEMBER NAME' = '       '  00180005
******** 'CCOPY         ' = 'N'                                         00190005
******** 'DRIVER        ' = 'N'                                         00200005
******** 'SUPERSET      ' = 'N'                                         00210005
******** 'MASK VERSION  ' = '10/06/1989' <= DO NOT CHANGE THIS LINE     00220005
******** ************************************************************** 00230005
********    'END OF' COMPILE/SETEMBLE OPTIONS FOR HRHSAO3C              00240005
******** ************************************************************** 00250005
      /* ************************************************************** 00260005
      ***************************************************************** 00270005
      *                                                               * 00280005
      *                        HRHSAO3C                               * 00290005
      *                                                               * 00300005
      *    FUNCTION:                                                  * 00310005
      *                                                               * 00320005
      *    THIS PROGRAM WILL TAKE THE CLAIMS SELECTED BY HRHPFBC2     * 00330005
      *    TO BE SENT TO ADVANCE PCS.  NEW STATUS 9 CLAIMS WILL BE    * 00340005
      *    GIVEN AS THEIR ACTUAL DOLLAR AMOUNTS.  FLUSHED STATUS 9    * 00350005
      *    CLAIMS WILL BE GIVEN AS THE NEGATIVE OF THEIR CURRENT      * 00360005
      *    DOLLAR AMOUNTS. THIS PROGRAM WILL REFORMAT THE CLAIM DATA  * 00370005
      *    INTO ONE RECORD PER MEMBER PER ADJ DATE PER ADJ TYPE.      * 00380005
      *                                                               * 00390005
      *    ENTRY POINTS: TOP OF PROGRAM HRHSAO3C.                     * 00400005
      *                                                               * 00410005
      *                                                               * 00420005
      *    INPUT FILE:   - HRHNIAPC - ADVANCE PCS TYPE-7 DETAIL FILE  * 00430005
      *                               (SORTED BY MEMBER ID AND        * 00440005
      *                                          ADJUSTMENT DATE)     * 00450005
      *                                                               * 00460005
      *    OUTPUT FILE:  - HRHNCRMK - ADVANCE PCS FILE WITH           * 00470005
      *                               HEADER  (TYPE 1),               * 00480005
      *                               DETAIL  (TYPE 7),               * 00490005
      *                               TRAILER (TYPE 9).               * 00500005
      *                                                               * 00510005
      *    INPUT PARMS:  - NONE                                       * 00520005
      *                                                               * 00530005
      *    OUTPUT PARMS: - NONE                                       * 00540005
      *                                                               * 00550005
      *    EXITS:        - WAASABND - PRODUCE USER ABEND              * 00560005
      *                                                               * 00570005
      *    ABENDS:         U1000 - BAD OPEN  OF HRHNIAPC              * 00580005
      *                    U1001 - BAD OPEN  OF HRHNCRMK              * 00590005
      *                    U1002 - BAD CLOSE OF HRHNIAPC              * 00600005
      *                    U1003 - BAD CLOSE OF HRHNCRMK              * 00610005
      *                    U1004 - BAD READ  OF HRHNIAPC              * 00620005
      *                    U1005 - BAD WRITE OF HRHNIAPC              * 00630005
      *                                                               * 00640005
      *    MODIFICATIONS:- DISPLAYED IN THE LOG BELOW                 * 00650005
      *                                                               * 00660005
      ***************************************************************** 00670005
******** ************************************************************** 00680005
********           MODIFICATION LOG FOR HRHSAO3C                        00690005
********  SCAN   ACTIVITY DATE     SE NAME                              00700005
********  ------ -------- -------- ------------------------------------ 00710005
********          NEW PROGRAM                                           00710105
********  NJ8241 NJ038241 08/01/07 ROBIN SOLLENBERGER                   00710205
********  AN4993 ANI14993 10/10/07 ANIL SADANANDAN                      00710305
********          CREATE NEW OUTPUT FOR ANTHEM 'ACARE' VENDOR           00710405
********  NJ0192 NJ040192 10/17/07 REMYA SASIDHARAN                     00710505
********          ADDED TIMESTAMP TO RECORD ID AND CLAIM ID             00710605
********  NJ9171 NJ039171 10/24/07 APARNA  SHAILENDRA                   00710705
********          TO ADD A PREFIX TO THE OUTPUT FILES                   00710805
********  AN4993 ANI14993 12/14/07 ANIL SADANANDAN                      00710905
********          RESET THE PAID SWITCH WHEN NEW ENROLLMENT             00711005
********  NRH112 NRCDH112 03/14/08 ROBIN SOLLENBERGER/                  00712005
********                           ANIL SADANANDAN                      00713005
********          SET PAT PAY EXCL DED TO ZERO ALWAYS                   00714005
********  AN8181 ANM28181 04/15/08 SAHANA  DESAI                        00715005
********          ADDED A NEW PROVIDER NUMBER THAT EXCLUDES THE         00716005
********          FINALIZED CLAIMS FROM GOING BACK OUT TO CRMK OUTBND   00717005
********          FILE                                                  00718005
********  NJUE03 NJLDUE03 10/20/08 AMARESHA GOWDA                       00719005
********          CHANGING MAPPING OF FIELD W-HOLD-LINE                 00720005
********                                                                00730005
********  MI1323 MI081323 12/15/08 PRADEEP TRIPATHI                     00740005
********          CREATE NEW OUTPUT FOR MICHIGAN 'MCARE' VENDOR         00750005
********          EXCLUDE CLAIM OF PROVIDER ID 7102000CMARK01           00760005
********  AN8824 ANI18824 12/22/08 AMARESHA GOWDA                       00770005
********          CHANGES TO INCLUDE NATIONWIDE ACCOUNT FOR ANTHEM      00780005
********  MI2155 MI082155 02/09/09 AMARESHA GOWDA                       00790005
********          CHANGES TO INCLUDE HENNIGES GROUP                     00800005
********  AN1855 ANI21855 09/16/09 HASSAN M MOHIDEEN                    00810005
********          CODE TO INCLUDE GENO TABLE CDHFLDMP FROM WHICH CLIENT 00820005
********          ID CAN BE MOVED TO OUTBOUND FILE TO ANTHEMCREMARK.    00830005
********  NJ1164 NJ051164 11/06/09 LINGESH S RAMPUR                     00840005
********          CREATE NEW OUTPUT FOR 'NJCDH' VENDOR                  00850005
********  AN1887 ANI21887 01/01/10 LINGESH S RAMPUR                     00860005
********          ADD NEW OUTBOUND FILE FOR AOPTM                       00870005
********  MA2010 MA012010 07/09/10 HASSAN M MOHIDEEN                    00880005
********          CREATE NEW OUTPUT FOR MA CAREMARK 'MACMK' VENDOR      00890005
********  MD2689 MDG02689 02/04/11 BINU K SUBRAMANIAN                   00900005
********          CHANGES FOR CAREFIRST                                 00910005
********  WDR001 09084943 03/02/11 DAN REEVES                           00920005
********          REMOVE DATA(24) FROM COMP MASK TO REDUCE REGION       00930005
********  AN6295 ANI36295 03/31/11 TAPASYA PURI                         00940005
********          BYPASS 2200BHCOMINGLE                                 00950005
********  NJ0983 NJ060983 10/13/11 BINU KS                              00960005
********          USE INVENOMP GENO FOR NJ                              00970005
********  AN4656 AN5T4656 12/23/11 AMARESH                              00980005
********          ADD CHANGES FOR IBM GROUPS                            00990005
********  MI1537 MI101537 12/31/11 BINU KS                              01000005
********          PREVENT CDH IND = S CLAIMS AT STATUS 8                01010005
********  NX1471 NX001471 01/18/12 AMARESHA GOWDA                       01020005
********          CHANGES FOR CLIENT ID FIX                             01030005
********  MN0489 MN000489 03/15/12 BILL CLITES                          01040005
********          CHANGED GROUP SECTION TO PIC X TO MATCH CLAIM         01050005
********          AND GENO DEFINITION                                   01060005
********          CHANGED L-INVM-RETURN-OUTBOUND-RECORD PARM USED TO    01070005
********          CALL HRHSINVMN TO PIC X(10000)                        01080005
********          CALL HRHSINVMN TO PIC X(10000)                        01090005
********  ANI547 ANI54719 10/10/12 BILL CLITES                          01100005
********          ADDED NEW COMINGLE CAT THAT WILL MAP DED TO OOP       01110005
********          IF RECEIVED FROM INVENOUT GENO.                       01120005
********  ANI612 ANI61277 04/08/13 BILL CLITES                          01130005
********          REMOVED READING OF SYSIN MEMBER TO DETERMINE          01140005
********          WHAT VALUE WAS TO BE MAPPED TO AOPTM HMO NETWORK      01150005
********          INITICATOR.                                           01160005
********  MA5057 MA015057 04/15/13 PANDIAN SUNDARAMOORTHY               01170005
********          TO PASS ONLY FIRST NINE BYTES TO VSAM XREF FILE       01180005
********          IN ORDER TO GET PATIENT-ID FOR VENDOR MACMK           01190005
NR1362**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01200005
NR1362**  CSR#      : NR001362 11/21/2013 GREG WELSH                    01210005
NR1362**  EMAIL     : GREG.WELSH@NASCO.COM                              01220005
NR1362**  REQRMENT  : CREATING REPORTS FOR EAV TOTAL CLAIMS VOLUME      01230005
NR1362**  SOLUTION  : ADDED CODE TO PASS BACK TO DRIVER PROGRAM WHEN    01240005
NR1362**            : A RECORD IS WRITTEN OUT.                          01250005
MD6619**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01260005
MD6619**  CSR#      : MD006619 10/29/2013 NAVEEN NARAYANRAO             01270005
MD6619**  EMAIL     : NAVEENKN@IN.IBM.COM                               01280005
MD6619**  REQRMENT  : CVSCM-MAP SPACES TO PATIENT-ID FROM POS 16-20.    01290005
MD6619**  SOLUTION  : CVSCM-MAP SPACES TO PATIENT-ID FROM POS 16-20.    01300005
MI0730**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01310005
MI0730**  CSR#      : MI140730 04/07/2014 GREG SEABORN                  01320005
MI0730**  EMAIL     : GREG.SEABORN@NASCO.COM                            01330005
MI0730**  REQRMENT  : TKUSA MEDICAL CLAIMS CORRECTION                   01340005
MI0730**  SOLUTION  : PERFORM PROCESS-CLAIM FOR GROUP BASE 000071314    01350005
MI0730**            : REMOVE DEBUG FUNCTION                             01360005
AN9308**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01370005
AN9308**  CSR#      : ANI69308 07/11/2014 LINDA BURGESS                 01380005
AN9308**  EMAIL     : LINDA.BURGESS@NASCO.COM                           01390005
AN9308**  REQRMENT  : NEED NEW COM CAT VALUES                           01400005
AN9308**  SOLUTION  : ADD NEW COM CAT VALUES                            01410005
MA5098**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01420005
MA5098**  CSR#      : MA015098 09/23/2014 TIM ELLIS                     01430005
MA5098**  EMAIL     : TIM.ELLIS@NASCO.COM                               01440005
MA5098**  REQRMENT  : MAP SPOUSE SEXREL DOMESTIC PARTNER F4 AND M4      01450005
MA5098**  SOLUTION  : ADD PLAN 700 VENDOR MACMK SEX REL MAPPING         01460005
MA5098**            : WHEN MEMBER STATUS 05 IS DOMESTIC PARTNER.        01470005
NR0007**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01480005
NR0007**  CSR#      : NRAV0007 07/16/2014 CHUCK SNYDER                  01490005
NR0007**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          01500005
NR0007**  REQRMENT  : RTI                                               01510005
NR0007**  SOLUTION  : USE COMMON COPYBOOK FOR LINKAGE SECTION           01520005
MD7568**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01530005
MD7568**  CSR#      : MDDC7568 12/04/2014 NAVEEN NARAYANRAO             01540005
MD7568**  EMAIL     : NAVEENKN@IN.IBM.COM                               01550005
MD7568**  REQRMENT  : CF/CVS OUTBOUND FILE ACCUM FOR CARVEOUT           01560005
MD7568**  SOLUTION  : PICK SSN FROM ELIGIBILITY IF NOT ON CLAIM         01570005
MI2783**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01580005
MI2783**  CSR#      : MI142783 02/16/2015 MARLENE CRONIN                01590005
MI2783**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          01600005
MI2783**  REQRMENT  : HAVE ALL MI VENDORS USE INVENOMP                  01610005
MI2783**  SOLUTION  : ADD LOGIC FOR MI PLAN CODE 710 TO USE CALL        01620005
MI2783**            : TO INVENOMP LOGIC SIMILAR TO 780                  01630005
MI2783**            : REMOVE CALL USING CDHFLDMP                        01640005
AN7535**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01650005
AN7535**  CSR#      : ANI87535 04/09/2015 LINDA BURGESS                 01660005
AN7535**  EMAIL     : LINDA.BURGESS@NASCO.COM                           01670005
AN7535**  REQRMENT  : CHANGE COM CAT G & J CALC OF OOP                  01680005
AN7535**  SOLUTION  : ADD DED AMT TO OON OOP FOR COM CATS G/J           01690005
MD8008**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01700005
MD8008**  CSR#      : MDDC8008 04/22/2015 CHUCK SNYDER                  01710005
MD8008**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          01720005
MD8008**  REQRMENT  : ALLOW CAREFIRST TO USE INVENOMP GENO              01730005
MD8008**  SOLUTION  : ADD PLAN 580 AND 690 CHECKS FOR INVENOMP USE      01740005
NJ7557**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01750005
NJ7557**  CSR#      : NJ077557 04/29/2015 LINDA BURGESS                 01760005
NJ7557**  EMAIL     : LINDA.BURGESS@NASCO.COM                           01770005
NJ7557**  REQRMENT  : NEED INFO FROM OBND FOR NEW REPORT                01780005
NJ7557**  SOLUTION  : PASS INFO ON VENDOR FILES FOR REPORTING           01790005
NRB066**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01800005
NRB066**  CSR#      : NRRBB066 09/03/2015 ROD BOYER                     01810005
NRB066**  EMAIL     : ROD.BOYER@NASCO.COM                               01820005
NRB066**  REQRMENT  : COMBINED OOP PROC (MI)                            01830005
NRB066**  SOLUTION  : CREATE NEW COMINGLE CATEGORY                      01840005
NJ9032**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01850005
NJ9032**  CSR#      : NJ079032 05/11/2015 MARLENE CRONIN                01860005
NJ9032**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          01870005
NJ9032**  REQRMENT  : FOR NJCDH ACCESS NEW GENO TABLE TO DETERMINE      01880005
NJ9032**  SOLUTION  : NEW RENEWAL DATE USE AND CHECK 4TH QUARTER        01890005
NJ9032**            : INDICATOR TO MOVE CAR-DED-AMT TO CAR-OOP-AMT      01900005
NRB062**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01910005
NRB062**  CSR#      : NRRBB062 08/04/2015 CHUCK SNYDER                  01920005
NRB062**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          01930005
NRB062**  REQRMENT  : RBB - EAV COMBINED OOP PROC                       01940005
NRB062**  SOLUTION  : CODE FOR NEW COM CAT VALUES ON INVENOUT GENO      01950005
NRB062**            : TABLE                                             01960005
NJ1348**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   01970005
NJ1348**  CSR#      : NJ081348 10/10/2015 PANDIAN S                     01980005
NJ1348**  EMAIL     : PANDIAN.SUNDARAMOORTHY@NASCO.COM                  01990005
NJ1348**  REQRMENT  : MAP TIER 2 BENEFIT FLAG                           02000005
NJ1348**  SOLUTION  : ADD 'T2' AFTER READING THE BENEFIT FLAG(BFLAG3)   02010005
NJ1348**            : FOR TIER 1 & 2.                                   02020005
AN1589**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02030005
AN1589**  CSR#      : ANI91589 10/08/2015 MARLENE CRONIN                02040005
AN1589**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          02050005
AN1589**  REQRMENT  : ADD VENDOR MAGALLEN FOR ANTHEM                    02060005
AN1589**  SOLUTION  : ADD VENDOR 'MAGLN' TO CODE WHERE APPLICABLE       02070005
MD7684**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02080005
MD7684**  CSR#      : MDDC7684 06/03/2015 PANDIAN S                     02090005
MD7684**  EMAIL     : PANDIAN.SUNDARAMOORTHY@NASCO.COM                  02100005
MD7684**  REQRMENT  : ADD CARRIER CODES TO CVSCM VENDOR                 02110005
MD7684**  SOLUTION  : CHANGES MADE TO MAP THE CARRIER NUMBER, PARTIC-   02120005
MD7684**            : IPANT NUMBER FROM HDRT5W2K TABLE.                 02130005
MI2203**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02140005
MI2203**  CSR#      : MI152203 11/20/2015 PANDIAN S                     02150005
MI2203**  EMAIL     : PANDIAN.SUNDARAMOORTHY@NASCO.COM                  02160005
MI2203**  REQRMENT  : MADE CHANGES TO PROCESS STATUS 8 CLAIMS           02170005
MI2203**  SOLUTION  : PROCESS STATUS 8/9/A CLAIMS WITHOUT CHECKING      02180005
MI2203**            : CDH INDICATOR                                     02190005
NJ3424**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02200005
NJ3424**  CSR#      : NJ083424 06/09/2016 NAVEEN NARAYARNAO             02210005
NJ3424**  EMAIL     : NAVEEN.NARAYANRAO@NASCO.COM                       02220005
NJ3424**  REQRMENT  : HCV - DAY 2 EAV OUTBOUND ENHANCEMENTS             02230005
NJ3424**  SOLUTION  : CHANGED THE MAPPING OF CAR-TIER2-BENEFIT-FLAG     02240005
NJ3424**  SOLUTION  : FIELD FOR NJCDH VENDOR                            02250005
NJ5584**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02260005
NJ5584**  CSR#      : NJ085584 10/07/2016 CHUCK SNYDER                  02270005
NJ5584**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02280005
NJ5584**  REQRMENT  : RWJ EAV OUTBOUND CST UPDATE                       02290005
NJ5584**  SOLUTION  : ADD NEW T9 TIER VALUE TO OUTBOUND MAPPING         02300005
NJ7476**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02310005
NJ7476**  CSR#      : NJ087476 06/06/2017 CHUCK SNYDER                  02320005
NJ7476**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02330005
NJ7476**  REQRMENT  : RWJB - OUTBOUND FIX                               02340005
NJ7476**  SOLUTION  : MAKE TIER 2 BENEFIT FLAG DEFAULT TO SPACES        02350005
NJ7476**            : WHEN COST SHARE TIER STATUS IS '2'                02360005
MA0364**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02370005
MA0364**  CSR#      : MA020364 04/06/2017 MARLENE CRONIN                02380005
MA0364**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          02390005
MA0364**  REQRMENT  : REMOVE LOGIC FOR TO ALLOW STATISTICAL CLAIMS      02400005
MA0364**  SOLUTION  : REMOVED CODE SO THE STATISTICAL CLAIMS WILL       02410005
MA0364**            : BE WRITTEN TO OUTBOUND FILE                       02420005
AN0609**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02430005
AN0609**  CSR#      : AND00609 08/17/2017 BILL CLITES                   02440005
AN0609**  EMAIL     : WILLIAM.CLITES@NASCO.COM                          02450005
AN0609**  REQRMENT  : CALL NEW GENO TABLE FOR 4TH QTR MAPPING           02460005
AN0609**  SOLUTION  : ADD CODE TO CALL NEW GENO TABLE                   02470005
AN0609**  SOLUTION  : REMOVED OLD COMMENT CODE MO VERSION BEFORE        02480005
AN0609**  SOLUTION  : REMOVAL CAN BE FOUND ON P: UNDER THIS CSR         02490005
MA8679**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02500005
MA8679**  CSR#      : MA018679 08/22/2017 CHUCK SNYDER                  02510005
MA8679**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02520005
MA8679**  REQRMENT  : EAV/CDH MODIFY OUTBOUND SSN MAPPING               02530005
MA8679**  SOLUTION  : CHANGES TO SSN MAPPING LOGIC                      02540005
NRS012**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02550005
NRS012**  CSR#      : NRAPS012 04/21/2017 CHUCK SNYDER                  02560005
NRS012**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02570005
NRS012**  REQRMENT  : APS TRACK 5 BACK END HISTORY                      02580005
NRS012**  SOLUTION  : CHANGE SUB ID NUMERIC CHECK TO LOOK AT PGM        02590005
NRS012**            : CODE                                              02600005
NJ8469**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02610005
NJ8469**  CSR#      : NJ088469 10/09/2017 MARLENE CRONIN                02620005
NJ8469**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          02630005
NJ8469**  REQRMENT  : HACK-MERID UPDATE                                 02640005
NJ8469**  SOLUTION  : ADD COST SHARE TIER STATUS FOR 3                  02650005
AN1105**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02660005
AN1105**  CSR#      : AND01105 01/03/2018 CHUCK SNYDER                  02670005
AN1105**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02680005
AN1105**  REQRMENT  : SSCR 38790 EAV 4TH QUARTER CARRYOVER DED          02690005
AN1105**  SOLUTION  : FOR DED AMTS GREATER THAN ZERO, UPDATE REC-ID     02700005
AN1105**            : AND CLM-ID WITH 4Q                                02710005
AN0127**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02720005
AN0127**  CSR#      : AND00127 11/18/2016 CHUCK SNYDER                  02730005
AN0127**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02740005
AN0127**  REQRMENT  : ADD NEW EAV VENDOR BEACON HEALTH                  02750005
AN0127**  SOLUTION  : CHANGES TO ADD NEW VENDOR BEACON                  02760005
MA1079**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02770005
MA1079**  CSR#      : MA021079 05/17/2018 MARLENE CRONIN                02780005
MA1079**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          02790005
MA1079**  REQRMENT  : ADD NEW FIELDS                                    02800005
MA1079**  SOLUTION  : INCREASE DETAIL RECORD WITH NEW FIELDS            02810005
MA1079**            : FOR MACMK (REPORTING ONLY)                        02820005
MA0815**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02830005
MA0815**  CSR#      : MA020815 12/05/2017 CHUCK SNYDER                  02840005
MA0815**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          02850005
MA0815**  REQRMENT  : ACCUM/MOOP - TRANSACTION REPOSITORY               02860005
MA0815**  SOLUTION  : ADD SUPPLEMENTAL FIELDS FROM PENDING              02870005
AN1308**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02880005
AN1308**  CSR#      : AND01308 06/11/2018 MARLENE CRONIN                02890005
AN1308**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          02900005
AN1308**  REQRMENT  : ADD ACTIVATION PLAN CODE                          02910005
AN1308**  SOLUTION  : PLAN CODE ADDED TO OUTBOUND RECORD FOR            02920005
AN1308**            : ACARE,AOPTM,BEACN AND MAGLN                       02930005
MA1368**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   02940005
MA1368**  CSR#      : MA021368 03/15/2018 BILL CLITES                   02950005
MA1368**  EMAIL     : WILLIAM.CLITES@NASCO.COM                          02960005
MA1368**  REQRMENT  : ADD RECYCLE INDICATOR TO FILE FOR DQ/DR RPT       02970005
MA1368**  SOLUTION  : ADD LOGIC TO ACCEPT RECYCLE IND AND ADD  E        02980005
MA1368**            : TO 3000 BYTE FILE                                 02990005
MA1368**  REQRMENT  : SETTING ZERO DOLLAR INDICATOR FOR REPORT          03000005
MA1368**  SOLUTION  : ADD LOGIC TO SET BNCH-ID TO Z FOR ZERO DOLLAR     03010005
MA1368**            : CLAIMS. ENSURE INDICATORS RESET WHEN CHANGE OF    03020005
MA1368**            : ICN OCCURS.                                       03030005
NR0099**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03040005
NR0099**  CSR#      : NRAV0099 06/18/2019 CHUCK SNYDER                  03050005
NR0099**  EMAIL     : CHARLES.SNYDER@NASCO.COM                          03060005
NR0099**  REQRMENT  : CORRECT DEFECT IN OUTBOUND EAV MERGE LOGIC        03070005
NR0099**  SOLUTION  : TEMPORARILY STORE ACTIVATION PLAN CODE ON         03080005
NR0099**            : OUTPUT FILE                                       03090005
AN1560**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03100005
AN1560**  CSR#      : AND01560 05/28/2019 MARLENE CRONIN                03110005
AN1560**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          03120005
AN1560**  REQRMENT  : ADD NEW VENDOR AOPTR FOR ANTHEM                   03130005
AN1560**  SOLUTION  : ADDED NEW VENDOR AOPTR TO PLAN 834 LOGIC          03140005
AN1781**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03150005
AN1781**  CSR#      : AND01781 02/10/2020 MARLENE CRONIN                03160005
AN1781**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          03170005
AN1781**  REQRMENT  : ADD ARCHIMEDES VENDOR FOR ANTHEM  AARCH           03180005
AN1781**  SOLUTION  : ADD CODE SIMILAR TO AOPTR FOR AARCH               03190005
NJ324D**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03200005
NJ324D**  CSR#      : NJ00324D 11/11/2020 RANDALL LETT                  03210005
NJ324D**  EMAIL     : RANDALL.LETT@NASCO.COM                            03220005
NJ324D**  REQRMENT  : ADD 4TH TIER VALUE                                03230005
NJ324D**  SOLUTION  : WHEN BENEFIT FLAG 518                             03240005
NJ476C**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03250005
NJ476C**  CSR#      : NJ00476C 09/14/2020 MARLENE CRONIN                03260005
NJ476C**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          03270005
NJ476C**  REQRMENT  : REQRMENT  : IDENT POS CLAIMS DURING EAV PROC      03280005
NJ476C**  SOLUTION  : SOLUTION  : ADD POS FLAG AND SET IF APPLIES       03290005
TR354I**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03291006
TR354I**  CSR#      : TR00354I 02/02/2021 MARLENE CRONIN                03292006
TR354I**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          03293006
TR354I**  REQRMENT  : REQRMENT  : DON'T INCLUDE CLMS W/BLNK HMO-IND     03294006
TR354I**  SOLUTION  : SOLUTION  : ADDED CODE TO PASS TO SUB-MODULES     03295006
TR354I**            : IF POS CLAIM HAD HMO-IND BLANK SO NOT ON RPT      03296006
MD838A**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03296107
MD838A**  CSR#      : MDDC838A 02/24/2021 MARLENE CRONIN                03296207
MD838A**  EMAIL     : MARLENE.CRONIN@NASCO.COM                          03296307
MD838A**  REQRMENT  : REQRMENT  : ADJ. W/DOS>MEMBER TERM NOT SENT       03296407
MD838A**  SOLUTION  : SOLUTION  : ADD NEW CODE TO CALL HRHSID01 TO      03296507
MD838A**            : CHECK DOS AGAINST CANCEL DATE FOR ADJUSTMENTS     03296607
MD161B**- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   03296714
MD161B**  CSR#      : MDDC161B 05/19/2021 NAVEEN NARAYANRAO             03296814
MD161B**  EMAIL     : NAVEEN.NARAYANRAO@NASCO.COM                       03296914
MD161B**  REQRMENT  : ADDRESS IN12053574                                03297014
MD161B**  SOLUTION  : CHANGED CODE TO READ MEMBER SEGMENT               03297114
MD161B**            : IN PLACE OF COVERAGE SEGMENT                      03297214
********  END OF MODLOG FOR HRHSAO3C                                    03297305
      ***************************************************************** 03298005
      /**************************************************************** 03299005
      *                IDENTIFICATION  DIVISION                       * 03300005
      ***************************************************************** 03310005
                                                                        03320005
       IDENTIFICATION DIVISION.                                         03330005
       PROGRAM-ID.    PANDIAN.                                          03340005
       AUTHOR.        ROBIN SOLLENBERGER.                               03350005
       DATE-WRITTEN.  08/01/07.                                         03360005
       DATE-COMPILED.                                                   03370005
                                                                        03380005
      ***************************************************************** 03390005
      *          ENVIRONMENT DIVISION                                 * 03400005
      ***************************************************************** 03410005
                                                                        03420005
       ENVIRONMENT DIVISION.                                            03430005
                                                                        03440005
       CONFIGURATION SECTION.                                           03450005
                                                                        03460005
       SOURCE-COMPUTER. IBM-370.                                        03470005
                                                                        03480005
       INPUT-OUTPUT SECTION.                                            03490005
                                                                        03500005
       FILE-CONTROL.                                                    03510005
                                                                        03520005
MI2155     SELECT GENO-FIELD-MAPPING-FILE ASSIGN TO HRBNGENO            03530005
MI2155                             ORGANIZATION IS INDEXED              03540005
MI2155                             ACCESS MODE IS DYNAMIC               03550005
MI2155                             RECORD KEY IS GENO-VENDOR-KEY        03560005
MI2155                             STATUS IS W-HRBNGENO-STATUS.         03570005
MD7684     SELECT OUT-CVSCM-VEN-FILE  ASSIGN TO HRHNOCVS                03580005
MD7684                           FILE STATUS IS W-FILE-STATUS.          03590005
AN4656*    SELECT OPTIONAL I-AOPTM-BH-CLIENTS ASSIGN TO HRHNIBHF        03600005
AN4656*                  FILE STATUS           IN-AOPTM-BH-FIL-STATUS.  03610005
      /**************************************************************** 03620005
      *          DATA DIVISION                                        * 03630005
      ***************************************************************** 03640005
                                                                        03650005
       DATA DIVISION.                                                   03660005
                                                                        03670005
       FILE SECTION.                                                    03680005
                                                                        03690005
MI2155******************************************************************03700005
MI2155*           CDHFLDMP GENO LAYOUT                                  03710005
MI2155******************************************************************03720005
MI2155 FD  GENO-FIELD-MAPPING-FILE                                      03730005
MI2155     BLOCK CONTAINS 0 RECORDS                                     03740005
MI2155     LABEL RECORDS ARE STANDARD                                   03750005
MI2155     DATA RECORD IS GENO-FIELD-MAPPING-RECORD.                    03760005
MI2155                                                                  03770005
MI2155 01  GENO-FIELD-MAPPING-RECORD.                                   03780005
MI2155     05  GENO-VENDOR-KEY.                                         03790005
MI2155        10  GENO-PLAN-CODEA           PIC  9(04) COMP.            03800005
MI2155        10  GENO-SEQUENCE-NUM         PIC S9(05) COMP-3.          03810005
MI2155        10  GENO-VENDOR-CODE          PIC  X(05).                 03820005
MI2155        10  GENO-GROUP-BASE-NO        PIC  X(09).                 03830005
MI2155        10  GENO-GROUP-SECTION-NO     PIC  X(04).                 03840005
MI2155        10  GENO-PACKAGE-CODEA        PIC  9(03) COMP.            03850005
MI2155     05  GENO-FIELD-NAME              PIC  X(08).                 03860005
MI2155     05  GENO-FIELD-VALUE             PIC  X(30).                 03870005
MI2155     05  FILLER                       PIC  X(02).                 03880005
AN4656* FD I-AOPTM-BH-CLIENTS                                           03890005
AN4656*    RECORDING MODE IS F,                                         03900005
AN4656*    LABEL RECORDS ARE STANDARD,                                  03910005
AN4656*    BLOCK CONTAINS 0 RECORDS.                                    03920005
AN4656*                                                                 03930005
AN4656* 01 I-AOPTM-BH-CLIENT-REC  PIC X(80).                            03940005
MD7684***************************************************************** 03950005
MD7684*                                                                 03960005
MD7684* DDNAME: HRHNOCVS     OUT-CVSCM-VEN-FILE                         03970005
MD7684*                                                                 03980005
MD7684***************************************************************** 03990005
MD7684                                                                  04000005
MD7684 FD  OUT-CVSCM-VEN-FILE                                           04010005
MD7684     RECORDING MODE IS F                                          04020005
MD7684     LABEL RECORDS ARE STANDARD                                   04030005
MD7684     BLOCK CONTAINS 0 RECORDS.                                    04040005
MD7684                                                                  04050005
MD7684 01  OUT-CVSCM-VEND-RECORDS       PIC  X(420).                    04060005
MD7684                                                                  04070005
      ***************************************************************** 04080005
      *          WORKING STORAGE                                      * 04090005
      ***************************************************************** 04100005
                                                                        04110005
       WORKING-STORAGE SECTION.                                         04120005
                                                                        04130005
       01  WS-START                     PIC X(40)                       04140005
           VALUE 'HRHSAO3C-WORKING STORAGE STARTS HERE'.                04150005
                                                                        04160005
      ******************************************************************04170005
      *                                                                *04180005
      *                    U S E R - A B E N D S                       *04190005
      *                                                                *04200005
      ******************************************************************04210005
                                                                        04220005
       01  USER-ABENDS.                                                 04230005
                                                                        04240005
           05  ABEND-CODE               PIC 9(04) VALUE ZERO.           04250005
           05  FILLER                   PIC X(01) VALUE '-'.            04260005
           05  ABEND-MESSAGE            PIC X(40) VALUE SPACE.          04270005
           05  FILLER                   PIC X(17)                       04280005
               VALUE '- FILE STATUS IS '.                               04290005
           05  ERROR-STATUS             PIC 9(04) VALUE ZERO.           04300005
                                                                        04310005
           05  U-ABEND-MSG              PIC  X(40)  VALUE  SPACES.      04320005
NRS012           88  BAD-RETURN-FROM-NASXDATE    VALUE                  04330005
NRS012                   'BAD RETURN FROM NASXDATE               '.     04340005
                                                                        04350005
      /**************************************************************** 04360005
      *          ACCUMULATORS                                         * 04370005
      ***************************************************************** 04380005
                                                                        04390005
       01  ACCUMULATORS.                                                04400005
                                                                        04410005
           05  A-TOT-CRMK-REC-IN        PIC S9(09) COMP SYNC VALUE +0.  04420005
           05  A-TOT-HEADER-RECORDS     PIC S9(09) COMP SYNC VALUE +0.  04430005
           05  A-TOT-TRAILER-RECORDS    PIC S9(09) COMP SYNC VALUE +0.  04440005
           05  A-TOT-CRMK-OUT-RECORDS   PIC S9(09) COMP SYNC VALUE +0.  04450005
AN4993     05  A-BAT-DETAIL-RECORDS-NJ  PIC S9(09) COMP SYNC VALUE +0.  04460005
AN4993     05  A-BAT-DETAIL-RECORDS-AN  PIC S9(09) COMP SYNC VALUE +0.  04470005
MI1323     05  A-BAT-DETAIL-RECORDS-CH  PIC S9(09) COMP SYNC VALUE +0.  04480005
NJ1164     05  A-BAT-DETAIL-RECORDS-CD  PIC S9(09) COMP SYNC VALUE +0.  04490005
MA2010     05  A-BAT-DETAIL-RECORDS-CK  PIC S9(09) COMP SYNC VALUE +0.  04500005
MD2689     05  A-BAT-DETAIL-RECORDS-CF  PIC S9(09) COMP SYNC VALUE +0.  04510005
MD6619     05  A-BAT-DETAIL-RECORDS-CV  PIC S9(09) COMP SYNC VALUE +0.  04520005
AN4993     05  A-BAT-TRL-OOP-AMT-NJ     PIC S9(09) COMP SYNC VALUE +0.  04530005
AN4993     05  A-BAT-TRL-OOP-AMT-AN     PIC S9(09) COMP SYNC VALUE +0.  04540005
MI1323     05  A-BAT-TRL-OOP-AMT-CH     PIC S9(09) COMP SYNC VALUE +0.  04550005
NJ1164     05  A-BAT-TRL-OOP-AMT-CD     PIC S9(09) COMP SYNC VALUE +0.  04560005
MA2010     05  A-BAT-TRL-OOP-AMT-CK     PIC S9(09) COMP SYNC VALUE +0.  04570005
MD2689     05  A-BAT-TRL-OOP-AMT-CF     PIC S9(09) COMP SYNC VALUE +0.  04580005
MD6619     05  A-BAT-TRL-OOP-AMT-CV     PIC S9(09) COMP SYNC VALUE +0.  04590005
AN1887     05  A-BAT-DETAIL-REC-ANOPT   PIC S9(09) COMP SYNC VALUE +0.  04600005
AN1887     05  A-BAT-TRL-OOP-AMT-ANOPT  PIC S9(09) COMP SYNC VALUE +0.  04610005
AN1887     05  AN-TOT-TRAILER-RECORDS   PIC S9(09) COMP SYNC VALUE +0.  04620005
AN1887     05  AN-TOT-CRMK-OUT-RECORDS  PIC S9(09) COMP SYNC VALUE +0.  04630005
AN1887     05  AN-BAT-DETAIL-RECORDS-AN PIC S9(09) COMP SYNC VALUE +0.  04640005
AN1887     05  AN-BAT-TRL-OOP-AMT-AN    PIC S9(09) COMP SYNC VALUE +0.  04650005
AN1589     05  AN-TOT-TRAILER-RECMAGL   PIC S9(09) COMP SYNC VALUE +0.  04660005
AN1589     05  AN-TOT-CRMK-OUT-RECMAGL  PIC S9(09) COMP SYNC VALUE +0.  04670005
AN1589     05  A-BAT-TRL-OOP-AMT-MG     PIC S9(09) COMP SYNC VALUE +0.  04680005
AN1589     05  AN-BAT-DETAIL-RECMAGL-MG PIC S9(09) COMP SYNC VALUE +0.  04690005
AN0127     05  AN-TOT-TRAILER-RECBCN    PIC S9(09) COMP SYNC VALUE +0.  04700005
AN0127     05  AN-TOT-CRMK-OUT-RECBCN   PIC S9(09) COMP SYNC VALUE +0.  04710005
AN0127     05  A-BAT-TRL-OOP-AMT-BCN    PIC S9(09) COMP SYNC VALUE +0.  04720005
AN0127     05  AN-BAT-DETAIL-RECBCN-BC  PIC S9(09) COMP SYNC VALUE +0.  04730005
AN1560     05  AN-TOT-TRAILER-RECOPTR   PIC S9(09) COMP SYNC VALUE +0.  04740005
AN1560     05  AN-TOT-CRMK-OUT-RECOPTR  PIC S9(09) COMP SYNC VALUE +0.  04750005
AN1560     05  A-BAT-TRL-OOP-AMT-AOR    PIC S9(09) COMP SYNC VALUE +0.  04760005
AN1560     05  AN-BAT-DETAIL-RECOPTR-OR PIC S9(09) COMP SYNC VALUE +0.  04770005
AN1781     05  AN-TOT-TRAILER-RECARCH   PIC S9(09) COMP SYNC VALUE +0.  04780005
AN1781     05  AN-TOT-CRMK-OUT-RECARCH  PIC S9(09) COMP SYNC VALUE +0.  04790005
AN1781     05  A-BAT-TRL-OOP-AMT-AAR    PIC S9(09) COMP SYNC VALUE +0.  04791005
AN1781     05  AN-BAT-DETAIL-RECARCH-OR PIC S9(09) COMP SYNC VALUE +0.  04792005
                                                                        04793005
      ***************************************************************** 04794005
      *          CONSTANTS                                            * 04795005
      ***************************************************************** 04796005
                                                                        04797005
       01  CONSTANTS.                                                   04798005
                                                                        04799005
NR0007     05  BAD-OPEN-HRBNGENO-ABND   PIC 9(04) VALUE 1001.           04800005
NR0007                                                                  04810005
NR0007     05  BAD-OPEN-HRBNGENO-MSG    PIC X(40) VALUE                 04820005
NR0007        'BAD OPEN OF HRBNGENO'.                                   04830005
MI2155     05  BAD-RD-HRBNGENO-ABND     PIC 9(04) VALUE 2009.           04840005
MI2155                                                                  04850005
MI2155     05  BAD-RD-HRBNGENO-MSG      PIC X(40) VALUE                 04860005
MI2155        'BAD READ OF HRBNGENO'.                                   04870005
MI2155                                                                  04880005
           05  BAD-IO-ACCESSOR-CODE     PIC S9(04) COMP SYNC VALUE      04890005
                                                             +2001.     04900005
                                                                        04910005
           05  BAD-Y2KXDATE-CODE-1      PIC S9(04) COMP SYNC VALUE      04920005
                                                             +2002.     04930005
           05  BAD-Y2KXDATE-CODE-2      PIC S9(04) COMP SYNC VALUE      04940005
                                                             +2003.     04950005
           05  BAD-Y2KXDATE-CODE-3      PIC S9(04) COMP SYNC VALUE      04960005
                                                             +2004.     04970005
           05  BAD-Y2KXDATE-CODE-4      PIC S9(04) COMP SYNC VALUE      04980005
                                                             +2005.     04990005
           05  BAD-Y2KXDATE-CODE-5      PIC S9(04) COMP SYNC VALUE      05000005
                                                             +2006.     05010005
           05  BAD-Y2KXDATE-CODE-6      PIC S9(04) COMP SYNC VALUE      05020005
                                                             +2007.     05030005
           05  BAD-Y2KXDATE-CODE-7      PIC S9(04) COMP SYNC VALUE      05040005
                                                             +2008.     05050005
           05  BAD-Y2KXDATE-CODE-8      PIC S9(04) COMP SYNC VALUE      05060005
                                                             +2009.     05070005
           05  BAD-Y2KXDATE-CODE-9      PIC S9(04) COMP SYNC VALUE      05080005
                                                             +2010.     05090005
           05  BAD-Y2KXDATE-CODE-10     PIC S9(04) COMP SYNC VALUE      05100005
                                                             +2011.     05110005
           05  BAD-Y2KXDATE-CODE-11     PIC S9(04) COMP SYNC VALUE      05120005
                                                             +2012.     05130005
                                                                        05140005
           05  BAD-IO-ACCESSOR-MSG      PIC X(40) VALUE                 05150005
               'HRHPFBC2 - BAD RETURN CODE FROM HRHPIOAC'.              05160005
                                                                        05170005
           05  BAD-Y2KXDATE-MSG         PIC X(40) VALUE                 05180005
               'HRHPFBC2 - BAD RETURN CODE FROM Y2KXDATE'.              05190005
                                                                        05200005
           05  BAD-INPUT-DATE-MSG         PIC X(45) VALUE               05210005
               'HRHPFBC2 - BAD INPUT DATE - PROGRAM STOPPED'.           05220005
                                                                        05230005
           05  TABLE-OVERFLOW-MSG       PIC X(40) VALUE                 05240005
               'HRHPFBC2 - TABLE OVERFLOW               '.              05250005
                                                                        05260005
           05  BAD-DATE-TYPE-MSG        PIC X(40) VALUE                 05270005
               'HRHPFBC2 - INVALID DATE TYPE VALUE      '.              05280005
AN1887                                                                  05290005
AN1887     05  BAD-WRITE-HRHNOAOP-MSG   PIC X(40) VALUE                 05300005
AN1887         'BAD WRITE OF HRHNOAOP'.                                 05310005
AN1887                                                                  05320005
AN1887     05  BAD-WRITE-HRHNOAOP-ABND  PIC 9(04) VALUE 1006.           05330005
AN1887                                                                  05340005
AN1887     05  AN-SENDER-ID             PIC X(05) VALUE '00631'.        05350005
AN1887                                                                  05360005
AN1887     05  AN-SENDER-NAME           PIC X(15)                       05370005
AN1887                 VALUE 'ANTHEMSAOPTM834'.                         05380005
AN1887                                                                  05390005
AN1887     05  AN-RECEIVER-ID           PIC X(05) VALUE '00991'.        05400005
AN1887                                                                  05410005
AN1887     05  AN-RECEIVER-NAME         PIC X(15)                       05420005
AN1887                 VALUE 'OPTUM          '.                         05430005
AN1589                                                                  05440005
AN1589     05  BAD-WRITE-HRHNOMAG-MSG   PIC X(40) VALUE                 05450005
AN1589         'BAD WRITE OF HRHNOMAG'.                                 05460005
AN1589                                                                  05470005
AN1589     05  BAD-WRITE-HRHNOMAG-ABND  PIC 9(04) VALUE 1007.           05480005
AN1589                                                                  05490005
AN1589     05  AN-SENDER-ID-MAG         PIC X(05) VALUE '00632'.        05500005
AN1589                                                                  05510005
AN1589     05  AN-SENDER-NAME-MAG       PIC X(15)                       05520005
AN1589                 VALUE 'ANTHEMSMAGLN834'.                         05530005
AN1589                                                                  05540005
AN1589     05  AN-RECEIVER-ID-MAG       PIC X(05) VALUE '00992'.        05550005
AN1589                                                                  05560005
AN1589     05  AN-RECEIVER-NAME-MAG     PIC X(15)                       05570005
AN1589                 VALUE 'MAGELLAN       '.                         05580005
AN0127                                                                  05590005
AN0127     05  AN-SENDER-ID-BCN         PIC X(05) VALUE '00633'.        05600005
AN0127                                                                  05610005
AN0127     05  AN-SENDER-NAME-BCN       PIC X(15)                       05620005
AN0127                 VALUE 'ANTHEMSBEACN834'.                         05630005
AN0127                                                                  05640005
AN0127     05  AN-RECEIVER-ID-BCN       PIC X(05) VALUE '00993'.        05650005
AN0127                                                                  05660005
AN0127     05  AN-RECEIVER-NAME-BCN     PIC X(15)                       05670005
AN0127                 VALUE 'BEACON         '.                         05680005
                                                                        05690005
AN1560                                                                  05700005
AN1560     05  BAD-WRITE-HRHNOAOR-MSG   PIC X(40) VALUE                 05710005
AN1560         'BAD WRITE OF HRHNOAOR'.                                 05720005
AN1560                                                                  05730005
AN1560     05  BAD-WRITE-HRHNOAOR-ABND  PIC 9(04) VALUE 1008.           05740005
AN1560                                                                  05750005
AN1560     05  ANR-SENDER-ID             PIC X(05) VALUE '00634'.       05760005
AN1560                                                                  05761005
AN1560     05  ANR-SENDER-NAME           PIC X(15)                      05762005
AN1560                 VALUE 'ANTHEMSAOPTR834'.                         05763005
AN1560                                                                  05764005
AN1560     05  ANR-RECEIVER-ID           PIC X(05) VALUE '00994'.       05765005
AN1560                                                                  05766005
AN1560     05  ANR-RECEIVER-NAME         PIC X(15)                      05767005
AN1560                 VALUE 'AOPTR          '.                         05768005
AN1560                                                                  05769005
AN1781                                                                  05769105
AN1781     05  BAD-WRITE-HRHNOAAR-MSG   PIC X(40) VALUE                 05769205
AN1781         'BAD WRITE OF HRHNOAAR'.                                 05769305
AN1781                                                                  05769405
AN1781     05  BAD-WRITE-HRHNOAAR-ABND  PIC 9(04) VALUE 1009.           05769505
AN1781                                                                  05769605
AN1781     05  AAR-SENDER-ID             PIC X(05) VALUE '00635'.       05769705
AN1781                                                                  05769805
AN1781     05  AAR-SENDER-NAME           PIC X(15)                      05769905
AN1781                 VALUE 'ANTHEMSAARCH834'.                         05770005
AN1781                                                                  05770105
AN1781     05  AAR-RECEIVER-ID           PIC X(05) VALUE '00995'.       05770205
AN1781                                                                  05770305
AN1781     05  AAR-RECEIVER-NAME         PIC X(15)                      05770405
AN1781                 VALUE 'AARCH          '.                         05770505
AN1781                                                                  05770605
           05  ABEND-PROGRAM            PIC X(08) VALUE 'WAASABND'.     05770705
NRS012     05  C-DATE-PROGRAM           PIC X(08) VALUE 'NASXDATE'.     05770805
NJ1348     05  C-BIT-TWIDDLER           PIC X(08) VALUE 'HRTP66BC'.     05770905
MA2010     05  W-HRHSMASN               PIC X(08) VALUE 'HRHSMASN'.     05771005
NJ0983     05  W-HRHSINVM               PIC X(08) VALUE 'HRHSINVM'.     05772005
NJ9032     05  W-HRHSEHNC               PIC X(08) VALUE 'HRHSEHNC'.     05773005
NJ9032                                                                  05774005
MD7684     05  W-HRHST5W2               PIC X(08) VALUE 'HRHST5W2'.     05775005
MD6619     05  C-ENROLL-PROGRAM         PIC X(08) VALUE 'HRHSID01'.     05776005
                                                                        05777005
NJ1348     05  W-BENEFIT-FLAG.                                          05778005
NJ1348          10  W-BENEFIT-FLAG-1      PIC X(01).                    05779005
NJ1348          10  W-BENEFIT-FLAG-2      PIC X(01).                    05780005
NJ1348          10  W-BENEFIT-FLAG-3      PIC X(01).                    05790005
NJ1348          10  W-BENEFIT-FLAG-4      PIC X(01).                    05800005
NJ1348          10  W-BENEFIT-FLAG-5      PIC X(01).                    05810005
NJ1348          10  W-BENEFIT-FLAG-6      PIC X(01).                    05820005
NJ1348          10  W-BENEFIT-FLAG-7      PIC X(01).                    05830005
NJ1348          10  W-BENEFIT-FLAG-8      PIC X(01).                    05840005
NJ1348          10  W-BENEFIT-FLAG-9      PIC X(01).                    05850005
NJ1348          10  W-BENEFIT-FLAG-10     PIC X(01).                    05860005
NJ1348          10  W-BENEFIT-FLAG-11     PIC X(01).                    05870005
NJ1348          10  W-BENEFIT-FLAG-12     PIC X(01).                    05880005
NJ1348          10  W-BENEFIT-FLAG-13     PIC X(01).                    05890005
NJ1348          10  W-BENEFIT-FLAG-14     PIC X(01).                    05900005
NJ1348          10  W-BENEFIT-FLAG-15     PIC X(01).                    05910005
NJ1348          10  W-BENEFIT-FLAG-16     PIC X(01).                    05920005
NJ1348          10  W-BENEFIT-FLAG-17     PIC X(01).                    05930005
NJ1348                                                                  05940005
NJ1348     05  THE-UNTWIDDLED-BITS        PIC X(01) VALUE LOW-VALUES.   05950005
NJ1348     05  THE-TWIDDLED-BITS.                                       05960005
NJ1348          10  BIT-1                    PIC X(01).                 05970005
NJ1348          10  BIT-2                    PIC X(01).                 05980005
NJ1348          10  BIT-3                    PIC X(01).                 05990005
NJ1348          10  BIT-4                    PIC X(01).                 06000005
NJ1348          10  BIT-5                    PIC X(01).                 06010005
NJ1348          10  BIT-6                    PIC X(01).                 06020005
NJ1348          10  BIT-7                    PIC X(01).                 06030005
NJ1348          10  BIT-8                    PIC X(01).                 06040005
NJ1348                                                                  06050005
NJ5584     05  W-BENEFIT-FLAG-401-560.                                  06060005
NJ5584          10  W1-BENEFIT-FLAG-1     PIC X(01).                    06070005
NJ5584          10  W1-BENEFIT-FLAG-2     PIC X(01).                    06080005
NJ5584          10  W1-BENEFIT-FLAG-3     PIC X(01).                    06090005
NJ5584          10  W1-BENEFIT-FLAG-4     PIC X(01).                    06100005
NJ5584          10  W1-BENEFIT-FLAG-5     PIC X(01).                    06110005
NJ5584          10  W1-BENEFIT-FLAG-6     PIC X(01).                    06120005
NJ5584          10  W1-BENEFIT-FLAG-7     PIC X(01).                    06130005
NJ5584          10  W1-BENEFIT-FLAG-8     PIC X(01).                    06140005
NJ5584          10  W1-BENEFIT-FLAG-9     PIC X(01).                    06150005
NJ5584          10  W1-BENEFIT-FLAG-10    PIC X(01).                    06160005
NJ5584          10  W1-BENEFIT-FLAG-11    PIC X(01).                    06170005
NJ5584          10  W1-BENEFIT-FLAG-12    PIC X(01).                    06180005
NJ5584          10  W1-BENEFIT-FLAG-13    PIC X(01).                    06190005
NJ5584          10  W1-BENEFIT-FLAG-14    PIC X(01).                    06200005
NJ5584          10  W1-BENEFIT-FLAG-15    PIC X(01).                    06210005
NJ5584          10  W1-BENEFIT-FLAG-16    PIC X(01).                    06220005
NJ5584          10  W1-BENEFIT-FLAG-17    PIC X(01).                    06230005
NJ5584          10  W1-BENEFIT-FLAG-18    PIC X(01).                    06240005
NJ5584          10  W1-BENEFIT-FLAG-19    PIC X(01).                    06250005
NJ5584          10  W1-BENEFIT-FLAG-20    PIC X(01).                    06260005
NJ5584                                                                  06270005
NJ5584     05  THE-UNTWIDDLED-BITS-W1     PIC X(01) VALUE LOW-VALUES.   06280005
NJ5584     05  THE-TWIDDLED-BITS-W1.                                    06290005
NJ5584          10  BIT-1-W1                 PIC X(01).                 06300005
NJ5584          10  BIT-2-W1                 PIC X(01).                 06310005
NJ5584          10  BIT-3-W1                 PIC X(01).                 06320005
NJ5584          10  BIT-4-W1                 PIC X(01).                 06330005
NJ5584          10  BIT-5-W1                 PIC X(01).                 06340005
NJ5584          10  BIT-6-W1                 PIC X(01).                 06350005
NJ5584          10  BIT-7-W1                 PIC X(01).                 06360005
NJ5584          10  BIT-8-W1                 PIC X(01).                 06370005
NJ5584                                                                  06380005
MA2010     05  W-PACK-CODE-NUMERIC       PIC 9(04).                     06390005
MI2155     05  W-PACK-CODE-BINARY        PIC S9(04) COMP.               06400005
MI2155     05  W-PACKAGE-CHR REDEFINES W-PACK-CODE-BINARY.              06410005
MI2155          10  FILLER                PIC X(01).                    06420005
MI2155          10  W-PACK-PICX           PIC X(01).                    06430005
                                                                        06440005
MA2010     05  W-PKG-BINARY                PIC S9(04) COMP VALUE ZERO.  06450005
MA2010     05  W-PKG-BINARY-X              REDEFINES W-PKG-BINARY.      06460005
MA2010         10 W-PKG-HIGH-ORDER         PIC X(01).                   06470005
MA2010         10 W-PKG-LOW-ORDER          PIC X(01).                   06480005
MA2010     05  W-PACKAGE-CODE-X            PIC 9(04).                   06490005
MA2010     05  W-PKG-CODE-CHAR             REDEFINES W-PACKAGE-CODE-X.  06500005
MA2010         10 W-PKG-HIGH-ORDER-X       PIC X(01).                   06510005
MA2010         10 W-PKG-CODE-X             PIC X(03).                   06520005
MD2689                                                                  06530005
MD2689     05  W-PEND-SSN                  PIC 9(09).                   06540005
MD2689                                                                  06550005
AN0609     05  W-HRHS4QTR               PIC X(08) VALUE 'HRHS4QTR'.     06560005
AN0609                                                                  06570005
AN0609     05  W-4QTR-RENEWAL-DATE      PIC X(8).                       06580005
MD7568     05  WS-MEM-BIRTH-DATE        PIC S9(04) COMP SYNC.           06590005
MD7568     05  WS-MEM-CONTROL-PLAN-CODE PIC 9(03).                      06600005
MD7568     05  WS-MEM-SUB-NO            PIC X(13).                      06610005
MD7568     05  WS-MEM-FIRST-NAME        PIC X(10).                      06620005
MD7568     05  WS-MEM-GROUP-NUMBER      PIC X(09).                      06630005
MD7568     05  WS-MEM-NUMBER            PIC 9(03).                      06640005
MD7568     05  WS-HOLD-SUB-PLAN         PIC 9(03).                      06650005
MD7568     05  NUMBER-OF-MBR-SEGS       PIC S9(04) COMP SYNC VALUE +0.  06660005
MD7568     05  MEMBER-SEGMENT-SEQ       PIC 9(03) VALUE ZERO.           06670005
MD7568                                                                  06680005
           05  C-SENDER-ID              PIC X(05) VALUE '00830'.        06690005
           05  C-SENDER-NAME            PIC X(15)                       06700005
                       VALUE 'HZNASCOWYETH780'.                         06710005
           05  C-RECEIVER-ID            PIC X(05) VALUE '00990'.        06720005
           05  C-RECEIVER-NAME          PIC X(15)                       06730005
                       VALUE 'CAREMARK       '.                         06740005
           05  C-ID-NAME                PIC X(30)                       06750005
                       VALUE 'WYETH0083001                  '.          06760005
AN4993     05  A-SENDER-ID              PIC X(05) VALUE '00630'.        06770005
AN4993     05  A-SENDER-NAME            PIC X(15)                       06780005
AN4993                 VALUE 'ANTHEMSACARE834'.                         06790005
AN4993     05  A-RECEIVER-ID            PIC X(05) VALUE '00990'.        06800005
AN4993     05  A-RECEIVER-NAME          PIC X(15)                       06810005
AN4993                 VALUE 'CAREMARK       '.                         06820005
MI1323     05  CHR-SENDER-ID              PIC X(05) VALUE '00860'.      06830005
MI1323     05  CHR-SENDER-NAME            PIC X(15)                     06840005
MI1323                 VALUE 'BCSM710MCARE710'.                         06850005
MI1323     05  CHR-RECEIVER-ID            PIC X(05) VALUE '00990'.      06860005
MI1323     05  CHR-RECEIVER-NAME          PIC X(15)                     06870005
MI1323                 VALUE 'CAREMARK       '.                         06880005
MI1323     05  CHR-ID-NAME                PIC X(30)                     06890005
MI1323                 VALUE 'CHRYSLER00860                 '.          06900005
NJ1164     05  NJ-SENDER-ID              PIC X(05) VALUE '00325'.       06910005
NJ1164     05  NJ-SENDER-NAME            PIC X(15)                      06920005
NJ1164                 VALUE 'NASCEAVNJCDH780'.                         06930005
NJ1164     05  NJ-RECEIVER-ID            PIC X(05) VALUE '00990'.       06940005
NJ1164     05  NJ-RECEIVER-NAME          PIC X(15)                      06950005
NJ1164                 VALUE 'CAREMARK       '.                         06960005
NJ1164     05  NJ-ID-NAME                PIC X(30)                      06970005
NJ1164                 VALUE 'HORIZON      0032503          '.          06980005
MA2010     05  CMK-SENDER-ID              PIC X(05) VALUE '00575'.      06990005
MA2010     05  CMK-SENDER-NAME            PIC X(15)                     07000005
MA2010                 VALUE 'BCBSMA0MACMK700'.                         07010005
MA2010     05  CMK-RECEIVER-ID            PIC X(05) VALUE '00990'.      07020005
MA2010     05  CMK-RECEIVER-NAME          PIC X(15)                     07030005
MA2010                 VALUE 'CAREMARK       '.                         07040005
MD2689     05  CCK-SENDER-ID              PIC X(05) VALUE '00720'.      07050005
MD2689     05  CCK-SENDER-NAME            PIC X(15)                     07060005
MD2689                 VALUE 'CAREFIRST      '.                         07070005
MD2689     05  CCK-RECEIVER-ID            PIC X(05) VALUE '00990'.      07080005
MD2689     05  CCK-RECEIVER-NAME          PIC X(15)                     07090005
MD2689                 VALUE 'CAREMARK       '.                         07100005
MD6619     05  CVS-SENDER-ID              PIC X(05) VALUE '00640'.      07110005
MD6619     05  CVS-SENDER-NAME            PIC X(15)                     07120005
MD6619                 VALUE 'CAREFIRSTMQ    '.                         07130005
MD6619     05  CVS-RECEIVER-ID            PIC X(05) VALUE '00990'.      07140005
MD6619     05  CVS-RECEIVER-NAME          PIC X(15)                     07150005
MD6619                 VALUE 'CAREMARK       '.                         07160005
MD6619     05  CVS-CAR-REC-ID-NAME        PIC X(30)                     07170005
MD6619                 VALUE 'CAREFIRSTNASCO                '.          07180005
AN8824     05  WS-GRP-BASE-LINCO          PIC X(9) VALUE '003321240'.   07190005
AN8824                                                                  07200005
AN8824     05  WS-GRP-BASE-NATION         PIC X(9) VALUE '003329930'.   07210005
AN8824                                                                  07220005
MI0730     05  WS-GRP-BASE-TKUSA          PIC X(9) VALUE '000071314'.   07230005
MI0730                                                                  07240005
           05  C-FOUR-ZEROS             PIC X(04) VALUE ZEROS.          07250005
                                                                        07260005
           05  C-BINARY-DATE            PIC X(02) VALUE '80'.           07270005
                                                                        07280005
           05  C-MMDDYYYY               PIC X(02) VALUE '12'.           07290005
                                                                        07300005
           05  C-MMDDYYYY               PIC X(02) VALUE '10'.           07310005
                                                                        07320005
           05  C-MMDDYY13               PIC X(02) VALUE '13'.           07330005
           05  C-YYYYMMDD               PIC X(02) VALUE '22'.           07340005
           05  C-YYMMDD                 PIC X(02) VALUE '20'.           07350005
           05  C-MMDDYYH                PIC X(02) VALUE '11'.           07360005
           05  C-CONVERT-CODE           PIC X(01) VALUE  '0'.           07370005
           05  IO-ACCESSOR              PIC X(08) VALUE 'HRHXIOAC'.     07380005
           05  MEMBERSHIP-ACCESSOR      PIC X(08) VALUE 'HRBXMIOC'.     07390005
                                                                        07400005
       01  MEMBERSHIP-ACCESSOR-WORKAREA.                                07410005
                                                                        07420005
       REPLACE ==(PP)== BY ==MEM==                                      07430005
               ==:L1:== BY ==05==                                       07440005
               ==:L2:== BY ==10==.                                      07450005
           COPY HRBYMIOC.                                               07460005
       REPLACE OFF.                                                     07470005
                                                                        07480005
           05  DATE-FORMAT-YYDDD        PIC X(02) VALUE '00'.           07490005
           05  DATE-FORMAT-YY-DDD       PIC X(02) VALUE '01'.           07500005
           05  DATE-FORMAT-YYYYDDD      PIC X(02) VALUE '05'.           07510005
           05  DATE-FORMAT-YYYY-DDD     PIC X(02) VALUE '06'.           07520005
           05  DATE-FORMAT-YYDDDS       PIC X(02) VALUE '07'.           07530005
           05  DATE-FORMAT-YYYYDDDS     PIC X(02) VALUE '08'.           07540005
           05  DATE-FORMAT-MMDDYY       PIC X(02) VALUE '10'.           07550005
           05  DATE-FORMAT-MM-DD-YY     PIC X(02) VALUE '11'.           07560005
           05  DATE-FORMAT-MMDDYYYY     PIC X(02) VALUE '12'.           07570005
           05  DATE-FORMAT-MM-DD-YYYY   PIC X(02) VALUE '13'.           07580005
           05  DATE-FORMAT-YYYY-MM-DDS  PIC X(02) VALUE '14'.           07590005
           05  DATE-FORMAT-YYYY-MM-DD   PIC X(02) VALUE '15'.           07600005
           05  DATE-FORMAT-DD-MM-YYYY   PIC X(02) VALUE '16'.           07610005
           05  DATE-FORMAT-YYMMDD       PIC X(02) VALUE '20'.           07620005
           05  DATE-FORMAT-YY-MM-DD     PIC X(02) VALUE '21'.           07630005
           05  DATE-FORMAT-YYYYMMDD     PIC X(02) VALUE '22'.           07640005
           05  DATE-FORMAT-FULL-DATE    PIC X(02) VALUE '30'.           07650005
           05  DATE-FORMAT-ABRV-DATE    PIC X(02) VALUE '31'.           07660005
           05  DATE-FORMAT-XXXDD-YY     PIC X(02) VALUE '32'.           07670005
           05  DATE-FORMAT-XXDDYY       PIC X(02) VALUE '33'.           07680005
           05  DATE-FORMAT-DDMMMYY      PIC X(02) VALUE '40'.           07690005
           05  DATE-FORMAT-MMMM         PIC X(02) VALUE '50'.           07700005
           05  DATE-FORMAT-MMMMDD       PIC X(02) VALUE '60'.           07710005
           05  DATE-FORMAT-YYYDDD       PIC X(02) VALUE '70'.           07720005
           05  DATE-FORMAT-YYYSDDS      PIC X(02) VALUE '71'.           07730005
           05  DATE-FORMAT-BINARY       PIC X(02) VALUE '80'.           07740005
                                                                        07750005
           05  DATE-FUNCTION-CONVERT    PIC X(01) VALUE '0'.            07760005
           05  DATE-FUNCTION-ADJUST     PIC X(01) VALUE '1'.            07770005
           05  DATE-FUNCTION-CALCULATE  PIC X(01) VALUE '2'.            07780005
           05  DATE-FUNCTION-DAY-OF-WK  PIC X(01) VALUE '3'.            07790005
           05  DATE-FUNCTION-NO-LEAP    PIC X(01) VALUE '4'.            07800005
           05  DATE-FUNCTION-SYSTEM     PIC X(01) VALUE '5'.            07810005
           05  HRHSCGT1                 PIC X(08) VALUE 'HRHSCGT1'.     07820005
                                                                        07830005
           COPY HQQYE01C.                                               07840005
           COPY HQQYE02C.                                               07850005
MD7568     COPY HQQYE03C.                                               07860005
           COPY HQQYG01C.                                               07870005
           COPY HQQYG02C.                                               07880005
                                                                        07890005
NJ0192/***************************************************************  07900005
NJ0192*    CURRENT-DATE FUNCTION DATA AREA                           *  07910005
NJ0192****************************************************************  07920005
NJ0192 01  W-CURRENT-DATE-AREA.                                         07930005
NJ0192 REPLACE ==:P:==  BY ==W-CURR==                                   07940005
NJ0192         ==:L1:== BY ==02==                                       07950005
NJ0192         ==:L2:== BY ==05==                                       07960005
NJ0192         ==:L3:== BY ==10==.                                      07970005
NJ0192     COPY HQCYDATC.                                               07980005
NJ0192 REPLACE OFF.                                                     07990005
NJ0192                                                                  08000005
      /**************************************************************** 08010005
      *          SWITCHES                                             * 08020005
      ***************************************************************** 08030005
                                                                        08040005
       01 SWITCHES.                                                     08050005
                                                                        08060005
           05  EOF-INPUT-STUB-FILE       PIC X(01) VALUE LOW-VALUES.    08070005
               88  END-OF-INPUT-STUB-FILE          VALUE HIGH-VALUES.   08080005
               88  NOT-END-OF-INPUT-STUB-FILE      VALUE LOW-VALUES.    08090005
           05  EOF-INPUT-FLUSH-FILE       PIC X(01) VALUE LOW-VALUES.   08100005
               88  END-OF-INPUT-FLUSH-FILE          VALUE HIGH-VALUES.  08110005
               88  NOT-END-OF-INPUT-FLUSH-FILE      VALUE LOW-VALUES.   08120005
                                                                        08130005
           05  W-ALREADY-SENT           PIC X(01) VALUE 'N'.            08140005
               88  ALREADY-SENT                   VALUE 'Y'.            08150005
               88  NOT-ALREADY-SENT               VALUE 'N'.            08160005
                                                                        08170005
MI2155     05  MAPPING-SWITCH           PIC X(01) VALUE 'N'.            08180005
MI2155         88  MAPPING-FOUND                  VALUE 'Y'.            08190005
MI2155         88  MAPPING-NOT-FOUND              VALUE 'N'.            08200005
MI2155                                                                  08210005
MI2155     05  SW-CHANGE-OF-KEY         PIC X(01) VALUE 'N'.            08220005
MI2155         88 CHANGE-OF-KEY                   VALUE 'Y'.            08230005
MI2155         88 NOT-CHANGE-OF-KEY               VALUE 'N'.            08240005
MI2155                                                                  08250005
MI2155                                                                  08260005
MI2155     05  SW-CLINT-ID              PIC X(01) VALUE 'Y'.            08270005
MI2155         88 CLINT-ID-FND                    VALUE 'N'.            08280005
MI2155         88 NOT-CLINT-ID-FND                VALUE 'Y'.            08290005
MI2155                                                                  08300005
MD838A     05  CLM-INCL-EXCL            PIC X(02) VALUE '01'.           08301007
MD838A         88 CLAIM-INCLUDE                   VALUE '01'.           08302007
MD838A         88 CLAIM-EXCLUDE                   VALUE '00'.           08303007
MD838A                                                                  08304007
MD6619     05  WS-SUBSCRIBER-PLAN             PIC 9(03).                08310005
MD6619     05  WS-SUBSCRIBER-PLAN-X  REDEFINES WS-SUBSCRIBER-PLAN       08320005
MD6619                                        PIC X(03).                08330005
MI2155                                                                  08340005
MI2155     05  W-HRBNGENO-STATUS              PIC X(02).                08350005
MI2155         88  W-HRBNGENO-GOOD-OPEN       VALUES '00' '97'.         08360005
MI2155         88  W-HRBNGENO-GOOD-READ       VALUES '00' '10'.         08370005
MI2155         88  W-HRBNGENO-REC-NOT-FND     VALUE  '23'.              08380005
MI2155         88  W-HRBNGENO-INV-NEXT-REC    VALUE  '46' '92'.         08390005
MI2155         88  W-HRBNGENO-GOOD-CLOSE      VALUES '00'.              08400005
MI2155                                                                  08410005
           05  W-FILE-STATUS        PIC X(02) VALUE ZERO.               08420005
               88  W-GOOD-OPEN                VALUES '00' '97'.         08430005
               88  W-GOOD-READ                VALUES '00' '10'.         08440005
               88  W-GOOD-WRITE               VALUE  '00'.              08450005
               88  W-GOOD-CLOSE               VALUE  '00'.              08460005
               88  W-REC-NOT-FND              VALUE  '23'.              08470005
                                                                        08480005
           05  SW-FIRST-TIME               PIC X(01) VALUE LOW-VALUES.  08490005
               88 FIRST-TIME               VALUE LOW-VALUES.            08500005
               88 NOT-FIRST-TIME           VALUE HIGH-VALUES.           08510005
AN4993                                                                  08520005
AN4993     05  SW-FIRST-TIME-IN-PROGRAM    PIC X(01) VALUE LOW-VALUES.  08530005
AN4993         88 FIRST-TIME-IN-PROGRAM-Y  VALUE LOW-VALUES.            08540005
AN4993         88 FIRST-TIME-IN-PROGRAM-N  VALUE HIGH-VALUES.           08550005
AN4993                                                                  08560005
           05  SW-DID-BENEFIT-MATCH           PIC X(01).                08570005
               88 SW-BENEFIT-MATCHED              VALUE 'Y'.            08580005
               88 SW-BENEFIT-DID-NOT-MATCH        VALUE 'N'.            08590005
AN1887                                                                  08600005
AN1887     05  SW-AOPTM-TRAILER            PIC X(01) VALUE LOW-VALUES.  08610005
AN1887         88 AOPTM-TRAILER-NO         VALUE LOW-VALUES.            08620005
AN1887         88 AOPTM-TRAILER-YES        VALUE HIGH-VALUES.           08630005
AN1560                                                                  08640005
AN1560     05  SW-AOPTR-TRAILER            PIC X(01) VALUE LOW-VALUES.  08650005
AN1560         88 AOPTR-TRAILER-NO         VALUE LOW-VALUES.            08660005
AN1560         88 AOPTR-TRAILER-YES        VALUE HIGH-VALUES.           08670005
AN1781                                                                  08680005
AN1781     05  SW-AARCH-TRAILER            PIC X(01) VALUE LOW-VALUES.  08690005
AN1781         88 AARCH-TRAILER-NO         VALUE LOW-VALUES.            08700005
AN1781         88 AARCH-TRAILER-YES        VALUE HIGH-VALUES.           08710005
AN1589                                                                  08720005
AN1589     05  SW-MAGLN-TRAILER            PIC X(01) VALUE LOW-VALUES.  08730005
AN1589         88 MAGLN-TRAILER-NO         VALUE LOW-VALUES.            08740005
AN1589         88 MAGLN-TRAILER-YES        VALUE HIGH-VALUES.           08750005
AN0127                                                                  08760005
AN0127     05  SW-BEACN-TRAILER            PIC X(01) VALUE LOW-VALUES.  08770005
AN0127         88 BEACN-TRAILER-NO         VALUE LOW-VALUES.            08780005
AN0127         88 BEACN-TRAILER-YES        VALUE HIGH-VALUES.           08790005
                                                                        08800005
           05  SW-W-ICN-MATCH              PIC X(01) VALUE 'N'.         08810005
ANI612         88 SW-W-ICN-MATCH-NO        VALUE 'N'.                   08820005
ANI612         88 SW-W-ICN-MATCH-YES       VALUE 'Y'.                   08830005
                                                                        08840005
           05  SW-PAID                     PIC X(01) VALUE 'N'.         08850005
ANI612         88 SW-PAID-NO               VALUE 'N'.                   08860005
ANI612         88 SW-PAID-YES              VALUE 'Y'.                   08870005
                                                                        08880005
AN4993     05  SW-WYETH-TRAILER            PIC X(01) VALUE LOW-VALUES.  08890005
AN4993         88 WYETH-TRAILER-NO         VALUE LOW-VALUES.            08900005
AN4993         88 WYETH-TRAILER-YES        VALUE HIGH-VALUES.           08910005
AN4993                                                                  08920005
AN4993     05  SW-ACARE-TRAILER            PIC X(01) VALUE LOW-VALUES.  08930005
AN4993         88 ACARE-TRAILER-NO         VALUE LOW-VALUES.            08940005
AN4993         88 ACARE-TRAILER-YES        VALUE HIGH-VALUES.           08950005
AN4993                                                                  08960005
MI1323     05  SW-MCARE-TRAILER             PIC X(01) VALUE LOW-VALUES. 08970005
MI1323         88 MCARE-TRAILER-NO            VALUE LOW-VALUES.         08980005
MI1323         88 MCARE-TRAILER-YES           VALUE HIGH-VALUES.        08990005
MI1323                                                                  09000005
NJ1164     05  SW-NJCDH-TRAILER             PIC X(01) VALUE LOW-VALUES. 09010005
NJ1164         88 NJCDH-TRAILER-NO            VALUE LOW-VALUES.         09020005
NJ1164         88 NJCDH-TRAILER-YES           VALUE HIGH-VALUES.        09030005
NJ1164                                                                  09040005
MA2010     05  SW-MACMK-TRAILER             PIC X(01) VALUE LOW-VALUES. 09050005
MA2010         88 MACMK-TRAILER-NO            VALUE LOW-VALUES.         09060005
MA2010         88 MACMK-TRAILER-YES           VALUE HIGH-VALUES.        09070005
MA2010                                                                  09080005
MD2689     05  SW-CCARE-TRAILER             PIC X(01) VALUE LOW-VALUES. 09090005
MD2689         88 CCARE-TRAILER-NO            VALUE LOW-VALUES.         09100005
MD2689         88 CCARE-TRAILER-YES           VALUE HIGH-VALUES.        09110005
MD2689                                                                  09120005
MD6619     05  SW-CVSCM-TRAILER             PIC X(01) VALUE LOW-VALUES. 09130005
MD6619         88 CVSCM-TRAILER-NO            VALUE LOW-VALUES.         09140005
MD6619         88 CVSCM-TRAILER-YES           VALUE HIGH-VALUES.        09150005
MD6619                                                                  09160005
           05  EOF-INPUT                PIC X(01) VALUE 'N'.            09170005
               88  END-OF-INPUT                   VALUE 'Y'.            09180005
               88  NOT-END-OF-INPUT               VALUE 'N'.            09190005
                                                                        09200005
           05  W-WRITE                  PIC X(01) VALUE '3'.            09210005
               88  OP-WRITE                       VALUE '1'.            09220005
               88  OP-REWRITE                     VALUE '2'.            09230005
               88  DO-NOT-WRITE                   VALUE '3'.            09240005
                                                                        09250005
           05  FILLER                   PIC X(01) VALUE LOW-VALUES.     09260005
               88  STOP-LOOP                      VALUE HIGH-VALUES.    09270005
               88  NO-STOP-LOOP                   VALUE LOW-VALUES.     09280005
                                                                        09290005
           05  FILLER                   PIC X(01) VALUE 'N'.            09300005
               88  CLAIM-FND                      VALUE 'Y'.            09310005
               88  CLAIM-NOT-FND                  VALUE 'N'.            09320005
                                                                        09330005
           05  MEMBERSHIP-STATUS        PIC X(01) VALUE LOW-VALUES.     09340005
               88  MEMBERSHIP-ERROR               VALUE HIGH-VALUES.    09350005
               88  NO-MEMBERSHIP-ERROR            VALUE LOW-VALUES.     09360005
                                                                        09370005
                                                                        09380005
MD7568     05  SW-ELIG-ERROR            PIC X(01) VALUE LOW-VALUES.     09390005
MD7568         88  ELIG-ERRORS                    VALUE HIGH-VALUES.    09400005
MD7568         88  NO-ELIG-ERRORS                 VALUE LOW-VALUES.     09410005
                                                                        09420005
                                                                        09430005
           05  COV-SEGMENT-MATCH-SWITCH PIC X(01) VALUE 'N'.            09440005
               88  COV-SEGMENT-MATCH              VALUE 'Y'.            09450005
               88  NO-COV-SEGMENT-MATCH           VALUE 'N'.            09460005
                                                                        09470005
MD7684     05  SW-T5W2K-MATCH           PIC X(01) VALUE LOW-VALUES.     09480005
MD7684         88  SW-T5W2K-MATCH-FOUND           VALUE HIGH-VALUES.    09490005
MD7684         88  SW-T5W2K-MATCH-NOT-FOUND       VALUE LOW-VALUES.     09500005
                                                                        09510005
AN0609     05  SW-MATCH-FOUND           PIC X(01) VALUE 'N'.            09520005
AN0609         88  SW-MATCH-FOUND-Y               VALUE 'Y'.            09530005
AN0609         88  SW-MATCH-FOUND-N               VALUE 'N'.            09540005
AN0609                                                                  09550005
AN0609     05  SW-COMINGLE-NT-L-O       PIC X(01) VALUE 'N'.            09560005
AN0609         88  SW-COMINGLE-NT-L-O-Y           VALUE 'Y'.            09570005
AN0609         88  SW-COMINGLE-NT-L-O-N           VALUE 'N'.            09580005
MA1368                                                                  09590005
MA1368     05  S-CLAIM-ZERO-DOL         PIC X(01) VALUE 'N'.            09600005
MA1368         88  S-CLAIM-ZERO-DOL-Y             VALUE 'Y'.            09610005
MA1368         88  S-CLAIM-ZERO-DOL-N             VALUE 'N'.            09620005
MA1368                                                                  09630005
MA1368     05  S-ZERO-DOL-HAD-DOL       PIC X(01) VALUE 'N'.            09640005
MA1368         88  S-ZERO-DOL-HAD-DOL-Y           VALUE 'Y'.            09650005
MA1368         88  S-ZERO-DOL-HAD-DOL-N           VALUE 'N'.            09660005
      /**************************************************************** 09670005
      *          WORK AREAS                                           * 09680005
      ***************************************************************** 09690005
                                                                        09700005
NR0007 REPLACE ==:P:==  BY ==W-SPGMPARM-==.                             09710005
NR0007     COPY HRHYNOFM.                                               09720005
NR0007 REPLACE OFF.                                                     09730005
NR0007                                                                  09740005
NR0007 REPLACE ==:P:==  BY ==OSM-==.                                    09750005
NR0007     COPY HRHYAO3X.                                               09760005
NR0007 REPLACE OFF.                                                     09770005
NR0007                                                                  09780005
       01  WORK-AREAS.                                                  09790005
                                                                        09800005
MI2155     05  WS-CHR-ID-NAME OCCURS 6 TIMES INDEXED BY CHR-INDEX.      09810005
MI2155         10 WS-CHR-ID-NAME-1        PIC X(30).                    09820005
AN0609                                                                  09830005
AN0609     05  WS-FST-SVC-DATE-YYYYMMDD   PIC X(08).                    09840005
AN0609                                                                  09850005
           05  W-SUB-PLAN-CODE            PIC S9(04) COMP.              09860005
           05  W-SUBPROGRAM               PIC X(8).                     09870005
           05  W-DATE-YYYYMMDD.                                         09880005
               10  W-DATE-CC              PIC X(02).                    09890005
               10  W-DATE-YY              PIC X(02).                    09900005
               10  W-DATE-MM              PIC X(02).                    09910005
               10  W-DATE-DD              PIC X(02).                    09920005
                                                                        09930005
           05  W-DATE-CYYMMDD.                                          09940005
               10  W-DT-C                 PIC X(01).                    09950005
               10  W-DT-YY                PIC X(02).                    09960005
               10  W-DT-MM                PIC X(02).                    09970005
               10  W-DT-DD                PIC X(02).                    09980005
           05  W-STB-DATE               PIC X(10).                      09990005
           05  CD-DATE                  PIC  X(21).                     10000005
           05  FILLER REDEFINES CD-DATE.                                10010005
               10  CD-CYMD              PIC  9(8).                      10020005
               10  FILLER               PIC  X(13).                     10030005
                                                                        10040005
           05  CD-CYMD-X                PIC  X(08).                     10050005
                                                                        10060005
           05  W-IDX-HOLD               PIC  9(4).                      10070005
                                                                        10080005
NJUE03     05  W-HOLD-LINE              PIC  9(04).                     10090005
                                                                        10100005
NJUE03     05  W-RELATIVE-LINE-NO       PIC  9(03).                     10110005
                                                                        10120005
           05  W-COPAYMENT-AMT   PIC S9(7)V99 COMP-3 VALUE +0.          10130005
           05  W-DED-AMT-DOLLAR  PIC S9(7)V99 COMP-3 VALUE +0.          10140005
           05  W-LIFETIME-MAX    PIC S9(7)V99 COMP-3 VALUE +0.          10150005
           05  W-COINSURANCE     PIC S9(7)V99 COMP-3 VALUE +0.          10160005
           05  W-SERV-UNITS      PIC S9(7)V99 COMP-3 VALUE +0.          10170005
           05  W-OUT-OF-POCKET   PIC S9(7)V99 COMP-3 VALUE +0.          10180005
           05  W-ALL-COMINGLE    PIC S9(7)V99 COMP-3 VALUE +0.          10190005
                                                                        10200005
           05  W-BINARY                   PIC S9(04) COMP VALUE +0.     10210005
           05  FILLER REDEFINES W-BINARY.                               10220005
               10 W-HIGH-ORDER            PIC X(01).                    10230005
               10 W-LOW-ORDER             PIC X(01).                    10240005
           05  HOLD-MEMBER-ID.                                          10250005
               10  W-HOLD-SUB-NO-BASE         PIC  X(13).               10260005
               10  W-HOLD-MBR-NO              PIC  X(05).               10270005
                                                                        10280005
                                                                        10290005
MI2155     05  WS-PLAN-CODE             PIC  X(03).                     10300005
MI2155     05  WS-VENDOR-CODE           PIC  X(05).                     10310005
MI2155     05  WS-GROUP-BASE-NO         PIC  X(09).                     10320005
MI2155     05  WS-GROUP-SECTION-NO      PIC  X(04).                     10330005
MI2155     05  WS-PACKAGE-CODE          PIC  9(03).                     10340005
MI2155     05  WS-FIELD-NAME            PIC  X(08).                     10350005
MI2155     05  WS-FIELD-VALUE           PIC  X(25).                     10360005
                                                                        10370005
           05  W-YYMMDD                   PIC X(06).                    10380005
           05  W-HHMMSS                   PIC 9(08).                    10390005
           05  W-HHMMSS-R REDEFINES W-HHMMSS.                           10400005
               10  W-HHMMSS-REDEF         PIC 9(06).                    10410005
               10  W-HHMMSS-FILLER        PIC 9(02).                    10420005
           05  W-TIME-HOLD.                                             10430005
               10  W-HHMMSS-X             PIC 9(06).                    10440005
               10  W-FILLER               PIC 9(04) VALUE ZEROS.        10450005
                                                                        10460005
           05  TOT-ADJ-AMOUNT           PIC  S9(05)V9(02) VALUE ZEROS.  10470005
                                                                        10480005
           05  GRAND-TOT-ADJ-AMOUNT                                     10490005
                                        PIC  S9(07)V9(02) VALUE ZEROS.  10500005
                                                                        10510005
           05  GRAND-TOT-ADJ-AMOUNT-X                                   10520005
                                        PIC -9(7).9(2).                 10530005
           05  WS-FST-SVC-DATE           PIC X(08).                     10540005
           05  W-IN-GROUP-SECTION-NUMBER  PIC  9(04).                   10550005
           05  W-IN-PACKAGE-CODE          PIC  9(04).                   10560005
           05  FILLER REDEFINES W-IN-PACKAGE-CODE.                      10570005
               10 FILLER                  PIC X(01).                    10580005
               10 W-IN-PACKAGE-CODEX      PIC X(03).                    10590005
           05  W-IN-CLIENT-NUMBER         PIC  9(09).                   10600005
           05  W-ABEND-CODE               PIC S9(09) BINARY VALUE +0.   10610005
               88  W-BAD-PEND-OPEN-CODE       VALUE  +2000.             10620005
               88  W-BAD-PEND-READ-CODE       VALUE  +2001.             10630005
               88  W-BAD-PEND-CLOSE-CODE      VALUE  +2002.             10640005
NRS012         88  W-BAD-NASXDATE-ABND        VALUE  +2003.             10650005
               88  W-BAD-STAT9-OPEN-CODE      VALUE  +2004.             10660005
               88  W-BAD-WRITE-CODE           VALUE  +2005.             10670005
               88  W-BAD-STAT9-READ-CODE      VALUE  +2006.             10680005
               88  W-BAD-STAT9-CLOSE-CODE     VALUE  +2007.             10690005
                                                                        10700005
           05  WORK-PLAN-CODE             PIC 9(03) VALUE ZERO.         10710005
           05  WORK-PLAN-CODE-CHR REDEFINES WORK-PLAN-CODE              10720005
                                          PIC X(03).                    10730005
           05  WK-EFF-DATE.                                             10740005
               10  WK-EFF-YEAR            PIC  9(04).                   10750005
               10  WK-EFF-MONTH           PIC  9(02).                   10760005
               10  WK-EFF-DAY             PIC  9(02).                   10770005
           05  W-EFF-DATE                 PIC S9(04) BINARY SYNC        10780005
                                                     VALUE ZERO.        10790005
           05  COVERAGE-SEGMENT-SEQ       PIC 9(03)  VALUE ZERO.        10800005
           05  NUMBER-OF-COV-SEGMENTS     PIC S9(04) COMP SYNC          10810005
                                                     VALUE ZERO.        10820005
           05  W-PBM-PKCD                 PIC S9(04) COMP.              10830005
           05  FILLER REDEFINES W-PBM-PKCD.                             10840005
               10  FILLER                     PIC X(01).                10850005
               10  W-PBM-PKCD-PICX            PIC X(01).                10860005
           05  W-TESTING        PIC 9(09) PACKED-DECIMAL.               10870005
           05  W-OOP                      PIC S9(07)V99 PACKED-DECIMAL  10880005
                                                     VALUE +0.          10890005
           05  W-DEDUCT                   PIC S9(07)V99 PACKED-DECIMAL  10900005
                                                     VALUE +0.          10910005
                                                                        10920005
           05  W-BIN-DT                   PIC S9(04) BINARY.            10930005
           05  W-CH-DT  REDEFINES   W-BIN-DT.                           10940005
               10  W-CHAR-BIN-DT              PIC  X(02).               10950005
                                                                        10960005
MA1368     05  W-HOLD-CLAIM-CTL-NO        PIC 9(15).                    10970005
                                                                        10980005
           05  W-WRAP-KEY.                                              10990005
               10 W-ICN                   PIC 9(14).                    11000005
               10 W-ACT-PLAN              PIC 9(03).                    11010005
                                                                        11020005
AN1308     05  WS-ACTIV-PLAN-CODE         PIC S9(03).                   11030005
AN1308                                                                  11040005
NR0099     05  WS-ACTIV-PLAN-CODE-NEW     PIC 9(03).                    11050005
NR0099                                                                  11060005
           05  W-PURG-ICN                 PIC 9(15).                    11070005
           05  W-PURG-ICN-X REDEFINES W-PURG-ICN                        11071005
                                          PIC X(15).                    11072005
                                                                        11073005
           05  W-SEX-REL                  PIC X(01).                    11074005
           05  W-MID                      PIC X(01).                    11075005
           05  W-FIRST-NAME               PIC X(25).                    11076005
                                                                        11077005
NJ7557     05  W-HOLD-SUFF-INFO.                                        11078005
NJ7557         12  W-HOLD-SUFF-GRP-BASE          PIC X(9).              11079005
NJ7557         12  W-HOLD-SUFF-SECTION           PIC X(4).              11080005
NJ7557         12  W-HOLD-SUFF-PKG               PIC 9(3).              11090005
NJ7557         12  W-HOLD-SUFF-SUBID             PIC X(13).             11100005
                                                                        11110005
MA1079     05  W-HOLD-SUFF-INFO-MACMK.                                  11120005
MA1079         12  W-HLD-SUFF-GRP-BASE-M         PIC X(9).              11130005
MA1079         12  W-HLD-SUFF-SECTION-M          PIC X(4).              11140005
MA1079         12  W-HLD-SUFF-PKG-M              PIC X(3).              11150005
MA1079         12  W-HLD-SUFF-SUBID-M            PIC X(13).             11160005
MA1079         12  W-HLD-SUFF-EMP-DPT-M          PIC X(9).              11170005
MA1079                                                                  11180005
           05  W-ERROR-MESSAGE            PIC X(30) VALUE SPACES.       11190005
               88  W-NO-STAT9-CLAIM        VALUE                        11200005
                   'NO MATCHING STAT 9 ON WRAP   '.                     11210005
               88  W-NO-FLUSH-CLAIM        VALUE                        11220005
                   'NO MATCHING FLUSH CLAIM      '.                     11230005
NJ9032                                                                  11240005
NJ9032     05  HOLD-RENEWAL-DATE          PIC X(8).                     11250005
NJ9032                                                                  11260005
NJ476C     05  W-HRHSTPOS               PIC X(08) VALUE 'HRHSTPOS'.     11270005
TR354I     05  W-GENO-POS               PIC X(01).                      11280006
           COPY HRHYXSC1.                                               11290005
                                                                        11300005
******** ************************************************************** 11310005
******** *         D A T E  P R O G R A M  W O R K  A R E A             11320005
******** ************************************************************** 11330005
                                                                        11340005
NRS012*    COPY Y2KYDATE.                                               11350005
NRS012                                                                  11360005
NRS012************************************************************      11370005
NRS012* NASXDATE WORKING STORAGE / LINKAGE AREAS                 *      11380005
NRS012************************************************************      11390005
NRS012     REPLACE    ==:L1:==   BY  ==11==                             11400005
NRS012                ==:L2:==   BY  ==12==                             11410005
NRS012                ==:L3:==   BY  ==13==                             11420005
NRS012                ==:L4:==   BY  ==14==                             11430005
NRS012                ==:L5:==   BY  ==15==                             11440005
NRS012                ==:L6:==   BY  ==16==                             11450005
NRS012                ==:L7:==   BY  ==17==                             11460005
NRS012                ==:L8:==   BY  ==18==                             11470005
NRS012                ==:L9:==   BY  ==19==                             11480005
NRS012                ==:P:==    BY  ==W-==                             11490005
NRS012                ==:S:==    BY  ====.                              11500005
NRS012 01 :P:ENTIRE-DATE-AREA.                                          11510005
NRS012     COPY  NASYDATE.                                              11520005
NRS012     REPLACE OFF.                                                 11530005
NRS012                                                                  11540005
      ******************************************************************11550005
      *                 B  I  T    T A B L E S   (SEX-REL)            **11560005
      ******************************************************************11570005
                                                                        11580005
       01  BIT-TEST-TABLE.                                              11590005
         05  FILLER                PIC X(10)   VALUE '0000000000'.      11600005
         05  FILLER                PIC X(10)   VALUE '0100000001'.      11610005
         05  FILLER                PIC X(10)   VALUE '0200000010'.      11620005
         05  FILLER                PIC X(10)   VALUE '0300000011'.      11630005
         05  FILLER                PIC X(10)   VALUE '0400000100'.      11640005
         05  FILLER                PIC X(10)   VALUE '0500000101'.      11650005
         05  FILLER                PIC X(10)   VALUE '0600000110'.      11660005
         05  FILLER                PIC X(10)   VALUE '0700000111'.      11670005
         05  FILLER                PIC X(10)   VALUE '0800001000'.      11680005
         05  FILLER                PIC X(10)   VALUE '0900001001'.      11690005
         05  FILLER                PIC X(10)   VALUE '0A00001010'.      11700005
         05  FILLER                PIC X(10)   VALUE '0B00001011'.      11710005
         05  FILLER                PIC X(10)   VALUE '0C00001100'.      11720005
         05  FILLER                PIC X(10)   VALUE '0D00001101'.      11730005
         05  FILLER                PIC X(10)   VALUE '0E00001110'.      11740005
         05  FILLER                PIC X(10)   VALUE '0F00001111'.      11750005
         05  FILLER                PIC X(10)   VALUE '1000010000'.      11760005
         05  FILLER                PIC X(10)   VALUE '1100010001'.      11770005
         05  FILLER                PIC X(10)   VALUE '1200010010'.      11780005
         05  FILLER                PIC X(10)   VALUE '1300010011'.      11790005
         05  FILLER                PIC X(10)   VALUE '1400010100'.      11800005
         05  FILLER                PIC X(10)   VALUE '1500010101'.      11810005
         05  FILLER                PIC X(10)   VALUE '1600010110'.      11820005
         05  FILLER                PIC X(10)   VALUE '1700010111'.      11830005
         05  FILLER                PIC X(10)   VALUE '1800011000'.      11840005
         05  FILLER                PIC X(10)   VALUE '1900011001'.      11850005
         05  FILLER                PIC X(10)   VALUE '1A00011010'.      11860005
         05  FILLER                PIC X(10)   VALUE '1B00011011'.      11870005
         05  FILLER                PIC X(10)   VALUE '1C00011100'.      11880005
         05  FILLER                PIC X(10)   VALUE '1D00011101'.      11890005
         05  FILLER                PIC X(10)   VALUE '1E00011110'.      11900005
         05  FILLER                PIC X(10)   VALUE '1F00011111'.      11910005
         05  FILLER                PIC X(10)   VALUE '2000100000'.      11920005
         05  FILLER                PIC X(10)   VALUE '2100100001'.      11930005
         05  FILLER                PIC X(10)   VALUE '2200100010'.      11940005
         05  FILLER                PIC X(10)   VALUE '2300100011'.      11950005
         05  FILLER                PIC X(10)   VALUE '2400100100'.      11960005
         05  FILLER                PIC X(10)   VALUE '2500100101'.      11970005
         05  FILLER                PIC X(10)   VALUE '2600100110'.      11980005
         05  FILLER                PIC X(10)   VALUE '2700100111'.      11990005
         05  FILLER                PIC X(10)   VALUE '2800101000'.      12000005
         05  FILLER                PIC X(10)   VALUE '2900101001'.      12010005
         05  FILLER                PIC X(10)   VALUE '2A00101010'.      12020005
         05  FILLER                PIC X(10)   VALUE '2B00101011'.      12030005
         05  FILLER                PIC X(10)   VALUE '2C00101100'.      12040005
         05  FILLER                PIC X(10)   VALUE '2D00101101'.      12050005
         05  FILLER                PIC X(10)   VALUE '2E00101110'.      12060005
         05  FILLER                PIC X(10)   VALUE '2F00101111'.      12070005
         05  FILLER                PIC X(10)   VALUE '3000110000'.      12080005
         05  FILLER                PIC X(10)   VALUE '3100110001'.      12090005
         05  FILLER                PIC X(10)   VALUE '3200110010'.      12100005
         05  FILLER                PIC X(10)   VALUE '3300110011'.      12110005
         05  FILLER                PIC X(10)   VALUE '3400110100'.      12120005
         05  FILLER                PIC X(10)   VALUE '3500110101'.      12130005
         05  FILLER                PIC X(10)   VALUE '3600110110'.      12140005
         05  FILLER                PIC X(10)   VALUE '3700110111'.      12150005
         05  FILLER                PIC X(10)   VALUE '3800111000'.      12160005
         05  FILLER                PIC X(10)   VALUE '3900111001'.      12170005
         05  FILLER                PIC X(10)   VALUE '3A00111010'.      12180005
         05  FILLER                PIC X(10)   VALUE '3B00111011'.      12190005
         05  FILLER                PIC X(10)   VALUE '3C00111100'.      12200005
         05  FILLER                PIC X(10)   VALUE '3D00111101'.      12210005
         05  FILLER                PIC X(10)   VALUE '3E00111110'.      12220005
         05  FILLER                PIC X(10)   VALUE '3F00111111'.      12230005
         05  FILLER                PIC X(10)   VALUE '4001000000'.      12240005
         05  FILLER                PIC X(10)   VALUE '4101000001'.      12250005
         05  FILLER                PIC X(10)   VALUE '4201000010'.      12260005
         05  FILLER                PIC X(10)   VALUE '4301000011'.      12270005
         05  FILLER                PIC X(10)   VALUE '4401000100'.      12280005
         05  FILLER                PIC X(10)   VALUE '4501000101'.      12290005
         05  FILLER                PIC X(10)   VALUE '4601000110'.      12300005
         05  FILLER                PIC X(10)   VALUE '4701000111'.      12310005
           EJECT                                                        12320005
         05  FILLER                PIC X(10)   VALUE '4801001000'.      12330005
         05  FILLER                PIC X(10)   VALUE '4901001001'.      12340005
         05  FILLER                PIC X(10)   VALUE '4A01001010'.      12350005
         05  FILLER                PIC X(10)   VALUE '4B01001011'.      12360005
         05  FILLER                PIC X(10)   VALUE '4C01001100'.      12370005
         05  FILLER                PIC X(10)   VALUE '4D01001101'.      12380005
         05  FILLER                PIC X(10)   VALUE '4E01001110'.      12390005
         05  FILLER                PIC X(10)   VALUE '4F01001111'.      12400005
         05  FILLER                PIC X(10)   VALUE '5001010000'.      12410005
         05  FILLER                PIC X(10)   VALUE '5101010001'.      12420005
         05  FILLER                PIC X(10)   VALUE '5201010010'.      12430005
         05  FILLER                PIC X(10)   VALUE '5301010011'.      12440005
         05  FILLER                PIC X(10)   VALUE '5401010100'.      12450005
         05  FILLER                PIC X(10)   VALUE '5501010101'.      12460005
         05  FILLER                PIC X(10)   VALUE '5601010110'.      12470005
         05  FILLER                PIC X(10)   VALUE '5701010111'.      12480005
         05  FILLER                PIC X(10)   VALUE '5801011000'.      12490005
         05  FILLER                PIC X(10)   VALUE '5901011001'.      12500005
         05  FILLER                PIC X(10)   VALUE '5A01011010'.      12510005
         05  FILLER                PIC X(10)   VALUE '5B01011011'.      12520005
         05  FILLER                PIC X(10)   VALUE '5C01011100'.      12530005
         05  FILLER                PIC X(10)   VALUE '5D01011101'.      12540005
         05  FILLER                PIC X(10)   VALUE '5E01011110'.      12550005
         05  FILLER                PIC X(10)   VALUE '5F01011111'.      12560005
         05  FILLER                PIC X(10)   VALUE '6001100000'.      12570005
         05  FILLER                PIC X(10)   VALUE '6101100001'.      12580005
         05  FILLER                PIC X(10)   VALUE '6201100010'.      12590005
         05  FILLER                PIC X(10)   VALUE '6301100011'.      12600005
         05  FILLER                PIC X(10)   VALUE '6401100100'.      12610005
         05  FILLER                PIC X(10)   VALUE '6501100101'.      12620005
         05  FILLER                PIC X(10)   VALUE '6601100110'.      12630005
         05  FILLER                PIC X(10)   VALUE '6701100111'.      12640005
         05  FILLER                PIC X(10)   VALUE '6801101000'.      12650005
         05  FILLER                PIC X(10)   VALUE '6901101001'.      12660005
         05  FILLER                PIC X(10)   VALUE '6A01101010'.      12670005
         05  FILLER                PIC X(10)   VALUE '6B01101011'.      12680005
         05  FILLER                PIC X(10)   VALUE '6C01101100'.      12690005
         05  FILLER                PIC X(10)   VALUE '6D01101101'.      12700005
         05  FILLER                PIC X(10)   VALUE '6E01101110'.      12710005
         05  FILLER                PIC X(10)   VALUE '6F01101111'.      12720005
         05  FILLER                PIC X(10)   VALUE '7001110000'.      12730005
         05  FILLER                PIC X(10)   VALUE '7101110001'.      12740005
         05  FILLER                PIC X(10)   VALUE '7201110010'.      12750005
         05  FILLER                PIC X(10)   VALUE '7301110011'.      12760005
         05  FILLER                PIC X(10)   VALUE '7401110100'.      12770005
         05  FILLER                PIC X(10)   VALUE '7501110101'.      12780005
         05  FILLER                PIC X(10)   VALUE '7601110110'.      12790005
         05  FILLER                PIC X(10)   VALUE '7701110111'.      12800005
         05  FILLER                PIC X(10)   VALUE '7801111000'.      12810005
         05  FILLER                PIC X(10)   VALUE '7901111001'.      12820005
         05  FILLER                PIC X(10)   VALUE '7A01111010'.      12830005
         05  FILLER                PIC X(10)   VALUE '7B01111011'.      12840005
         05  FILLER                PIC X(10)   VALUE '7C01111100'.      12850005
         05  FILLER                PIC X(10)   VALUE '7D01111101'.      12860005
         05  FILLER                PIC X(10)   VALUE '7E01111110'.      12870005
         05  FILLER                PIC X(10)   VALUE '7F01111111'.      12880005
         05  FILLER                PIC X(10)   VALUE '8010000000'.      12890005
         05  FILLER                PIC X(10)   VALUE '8110000001'.      12900005
         05  FILLER                PIC X(10)   VALUE '8210000010'.      12910005
         05  FILLER                PIC X(10)   VALUE '8310000011'.      12920005
         05  FILLER                PIC X(10)   VALUE '8410000100'.      12930005
         05  FILLER                PIC X(10)   VALUE '8510000101'.      12940005
         05  FILLER                PIC X(10)   VALUE '8610000110'.      12950005
         05  FILLER                PIC X(10)   VALUE '8710000111'.      12960005
         05  FILLER                PIC X(10)   VALUE '8810001000'.      12970005
         05  FILLER                PIC X(10)   VALUE '8910001001'.      12980005
         05  FILLER                PIC X(10)   VALUE '8A10001010'.      12990005
         05  FILLER                PIC X(10)   VALUE '8B10001011'.      13000005
         05  FILLER                PIC X(10)   VALUE '8C10001100'.      13010005
         05  FILLER                PIC X(10)   VALUE '8D10001101'.      13020005
         05  FILLER                PIC X(10)   VALUE '8E10001110'.      13030005
           EJECT                                                        13040005
         05  FILLER                PIC X(10)   VALUE '8F10001111'.      13050005
         05  FILLER                PIC X(10)   VALUE '9010010000'.      13060005
         05  FILLER                PIC X(10)   VALUE '9110010001'.      13070005
         05  FILLER                PIC X(10)   VALUE '9210010010'.      13080005
         05  FILLER                PIC X(10)   VALUE '9310010011'.      13090005
         05  FILLER                PIC X(10)   VALUE '9410010100'.      13100005
         05  FILLER                PIC X(10)   VALUE '9510010101'.      13110005
         05  FILLER                PIC X(10)   VALUE '9610010110'.      13120005
         05  FILLER                PIC X(10)   VALUE '9710010111'.      13130005
         05  FILLER                PIC X(10)   VALUE '9810011000'.      13140005
         05  FILLER                PIC X(10)   VALUE '9910011001'.      13150005
         05  FILLER                PIC X(10)   VALUE '9A10011010'.      13160005
         05  FILLER                PIC X(10)   VALUE '9B10011011'.      13170005
         05  FILLER                PIC X(10)   VALUE '9C10011100'.      13180005
         05  FILLER                PIC X(10)   VALUE '9D10011101'.      13190005
         05  FILLER                PIC X(10)   VALUE '9E10011110'.      13200005
         05  FILLER                PIC X(10)   VALUE '9F10011111'.      13210005
         05  FILLER                PIC X(10)   VALUE 'A010100000'.      13220005
         05  FILLER                PIC X(10)   VALUE 'A110100001'.      13230005
         05  FILLER                PIC X(10)   VALUE 'A210100010'.      13240005
         05  FILLER                PIC X(10)   VALUE 'A310100011'.      13250005
         05  FILLER                PIC X(10)   VALUE 'A410100100'.      13260005
         05  FILLER                PIC X(10)   VALUE 'A510100101'.      13270005
         05  FILLER                PIC X(10)   VALUE 'A610100110'.      13280005
         05  FILLER                PIC X(10)   VALUE 'A710100111'.      13290005
         05  FILLER                PIC X(10)   VALUE 'A810101000'.      13300005
         05  FILLER                PIC X(10)   VALUE 'A910101001'.      13310005
         05  FILLER                PIC X(10)   VALUE 'AA10101010'.      13320005
         05  FILLER                PIC X(10)   VALUE 'AB10101011'.      13330005
         05  FILLER                PIC X(10)   VALUE 'AC10101100'.      13340005
         05  FILLER                PIC X(10)   VALUE 'AD10101101'.      13350005
         05  FILLER                PIC X(10)   VALUE 'AE10101110'.      13360005
         05  FILLER                PIC X(10)   VALUE 'AF10101111'.      13370005
         05  FILLER                PIC X(10)   VALUE 'B010110000'.      13380005
         05  FILLER                PIC X(10)   VALUE 'B110110001'.      13390005
         05  FILLER                PIC X(10)   VALUE 'B210110010'.      13400005
         05  FILLER                PIC X(10)   VALUE 'B310110011'.      13410005
         05  FILLER                PIC X(10)   VALUE 'B410110100'.      13420005
         05  FILLER                PIC X(10)   VALUE 'B510110101'.      13430005
         05  FILLER                PIC X(10)   VALUE 'B610110110'.      13440005
         05  FILLER                PIC X(10)   VALUE 'B710110111'.      13450005
         05  FILLER                PIC X(10)   VALUE 'B810111000'.      13460005
         05  FILLER                PIC X(10)   VALUE 'B910111001'.      13470005
         05  FILLER                PIC X(10)   VALUE 'BA10111010'.      13480005
         05  FILLER                PIC X(10)   VALUE 'BB10111011'.      13490005
         05  FILLER                PIC X(10)   VALUE 'BC10111100'.      13500005
         05  FILLER                PIC X(10)   VALUE 'BD10111101'.      13510005
         05  FILLER                PIC X(10)   VALUE 'BE10111110'.      13520005
         05  FILLER                PIC X(10)   VALUE 'BF10111111'.      13530005
         05  FILLER                PIC X(10)   VALUE 'C011000000'.      13540005
         05  FILLER                PIC X(10)   VALUE 'C111000001'.      13550005
         05  FILLER                PIC X(10)   VALUE 'C211000010'.      13560005
         05  FILLER                PIC X(10)   VALUE 'C311000011'.      13570005
         05  FILLER                PIC X(10)   VALUE 'C411000100'.      13580005
         05  FILLER                PIC X(10)   VALUE 'C511000101'.      13590005
         05  FILLER                PIC X(10)   VALUE 'C611000110'.      13600005
         05  FILLER                PIC X(10)   VALUE 'C711000111'.      13610005
         05  FILLER                PIC X(10)   VALUE 'C811001000'.      13620005
         05  FILLER                PIC X(10)   VALUE 'C911001001'.      13630005
         05  FILLER                PIC X(10)   VALUE 'CA11001010'.      13640005
         05  FILLER                PIC X(10)   VALUE 'CB11001011'.      13650005
         05  FILLER                PIC X(10)   VALUE 'CC11001100'.      13660005
         05  FILLER                PIC X(10)   VALUE 'CD11001101'.      13670005
         05  FILLER                PIC X(10)   VALUE 'CE11001110'.      13680005
         05  FILLER                PIC X(10)   VALUE 'CF11001111'.      13690005
         05  FILLER                PIC X(10)   VALUE 'D011010000'.      13700005
         05  FILLER                PIC X(10)   VALUE 'D111010001'.      13710005
         05  FILLER                PIC X(10)   VALUE 'D211010010'.      13720005
         05  FILLER                PIC X(10)   VALUE 'D311010011'.      13730005
         05  FILLER                PIC X(10)   VALUE 'D411010100'.      13740005
         05  FILLER                PIC X(10)   VALUE 'D511010101'.      13750005
           EJECT                                                        13760005
         05  FILLER                PIC X(10)   VALUE 'D611010110'.      13770005
         05  FILLER                PIC X(10)   VALUE 'D711010111'.      13780005
         05  FILLER                PIC X(10)   VALUE 'D811011000'.      13790005
         05  FILLER                PIC X(10)   VALUE 'D911011001'.      13800005
         05  FILLER                PIC X(10)   VALUE 'DA11011010'.      13810005
         05  FILLER                PIC X(10)   VALUE 'DB11011011'.      13820005
         05  FILLER                PIC X(10)   VALUE 'DC11011100'.      13830005
         05  FILLER                PIC X(10)   VALUE 'DD11011101'.      13840005
         05  FILLER                PIC X(10)   VALUE 'DE11011110'.      13850005
         05  FILLER                PIC X(10)   VALUE 'DF11011111'.      13860005
         05  FILLER                PIC X(10)   VALUE 'E011100000'.      13870005
         05  FILLER                PIC X(10)   VALUE 'E111100001'.      13880005
         05  FILLER                PIC X(10)   VALUE 'E211100010'.      13890005
         05  FILLER                PIC X(10)   VALUE 'E311100011'.      13900005
         05  FILLER                PIC X(10)   VALUE 'E411100100'.      13910005
         05  FILLER                PIC X(10)   VALUE 'E511100101'.      13920005
         05  FILLER                PIC X(10)   VALUE 'E611100110'.      13930005
         05  FILLER                PIC X(10)   VALUE 'E711100111'.      13940005
         05  FILLER                PIC X(10)   VALUE 'E811101000'.      13950005
         05  FILLER                PIC X(10)   VALUE 'E911101001'.      13960005
         05  FILLER                PIC X(10)   VALUE 'EA11101010'.      13970005
         05  FILLER                PIC X(10)   VALUE 'EB11101011'.      13980005
         05  FILLER                PIC X(10)   VALUE 'EC11101100'.      13990005
         05  FILLER                PIC X(10)   VALUE 'ED11101101'.      14000005
         05  FILLER                PIC X(10)   VALUE 'EE11101110'.      14010005
         05  FILLER                PIC X(10)   VALUE 'EF11101111'.      14020005
         05  FILLER                PIC X(10)   VALUE 'F011110000'.      14030005
         05  FILLER                PIC X(10)   VALUE 'F111110001'.      14040005
         05  FILLER                PIC X(10)   VALUE 'F211110010'.      14050005
         05  FILLER                PIC X(10)   VALUE 'F311110011'.      14060005
         05  FILLER                PIC X(10)   VALUE 'F411110100'.      14070005
         05  FILLER                PIC X(10)   VALUE 'F511110101'.      14080005
         05  FILLER                PIC X(10)   VALUE 'F611110110'.      14090005
         05  FILLER                PIC X(10)   VALUE 'F711110111'.      14100005
         05  FILLER                PIC X(10)   VALUE 'F811111000'.      14110005
         05  FILLER                PIC X(10)   VALUE 'F911111001'.      14120005
         05  FILLER                PIC X(10)   VALUE 'FA11111010'.      14130005
         05  FILLER                PIC X(10)   VALUE 'FB11111011'.      14140005
         05  FILLER                PIC X(10)   VALUE 'FC11111100'.      14150005
         05  FILLER                PIC X(10)   VALUE 'FD11111101'.      14160005
         05  FILLER                PIC X(10)   VALUE 'FE11111110'.      14170005
         05  FILLER                PIC X(10)   VALUE 'FF11111111'.      14180005
           EJECT                                                        14190005
       01  FILLER                  REDEFINES BIT-TEST-TABLE.            14200005
         05  FILLER                PIC X(10).                           14210005
         05  BIT-BYTE              OCCURS 255 TIMES INDEXED BY BX.      14220005
           10  BYTE-2-BYTE         PIC XX.                              14230005
           10  BYTE-8-BYTE.                                             14240005
             15  BIT-80            PIC X.                               14250005
               88  BIT-80-OFF                  VALUE '0'.               14260005
               88  BIT-80-ON                   VALUE '1'.               14270005
             15  BIT-40            PIC X.                               14280005
               88  BIT-40-OFF                  VALUE '0'.               14290005
               88  BIT-40-ON                   VALUE '1'.               14300005
             15  BIT-20            PIC X.                               14310005
               88  BIT-20-OFF                  VALUE '0'.               14320005
               88  BIT-20-ON                   VALUE '1'.               14330005
             15  BIT-10            PIC X.                               14340005
               88  BIT-10-OFF                  VALUE '0'.               14350005
               88  BIT-10-ON                   VALUE '1'.               14360005
             15  BIT-08            PIC X.                               14370005
               88  BIT-08-OFF                  VALUE '0'.               14380005
               88  BIT-08-ON                   VALUE '1'.               14390005
             15  BIT-04            PIC X.                               14400005
               88  BIT-04-OFF                  VALUE '0'.               14410005
               88  BIT-04-ON                   VALUE '1'.               14420005
             15  BIT-02            PIC X.                               14430005
               88  BIT-02-OFF                  VALUE '0'.               14440005
               88  BIT-02-ON                   VALUE '1'.               14450005
             15  BIT-01            PIC X.                               14460005
               88  BIT-01-OFF                  VALUE '0'.               14470005
               88  BIT-01-ON                   VALUE '1'.               14480005
       01  BINARY-BYTE-WORK-AREA.                                       14490005
         05  BYTE-WORK-AREA.                                            14500005
           10  FILLER              PIC X       VALUE LOW-VALUE.         14510005
           10  BYTE-WORK           PIC X       VALUE LOW-VALUE.         14520005
         05  FILLER                REDEFINES   BYTE-WORK-AREA.          14530005
           10  BINARY-BYTE         PIC S9(4)   COMP.                    14540005
                                                                        14550005
         05  SEX-REL-SAVE.                                              14560005
           10  SEX-REL-PRINT       PIC XX.                              14570005
           10  SR-BIT-80           PIC X.                               14580005
             88  SEX-REL-SPONSORED             VALUE '1'.               14590005
           10  SR-BIT-40           PIC X.                               14600005
             88  SEX-REL-MARRIED               VALUE '1'.               14610005
           10  SR-BIT-20           PIC X.                               14620005
             88  SEX-REL-SINGLE                VALUE '1'.               14630005
           10  SR-BIT-10           PIC X.                               14640005
             88  SEX-REL-DEPENDENT             VALUE '1'.               14650005
           10  SR-BIT-08           PIC X.                               14660005
             88  SEX-REL-SPOUSE                VALUE '1'.               14670005
           10  SR-BIT-04           PIC X.                               14680005
             88  SEX-REL-SUBSCRIBER            VALUE '1'.               14690005
           10  SR-BIT-02           PIC X.                               14700005
             88  SEX-REL-FEMALE                VALUE '1'.               14710005
           10  SR-BIT-01           PIC X.                               14720005
             88  SEX-REL-MALE                  VALUE '1'.               14730005
                                                                        14740005
MD7684/**************************************************************** 14750005
MD7684*                                                               * 14760005
MD7684*                     R E P O R T      A R E A                  * 14770005
MD7684*                                                               * 14780005
MD7684***************************************************************** 14790005
MD7684                                                                  14800005
MD7684 01  ERROR-REPORT.                                                14810005
MD7684                                                                  14820005
MD7684     05  RPT-DUPLICATE-ERROR-FILE.                                14830005
MD7684                                                                  14840005
MD7684         10  RPT-ERR-HDR-1ST-LINE.                                14850005
MD7684             15  FILLER     PIC X(20) VALUE SPACES.               14860005
MD7684             15  FILLER     PIC X(18) VALUE 'MISMATCH ON GROUP/'. 14870005
MD7684             15  FILLER     PIC X(16) VALUE 'SECTION/PACKAGE '.   14880005
MD7684             15  FILLER     PIC X(13) VALUE 'WITH HDRT5W2K'.      14890005
MD7684             15  FILLER     PIC X(353) VALUE SPACES.              14900005
MD7684                                                                  14910005
******** ************************************************************** 14920005
******** *          I O  A C C E S S O R  W O R K  A R E A              14930005
******** ************************************************************** 14940005
                                                                        14950005
       01  TOTAL-CLAIM-LENGTH           PIC S9(09) COMP SYNC.           14960005
       01  MAXIMUM-LINES                PIC S9(09) COMP SYNC.           14970005
       01  MAXIMUM-HEADER-LENGTH        PIC S9(09) COMP SYNC.           14980005
       01  MAXIMUM-DETAIL-LENGTH        PIC S9(09) COMP SYNC.           14990005
                                                                        15000005
       01  IOAREA1-ADDRESS              PIC S9(09) COMP SYNC.           15010005
                                                                        15020005
       01  IOAREA1-POINTER REDEFINES IOAREA1-ADDRESS                    15030005
                                   USAGE IS POINTER.                    15040005
                                                                        15050005
       01  IOAREA2-ADDRESS              PIC S9(09) COMP SYNC.           15060005
                                                                        15070005
       01  IOAREA2-POINTER REDEFINES IOAREA2-ADDRESS                    15080005
                                   USAGE IS POINTER.                    15090005
                                                                        15100005
       01  VSAM-IO-WORKAREA             PIC X(01).                      15110005
       01  IO-ACCESSOR-BLOW-UP-MSG      PIC X(128) VALUE SPACES.        15120005
       01  IO-ACCESSOR-INPUT-DCB        PIC X(96).                      15130005
       01  IO-ACCESSOR-NEW-MASTER-DCB   PIC X(96).                      15140005
       01  IO-ACCESSOR-DELETES-DCB      PIC X(96).                      15150005
       01  IO-ACCESSOR-FIXED-DCB        PIC X(96).                      15160005
                                                                        15170005
       01  IO-ACCESSOR-SAVE.                                            15180005
           05  FILLLER                  PIC X(392) VALUE LOW-VALUES.    15190005
           05  QSAM-PENDING-DDNAME      PIC X(08) VALUE 'SEQFILE '.     15200005
                                                                        15210005
       01  IO-ACCESSOR-TYPEIO.                                          15220005
                                                                        15230005
           05  IO-ACCESSOR-IOTYPE       PIC X(4).                       15240005
           05  IO-ACCESSOR-VIEW         PIC X(02) VALUE 'DY'.           15250005
                                                                        15260005
       01  IO-ACCESSOR-RETURN-CODE      PIC S9(09) COMP SYNC.           15270005
                                                                        15280005
       01  IO-ACCESSOR-IOPARM-SETUP.                                    15290005
                                                                        15300005
           05  IO-ACCESSOR-IOPARM-SETUP-AREA.                           15310005
                                                                        15320005
              10  IO-ACCESSOR-COMMAND   PIC X(04) VALUE SPACES.         15330005
                                                                        15340005
                  88 IOPARM-OPEN                  VALUE ' OAI' ' OAO'.  15350005
                  88 IOPARM-CLOSE                 VALUE ' CLA'.         15360005
                  88 IOPARM-READ                  VALUE ' RA2' ' RA3'.  15370005
                  88 IOPARM-WRITE                 VALUE ' AA2' 'HAA2'.  15380005
                  88 IOPARM-EXPLODE               VALUE ' RF1'.         15390005
                  88 IOPARM-IMPLODE               VALUE ' IMP'.         15400005
                  88 IOPARM-GETMAIN               VALUE 'GETM'.         15410005
                                                                        15420005
              10  IO-ACCESSOR-LITERAL   PIC X(07) VALUE SPACES.         15430005
              10  IO-ACCESSOR-DDNAME    PIC X(08) VALUE SPACES.         15440005
                                                                        15450005
       01 WK-DATE-CHECK.                                                15460005
          05 LOW-DATE-CHECK             PIC 9(08) VALUE 19600101.       15470005
          05 HIGH-DAE-CHECK             PIC 9(08) VALUE 0.              15480005
                                                                        15490005
ANI612 01 WK-DISPLAY-PROCESSED.                                         15500005
ANI612    05 WK-ACARE-HEAD-CNT           PIC 9(08) VALUE 0.             15510005
ANI612    05 WK-MCARE-HEAD-CNT           PIC 9(08) VALUE 0.             15520005
ANI612    05 WK-NJCDH-HEAD-CNT           PIC 9(08) VALUE 0.             15530005
ANI612    05 WK-AOPTM-HEAD-CNT           PIC 9(08) VALUE 0.             15540005
AN1560    05 WK-AOPTR-HEAD-CNT           PIC 9(08) VALUE 0.             15550005
AN1781    05 WK-AARCH-HEAD-CNT           PIC 9(08) VALUE 0.             15560005
AN1589    05 WK-MAGLN-HEAD-CNT           PIC 9(08) VALUE 0.             15570005
AN0127    05 WK-BEACN-HEAD-CNT           PIC 9(08) VALUE 0.             15580005
ANI612    05 WK-MACMK-HEAD-CNT           PIC 9(08) VALUE 0.             15590005
ANI612    05 WK-CCARE-HEAD-CNT           PIC 9(08) VALUE 0.             15600005
MD6619    05 WK-CVSCM-HEAD-CNT           PIC 9(08) VALUE 0.             15610005
ANI612    05 WK-WYETH-HEAD-CNT           PIC 9(08) VALUE 0.             15620005
ANI612    05 WK-ACARE-DETL-CNT           PIC 9(08) VALUE 0.             15630005
ANI612    05 WK-MCARE-DETL-CNT           PIC 9(08) VALUE 0.             15640005
ANI612    05 WK-NJCDH-DETL-CNT           PIC 9(08) VALUE 0.             15650005
ANI612    05 WK-AOPTM-DETL-CNT           PIC 9(08) VALUE 0.             15660005
AN1560    05 WK-AOPTR-DETL-CNT           PIC 9(08) VALUE 0.             15670005
AN1781    05 WK-AARCH-DETL-CNT           PIC 9(08) VALUE 0.             15680005
AN1589    05 WK-MAGLN-DETL-CNT           PIC 9(08) VALUE 0.             15690005
AN0127    05 WK-BEACN-DETL-CNT           PIC 9(08) VALUE 0.             15700005
ANI612    05 WK-MACMK-DETL-CNT           PIC 9(08) VALUE 0.             15710005
ANI612    05 WK-CCARE-DETL-CNT           PIC 9(08) VALUE 0.             15720005
MD6619    05 WK-CVSCM-DETL-CNT           PIC 9(08) VALUE 0.             15730005
ANI612    05 WK-WYETH-DETL-CNT           PIC 9(08) VALUE 0.             15740005
ANI612    05 WK-ACARE-TRLR-CNT           PIC 9(08) VALUE 0.             15750005
ANI612    05 WK-MCARE-TRLR-CNT           PIC 9(08) VALUE 0.             15760005
ANI612    05 WK-NJCDH-TRLR-CNT           PIC 9(08) VALUE 0.             15770005
ANI612    05 WK-AOPTM-TRLR-CNT           PIC 9(08) VALUE 0.             15780005
AN1560    05 WK-AOPTR-TRLR-CNT           PIC 9(08) VALUE 0.             15790005
AN1781    05 WK-AARCH-TRLR-CNT           PIC 9(08) VALUE 0.             15800005
AN1589    05 WK-MAGLN-TRLR-CNT           PIC 9(08) VALUE 0.             15810005
AN0127    05 WK-BEACN-TRLR-CNT           PIC 9(08) VALUE 0.             15820005
ANI612    05 WK-MACMK-TRLR-CNT           PIC 9(08) VALUE 0.             15821005
ANI612    05 WK-CCARE-TRLR-CNT           PIC 9(08) VALUE 0.             15822005
MD6619    05 WK-CVSCM-TRLR-CNT           PIC 9(08) VALUE 0.             15823005
ANI612    05 WK-WYETH-TRLR-CNT           PIC 9(08) VALUE 0.             15824005
                                                                        15825005
       01  WK-ERROR-SW.                                                 15826005
           05 FILLER                        PIC 9(4) COMP VALUE 0.      15827005
           05 FILLER                        PIC 9(4) COMP VALUE 0.      15828005
           05 FILLER                        PIC 9(4) COMP VALUE 0.      15829005
           05 FILLER                        PIC 9(4) COMP VALUE 0.      15830005
           05 FILLER                        PIC 9(4) COMP VALUE 0.      15840005
                                                                        15850005
       01  WK-ERROR-POINTERS REDEFINES WK-ERROR-SW.                     15860005
           05 ERROR-CODE                       PIC 9(04) COMP           15870005
               OCCURS 5 TIMES INDEXED BY WK-ERROR-CD-IDX.               15880005
              88 INVALID-RECORD-TYPE          VALUE 0003.               15890005
              88 PARTICIPANT-ACCT-NUMBER      VALUE 0001.               15900005
              88 INVALID-PARTICIPANT          VALUE 0002.               15910005
              88 TARTICIPANT-ACCT-NUMBER      VALUE 0004.               15920005
              88 YARTICIPANT-ACCT-NUMBER      VALUE 0005.               15930005
              88 UARTICIPANT-ACCT-NUMBER      VALUE 0006.               15940005
                                                                        15950005
       01  WK-ERROR-MSG.                                                15960005
           05 FILLER                        PIC X(80) VALUE             15970005
             "INVALID-RECORD-TYPE            ".                         15980005
           05 FILLER                        PIC X(80) VALUE             15990005
             "INVALID PARTICIPANT-ACCT-NUMBER".                         16000005
           05 FILLER                        PIC X(80) VALUE             16010005
             "INVALID PARTICIPANT-GENDER".                              16020005
           05 FILLER                        PIC X(80) VALUE             16030005
             "INVALID PARTICIPANT-ACCT-NUMBER".                         16040005
           05 FILLER                        PIC X(80) VALUE             16050005
             "INVALID PARTICIPANT-ACCT-NUMBER".                         16060005
           05 FILLER                        PIC X(80) VALUE             16070005
             "INVALID PARTICIPANT-ACCT-NUMBER".                         16080005
                                                                        16090005
       01  WK-ERROR-MSG-R REDEFINES WK-ERROR-MSG.                       16100005
           05 ERROR-MSG-TEXT.                                           16110005
              10 ERROR-MSG                     PIC X(80)                16120005
                  OCCURS 5 TIMES INDEXED BY  WK-ERROR-MS-IDX.           16130005
                                                                        16140005
       01  WK-ERROR-PASS-FAIL.                                          16150005
           05 ERROR-CODES.                                              16160005
              10 ERROR-PASS-FAIL               PIC X(05)                16170005
                                                  VALUE "PPPPP".        16180005
                 88 ERROR-TO-PASS                 VALUE                 16190005
                                                 "PPPPP".               16200005
           05 ERROR-CODES-R REDEFINES ERROR-CODES.                      16210005
              10 ERROR-PASS-FAIL-R             PIC X(01)                16220005
                  OCCURS 5 TIMES INDEXED BY ERROR-CHECK-IDX.            16230005
                 88 ERROR-PASS                    VALUE "P".            16240005
                 88 ERROR-FAIL                    VALUE "F".            16250005
                                                                        16260005
       01  WK-DATES.                                                    16270005
           05 FIRST-DATE                       PIC 9(08)  VALUE         16280005
                                                  20000101.             16290005
           05 TODAYS-DATE                      PIC 9(08)  VALUE         16300005
                                                  ZERO.                 16310005
MD6619 01  WS-PAT-NAME.                                                 16320005
MD6619     05  WS-PAT-FIRST-NAME               PIC X(25).               16330005
MD6619     05  WS-PAT-MIDDLE-NAME              PIC X(25).               16340005
MD6619     05  WS-PAT-LAST-NAME                PIC X(25).               16350005
MD6619                                                                  16360005
MD6619 01  RECORD-TYPE-CA.                                              16370005
MD6619     05  CVS-CONTROL-PLAN-CODE           PIC  X(03).              16380005
MD6619     05  CVS-SUBSCRIBER-ID               PIC  X(13).              16390005
MD6619     05  CVS-FILE-TYPE                   PIC  X(1).               16400005
MD6619     05  CVS-VENDOR-NAME                 PIC  X(5).               16410005
MD6619     05  CVS-PAT-FST-FULL-NAME           PIC  X(25).              16420005
MD6619     05  CVS-PAT-DOB                     PIC  S9(4) COMP.         16430005
MD6619     05  CVS-DOS                         PIC  S9(4) COMP.         16440005
MD6619                                                                  16450005
MD6619 01  HRHSID01-PARM.                                               16460005
MD6619      05  L-REQUEST-TYPE                 PIC  X(01).              16470005
MD6619      05  L-REQUEST-FUNC                 PIC  X(03).              16480005
MD6619      05  L-RETURN-CODE-CVS              PIC  X(03).              16490005
MD6619 01  HRHSID01-VALUES.                                             16500005
MD6619      05  L-INPUT-PARMS-CVS              PIC X(100).              16510005
MD6619      05  L-RETURN-AREA-CVS              PIC X(300).              16520005
MD6619                                                                  16530005
MA5098 01  RECORD-TYPE-CM.                                              16540005
MA5098     05  CMK-CONTROL-PLAN-CODE           PIC  X(03).              16550005
MA5098     05  CMK-SUBSCRIBER-ID               PIC  X(13).              16560005
MA5098     05  CMK-FILE-TYPE                   PIC  X(01).              16570005
MA5098     05  CMK-PAT-SEX-REL                 PIC  X(01).              16580005
MA5098     05  CMK-PAT-DOB                     PIC  S9(4) COMP.         16590005
MA5098     05  CMK-DOS                         PIC  S9(4) COMP.         16600005
MA5098     05  CMK-VENDOR-CD                   PIC  X(05).              16610005
MA5098                                                                  16620005
MD838A                                                                  16621007
MD838A 01 RECORD-TYPE-XX.                                               16622007
MD838A     05  LXX-CONTROL-PLAN-CODE           PIC  X(03).              16623007
MD838A     05  LXX-SUBSCRIBER-ID               PIC  X(13).              16624007
MD838A     05  LXX-DOS                         PIC  S9(4) COMP.         16625007
MD161B     05  LXX-PAT-DOB                     PIC  S9(4) COMP.         16625114
MD161B     05  LXX-PAT-FST-FULL-NAME           PIC  X(31).              16625214
                                                                        16626011
      /**************************************************************** 16630005
      **                    W O R K  A R E A S                          16640005
      ***************************************************************** 16650005
                                                                        16660005
       01  WS-CAREGAIN-TABLE-PARMS.                                     16670005
                                                                        16680005
          03  WS-CTL-PARMS.                                             16690005
              05  CGT1-REQ-TYPE                      PIC  X(01).        16700005
              05  CGT1-RETURN-CODE                   PIC  X(01).        16710005
          03  WS-DATA-PARMS.                                            16720005
              05  CGT1-SUB-PLAN                      PIC  9(03).        16730005
              05  CGT1-GROUP-BASE                    PIC  X(09).        16740005
              05  CGT1-SECTION                       PIC  X(04).        16750005
              05  CGT1-PKG-CODE                      PIC  9(03).        16760005
              05  CGT1-FDOS                          PIC  X(08).        16770005
              05  CGT1-RETURN-AREA.                                     16780005
                  07  CGT1-PLAN-CODE             PIC 9(03).             16790005
                  07  CGT1-GRP-NUMBER            PIC X(09).             16800005
                  07  CGT1-GRP-SECT-NUM          PIC 9(04).             16810005
                  07  CGT1-PACKAGE-CODE          PIC 9(03).             16820005
                  07  CGT1-SEQ-NO                PIC 9(04).             16830005
                  07  CGT1-EFF-DATE              PIC X(08).             16840005
                  07  CGT1-CANCEL-DATE           PIC X(08).             16850005
                  07  CGT1-CLIENT-NUMBER         PIC X(09).             16860005
                  07  CGT1-DEBIT-CARD-IND        PIC X(01).             16870005
                  07  CGT1-PBM-IND               PIC X(01).             16880005
                  07  CGT1-CDH-IND               PIC X(04).             16890005
                  07  CGT1-GRP-NAME              PIC X(25).             16900005
                  07  CGT1-DATE-STAMP            PIC X(08).             16910005
              05  FILLER                         PIC X(25).             16920005
                                                                        16930005
NJ0192 01  CAR-REC-ID-TIMEH                      PIC 9(08).             16940005
NJ0192 01  CAR-CLM-ID-TIMEH                      PIC 9(08).             16950005
NJ0192 01  CAR-REC-ID-TIMEJ                      PIC 9(03).             16960005
NJ0192 01  CAR-CLM-ID-TIMEJ                      PIC 9(03).             16970005
MA2010                                                                  16980005
MA2010 01  HRHSMASN-PARMS.                                              16990005
MA2010     05 L-CALL-STATUS                   PIC X(01).                17000005
MA2010          88 L-FIRST-CALL                 VALUE 'F'.              17010005
MA2010          88 L-PROCESSING-CALL            VALUE 'P'.              17020005
MA2010          88 L-LAST-CALL                  VALUE 'Y'.              17030005
MA2010     05 L-CALL-REQUEST                  PIC X(01).                17040005
MA2010          88 L-GET-XREF-ID                VALUE 'X'.              17050005
MA2010          88 L-GET-CLIENT-ID              VALUE 'C'.              17060005
MA2010          88 L-GET-GROUP                  VALUE 'G'.              17070005
MA2010     05 L-XREF-CALL-PARM.                                         17080005
MA2010        10 L-INPUT-SSN                     PIC X(13).             17090005
MA2010     05 L-SYSIN-CALL-PARM.                                        17100005
MA2010        10 L-SUB-PLAN                      PIC 9(04) COMP.        17110005
MA2010        10 L-VENDOR-FMT                    PIC X(05).             17120005
MA2010        10 L-GP-BASE                       PIC X(09).             17130005
MA2010        10 L-GP-SEC                        PIC X(04).             17140005
MA2010        10 L-GP-PKG                        PIC X(03).             17150005
MA2010     05 L-RETURN-AREA.                                            17160005
MA2010        10 L-RETURN-SSN                 PIC X(09).                17170005
MA2010        10 L-RETURN-GROUP               PIC X(15).                17180005
MA2010        10 L-RETURN-PGROUP              PIC X(15).                17190005
MA2010        10 L-RETURN-CLIENT-ID           PIC X(15).                17200005
MA2010        10 L-FILLER                     PIC X(44).                17210005
MA2010        10 L-RETURN-CODE                PIC X(02).                17220005
MA2010                                                                  17230005
NJ0983 01  HRHSINVM-PARMS.                                              17240005
NJ0983     05  L-INVM-CALL-STATUS                     PIC X(01).        17250005
NJ0983         88  L-INVM-FIRST-CALL            VALUE 'F'.              17260005
NJ0983         88  L-INVM-PROCESSING-CALL       VALUE 'P'.              17270005
NJ0983         88  L-INVM-LAST-CALL             VALUE 'L'.              17280005
NJ0983     05  L-INVM-CALL-PARM.                                        17290005
NJ0983         10  L-INVM-SUB-PLAN                    PIC 9(04) COMP.   17300005
NJ0983         10  L-INVM-VENDOR-ID                   PIC X(05).        17310005
NJ0983         10  L-INVM-GP-BASE                     PIC X(09).        17320005
NJ0983         10  L-INVM-GP-SEC                      PIC X(04).        17330005
NJ0983         10  L-INVM-GP-PKG                      PIC 9(03).        17340005
NJ0983         10  L-INVM-FIRST-DOS                   PIC X(08).        17350005
NJ0983     05  L-INVM-RETURN-AREA.                                      17360005
MN0489         10  L-INVM-RETURN-OUTBOUND-RECORD      PIC X(10000).     17370005
NJ0983         10  L-INVM-RETURN-CODE                 PIC X(02).        17380005
NJ0983                                                                  17390005
NJ9032 01  HRHSEHNC-PARMS.                                              17400005
NJ9032     REPLACE ==:P:==   BY ==L-==                                  17410005
NJ9032             ==:L2:==  BY ==05==                                  17420005
NJ9032             ==:L3:==  BY ==10==                                  17430005
NJ9032             ==:L4:==  BY ==15==.                                 17440005
NJ9032             COPY HRHYPMN1.                                       17450005
NJ9032     REPLACE OFF.                                                 17460005
                                                                        17470005
AN0609 01  HRHS4QTR-PARMS.                                              17480005
AN0609                                                                  17490005
AN0609     REPLACE ==:P:==   BY ==L-==                                  17500005
AN0609             ==:L2:==  BY ==05==                                  17510005
AN0609             ==:L3:==  BY ==10==                                  17520005
AN0609             ==:L4:==  BY ==15==.                                 17530005
AN0609             COPY HRHYPAN1.                                       17540005
AN0609                                                                  17550005
AN0609     REPLACE OFF.                                                 17560005
MD7684                                                                  17570005
MD7684 01  HRHST5W2-PARMS.                                              17580005
MD7684     05  L-T5W2-CALL-STATUS                     PIC X(01).        17590005
MD7684         88  L-T5W2-FIRST-CALL            VALUE 'F'.              17600005
MD7684         88  L-T5W2-PROCESSING-CALL       VALUE 'P'.              17610005
MD7684         88  L-T5W2-LAST-CALL             VALUE 'L'.              17620005
MD7684     05  L-T5W2-CALL-PARM.                                        17630005
MD7684         10  L-T5W2-SUB-PLAN                    PIC 9(03) COMP.   17640005
MD7684         10  L-T5W2-GP-BASE                     PIC X(09).        17650005
MD7684         10  L-T5W2-GP-SEC                      PIC X(04).        17660005
MD7684         10  L-T5W2-GP-PKG                      PIC 9(03).        17670005
MD7684         10  L-T5W2-FIRST-DOS                   PIC X(08).        17680005
MD7684     05  L-T5W2-RETURN-AREA.                                      17690005
MD7684         10  L-T5W2-RETURN-OUTBOUND-RECORD      PIC X(30).        17700005
MD7684         10  L-T5W2-RETURN-CODE                 PIC X(02).        17710005
MD7684                                                                  17720005
NJ476C 01  HRHSTPOS-PARMS.                                              17730005
NJ476C     05  L-TPOS-CALL-STATUS                     PIC X(01).        17740005
NJ476C         88  L-TPOS-FIRST-CALL            VALUE 'F'.              17750005
NJ476C         88  L-TPOS-PROCESSING-CALL       VALUE 'P'.              17760005
NJ476C         88  L-TPOS-LAST-CALL             VALUE 'L'.              17770005
NJ476C     05  L-TPOS-CALL-PARM.                                        17780005
NJ476C         10  L-TPOS-CLAIM-NO                    PIC 9(14).        17790005
NJ476C         10  L-TPOS-VENDOR-ID                   PIC X(05).        17800005
NJ476C         10  L-TPOS-FREQUENCY                   PIC X(01).        17810005
NJ476C         10  L-TPOS-TIER-VALUE                  PIC X(01).        17820005
NJ476C                                                                  17830005
       01  WORKING-STORAGE-END          PIC X(40) VALUE                 17831005
           'HRHSAO3C ***** WORKING STORAGE ENDS HERE'.                  17832005
                                                                        17833005
      ****************************************************************  17834005
      *              L I N K A G E   S E C T I O N                   *  17835005
      ****************************************************************  17836005
                                                                        17837005
       LINKAGE SECTION.                                                 17838005
                                                                        17839005
       REPLACE ==:P:==  BY ==    ==.                                    17840005
           COPY HQCYDCLC.                                               17850005
       REPLACE OFF.                                                     17860005
                                                                        17870005
NR0007 REPLACE ==:P:==  BY ==GENO-==.                                   17880005
NR0007     COPY HRHYNOFM.                                               17890005
NR0007 REPLACE OFF.                                                     17900005
NR0007                                                                  17910005
      ***************************************************************** 17920005
      *            E N D  O F  W O R K I N G  S T O R A G E             17930005
      ***************************************************************** 17940005
                                                                        17950005
                                                                        17960005
      ******************************************************************17970005
      *                    PROCEDURE DIVISION                          *17980005
      ******************************************************************17990005
                                                                        18000005
       PROCEDURE DIVISION USING DYNAMIC-CLAIM,                          18010005
                                CLAIM-HEADER,                           18020005
                                CLAIM-DETAIL,                           18030005
                                GENO-TABLE-ENTRY.                       18040005
                                                                        18050005
       MAINLINE.                                                        18060005
NJ0192     MOVE FUNCTION CURRENT-DATE TO W-CURRENT-DATE-AREA            18070005
NJ0192     MOVE W-CURR-TIME     TO CAR-REC-ID-TIMEH                     18080005
NJ0192                             CAR-CLM-ID-TIMEH                     18090005
                                                                        18100005
NJ0192     MOVE DATE-FORMAT-YYYYDDD       TO W-FORMAT-2                 18110005
NJ0192     MOVE DATE-FUNCTION-SYSTEM      TO W-FUNCTION-CODE            18120005
NJ0192     PERFORM CONVERT-DATE                                         18130005
NJ0192     MOVE W-DATE-2-8 (5:3)          TO CAR-REC-ID-TIMEJ           18140005
NJ0192                                       CAR-CLM-ID-TIMEJ           18150005
           EVALUATE TRUE                                                18160005
NR0007        WHEN  GENO-PROCESSING-CALL                                18170005
MI0730*MI2203   IF GRP-BASE(LINE-INDEX) = WS-GRP-BASE-TKUSA             18180005
MI0730*MI2203      PERFORM PROCESS-CLAIM                                18190005
MI0730*MI2203   ELSE                                                    18200005
MI1537*MI2203      IF CONSUMER-DRIVEN-HEALTH-IND NOT = 'S' OR           18210005
MI1537*MI2203        (CONSUMER-DRIVEN-HEALTH-IND    = 'S' AND           18220005
MI1537*MI2203         CLAIM-PROCESS-STATUS      NOT = '8')              18230005
                      PERFORM PROCESS-CLAIM                             18240005
MI1537*MI2203      END-IF                                               18250005
MI0730*MI2203   END-IF                                                  18260005
                                                                        18270005
NR0007        WHEN  GENO-FIRST-CALL                                     18280005
                 PERFORM INITIALIZATION                                 18290005
                                                                        18300005
NR0007        WHEN  GENO-LAST-CALL                                      18310005
                 PERFORM FINALIZATION                                   18320005
           END-EVALUATE                                                 18330005
                                                                        18340005
           GOBACK                                                       18350005
           .                                                            18360005
PERIOD****/ \                                                           18370005
      /*****************************************************************18380005
      *               I N I T I A L I Z A T I O N                       18390005
      *                                                                 18400005
      * - OPEN FILES                                                    18410005
      * - CHK FOR SUCCESSFUL OPEN OF FILES                              18420005
      * - WRITE HEADER RECORD (TYPE-1)                                  18430005
      * - READ FIRST TYPE-7 RECORD                                      18440005
      ******************************************************************18450005
                                                                        18460005
       INITIALIZATION.                                                  18470005
                                                                        18480005
AN4993                                                                  18490005
AN4993     IF FIRST-TIME-IN-PROGRAM-Y                                   18500005
               MOVE +0 TO A-TOT-CRMK-REC-IN                             18510005
                          A-TOT-HEADER-RECORDS                          18520005
                          A-TOT-TRAILER-RECORDS                         18530005
                          A-TOT-CRMK-OUT-RECORDS                        18540005
AN4993                    A-BAT-DETAIL-RECORDS-NJ                       18550005
AN4993                    A-BAT-DETAIL-RECORDS-AN                       18560005
MI1323                    A-BAT-DETAIL-RECORDS-CH                       18570005
NJ1164                    A-BAT-DETAIL-RECORDS-CD                       18580005
MA2010                    A-BAT-DETAIL-RECORDS-CK                       18590005
MD2689                    A-BAT-DETAIL-RECORDS-CF                       18600005
MD6619                    A-BAT-DETAIL-RECORDS-CV                       18610005
                          A-BAT-TRL-OOP-AMT-NJ                          18620005
                          A-BAT-TRL-OOP-AMT-AN                          18630005
AN1589                    A-BAT-TRL-OOP-AMT-MG                          18640005
AN0127                    A-BAT-TRL-OOP-AMT-BCN                         18650005
AN1560                    A-BAT-TRL-OOP-AMT-AOR                         18660005
AN1781                    A-BAT-TRL-OOP-AMT-AAR                         18670005
MI1323                    A-BAT-TRL-OOP-AMT-CH                          18680005
NJ1164                    A-BAT-TRL-OOP-AMT-CD                          18690005
MA2010                    A-BAT-TRL-OOP-AMT-CK                          18700005
MD2689                    A-BAT-TRL-OOP-AMT-CF                          18710005
MD6619                    A-BAT-TRL-OOP-AMT-CV                          18720005
                                                                        18730005
MI2155        OPEN INPUT  GENO-FIELD-MAPPING-FILE                       18740005
MI2155                                                                  18750005
MI2155        IF  W-GOOD-OPEN                                           18760005
MI2155            CONTINUE                                              18770005
MI2155        ELSE                                                      18780005
MI2155            DISPLAY 'CDHFLDMP GENO FILE OPEN ERR ' W-FILE-STATUS  18790005
MI2155            PERFORM PROGRAM-ABEND                                 18800005
MI2155        END-IF                                                    18810005
NJ9032                                                                  18820005
NJ9032        INITIALIZE HRHSEHNC-PARMS                                 18830005
                                                                        18840005
MD7684        OPEN OUTPUT OUT-CVSCM-VEN-FILE                            18850005
MD7684                                                                  18860005
MD7684        IF W-GOOD-OPEN                                            18870005
MD7684           CONTINUE                                               18880005
MD7684        ELSE                                                      18890005
MD7684           DISPLAY 'CVSCM VEND FILE OPEN ERR ' W-FILE-STATUS      18900005
MD7684           PERFORM PROGRAM-ABEND                                  18910005
MD7684        END-IF                                                    18920005
MD7684                                                                  18930005
MD7684        WRITE OUT-CVSCM-VEND-RECORDS FROM RPT-ERR-HDR-1ST-LINE    18940005
MD7684                                                                  18950005
AN4993        SET FIRST-TIME-IN-PROGRAM-N  TO TRUE                      18960005
AN4993        PERFORM WRITE-HEADER                                      18970005
AN4993     END-IF                                                       18980005
MA1368                                                                  18990005
MA1368     SET  S-ZERO-DOL-HAD-DOL-N          TO TRUE                   19000005
                                                                        19010005
           .                                                            19020005
PERIOD****/ \                                                           19030005
      /**************************************************************** 19040005
      *                   M A I N L I N E                               19050005
      * - IF NEW MEMBER ID, WRITE RECORD                                19060005
      * - IF NEW ADJ DATE,  WRITE RECORD                                19070005
      * - IF NEW ADJ TYPE,  WRITE RECORD                                19080005
      * - IF SAME MEMBER ID, SAME ADJ DATE AND SAME ADJ TYPE,           19090005
      *   ADD DOLLAR AMOUNTS.                                           19100005
      ******************************************************************19110005
       PROCESS-CLAIM.                                                   19120005
                                                                        19130005
MD838A     MOVE '01'                    TO CLM-INCL-EXCL                19131007
MD838A                                                                  19132007
MD838A     IF (CPE-NEG-ADJ OR CPE-POS-ADJ)     AND                      19133007
MD838A        (GENO-VENDOR-CCARE OR GENO-VENDOR-CVSCM)                  19134007
MD838A                                                                  19135608
MD838A       MOVE 'P'                   TO L-REQUEST-TYPE               19136007
MD838A       MOVE 'LXX'                 TO L-REQUEST-FUNC               19137007
MD838A                                                                  19138007
MD838A       MOVE SUBSCRIBER-PLAN       TO WS-SUBSCRIBER-PLAN           19139007
MD838A       MOVE WS-SUBSCRIBER-PLAN-X  TO LXX-CONTROL-PLAN-CODE        19139107
MD838A       MOVE PND-SUB-NO            TO LXX-SUBSCRIBER-ID            19139207
MD838A       MOVE FST-SVC-DATE(1)       TO LXX-DOS                      19139307
MD161B       MOVE PAT-DOB               TO LXX-PAT-DOB                  19139414
MD161B       MOVE PAT-NAME              TO LXX-PAT-FST-FULL-NAME        19139514
MD838A                                                                  19139607
MD838A       MOVE RECORD-TYPE-XX        TO L-INPUT-PARMS-CVS            19139707
MD838A                                                                  19139807
MD838A       CALL C-ENROLL-PROGRAM USING       HRHSID01-PARM            19139907
MD838A                                         HRHSID01-VALUES          19140007
MD838A       IF L-RETURN-AREA-CVS(1:2)      =  '00'                     19140107
MD838A          MOVE L-RETURN-AREA-CVS(1:02) TO CLM-INCL-EXCL           19140207
MD838A       ELSE                                                       19140307
MD838A          MOVE '01'                    TO CLM-INCL-EXCL           19140407
MD838A       END-IF                                                     19140507
MD838A     ELSE                                                         19140607
MD838A       MOVE '01'                       TO CLM-INCL-EXCL           19140707
MD838A     END-IF                                                       19140807
MD838A                                                                  19140907
MD838A     IF CLAIM-EXCLUDE                                             19141007
MD838A      CONTINUE                                                    19141107
MD838A     ELSE                                                         19141207
MD838A                                                                  19141307
                                                                        19142005
           ADD +1                         TO A-TOT-CRMK-REC-IN          19150005
                                                                        19160005
           IF FIRST-TIME                                                19170005
              MOVE SUBSCRIBER-PLAN        TO W-SUB-PLAN-CODE            19180005
              SET  NOT-FIRST-TIME         TO TRUE                       19190005
                                                                        19200005
           ELSE                                                         19210005
                                                                        19220005
              IF SUBSCRIBER-PLAN NOT = W-SUB-PLAN-CODE                  19230005
                 MOVE SUBSCRIBER-PLAN     TO W-SUB-PLAN-CODE            19240005
              END-IF                                                    19250005
           END-IF                                                       19260005
                                                                        19270005
ANI612     SET  SW-PAID-NO                TO TRUE                       19280005
                                                                        19290005
RDARDA     IF W-ICN = CLAIM-CTL-NO                                      19300005
RDARDA         SET  SW-W-ICN-MATCH-YES    TO TRUE                       19310005
RDARDA     ELSE                                                         19320005
RDARDA         SET  SW-W-ICN-MATCH-NO     TO TRUE                       19330005
RDARDA     END-IF                                                       19340005
RDARDA                                                                  19350005
RDARDA     PERFORM                                                      19360005
RDARDA         VARYING LINE-INDEX FROM +1 BY +1                         19370005
RDARDA         UNTIL LINE-INDEX GREATER CLAIM-LINE-CNT                  19380005
RDARDA             SET W-IDX-HOLD TO LINE-INDEX                         19390005
RDARDA             PERFORM CALL-SUBPRGM-BENEFITS                        19400005
RDARDA             IF SW-BENEFIT-MATCHED                                19410005
RDARDA               IF PROVIDER-GROUP-NUMBER              =            19420005
AN1887                   '780200CAREMARK'                               19430005
MI1323                   OR '7102000CMARK01'                            19440005
MA2010*MA0364            OR '70020000CAREMK'                            19450005
AN6295                   OR '2200BHCOMINGLE'                            19460005
RDARDA                   CONTINUE                                       19470005
AN1887               ELSE                                               19480005
NR0007*AN1589          IF GENO-VENDOR-ACARE OR  GENO-VENDOR-AOPTM       19490005
AN1589                 IF (GENO-VENDOR-ACARE OR  GENO-VENDOR-AOPTM OR   19500005
AN1781                     GENO-VENDOR-AARCH OR                         19510005
AN1560                     GENO-VENDOR-AOPTR OR                         19520005
AN0127                     GENO-VENDOR-MAGLN OR  GENO-VENDOR-BEACN)     19530005
NR0007*AN1560          IF GENO-VENDOR-ACARE AND PROVIDER-GROUP-NUMBER   19540005
AN1560                    IF ((GENO-VENDOR-ACARE OR GENO-VENDOR-AOPTR)  19550005
AN1560                                  AND PROVIDER-GROUP-NUMBER       19560005
AN1887                              NOT EQUAL '220000POSTHIST')         19570005
AN1887                                                                  19580005
AN1887                          PERFORM WRITE-VENDOR-FILE               19590005
AN1887                    ELSE                                          19600005
                                                                        19610005
                                                                        19620005
AN0127                      IF (GENO-VENDOR-AOPTM OR GENO-VENDOR-MAGLN  19630005
AN0127                                            OR GENO-VENDOR-BEACN) 19640005
AN1589                         AND PROVIDER-GROUP-NUMBER                19650005
AN1887                                 NOT EQUAL '2200BHPOSTHIST'       19660005
AN1887                                                                  19670005
AN1887                            PERFORM WRITE-VENDOR-FILE             19680005
AN1781                      ELSE                                        19690005
AN1781                                                                  19700005
AN1781                         IF (GENO-VENDOR-AARCH                    19710005
AN1781                                  AND PROVIDER-GROUP-NUMBER       19720005
AN1781                              NOT EQUAL '220SRXPOSTHIST')         19730005
AN1781                            PERFORM WRITE-VENDOR-FILE             19731005
AN1781                         END-IF                                   19732005
AN1887                      END-IF                                      19733005
AN1887                    END-IF                                        19734005
RDARDA               ELSE                                               19735005
RDARDA                   PERFORM WRITE-VENDOR-FILE                      19736005
AN1887                 END-IF                                           19737005
RDARDA               END-IF                                             19738005
RDARDA             END-IF                                               19739005
RDARDA     END-PERFORM                                                  19740005
MD838A                                                                  19741007
MD838A     END-IF                                                       19742007
AN1887                                                                  19750005
           .                                                            19760005
PERIOD****/ \                                                           19770005
      ******************************************************************19780005
      *                  F I N A L I Z A T I O N                        19790005
      * - WRITE TRAILER RECORD                                          19800005
      * - CLOSE FILES AND CHECK FOR GOOD CLOSE                          19810005
      * - WRITE CONTROL TOTALS LINES                                    19820005
      ******************************************************************19830005
                                                                        19840005
       FINALIZATION.                                                    19850005
                                                                        19860005
                                                                        19870005
           PERFORM WRITE-TRAILER                                        19880005
                                                                        19890005
                                                                        19900005
MI2155     CLOSE GENO-FIELD-MAPPING-FILE                                19910005
MD7684     CLOSE OUT-CVSCM-VEN-FILE                                     19920005
                                                                        19930005
                                                                        19940005
           MOVE  GRAND-TOT-ADJ-AMOUNT TO GRAND-TOT-ADJ-AMOUNT-X         19950005
                                                                        19960005
           DISPLAY '++++++++++++++++++++++++++++++++++++++++++++++',    19970005
                   '++++++++++++'                                       19980005
           DISPLAY '+++++++++++++++++++HRHSAO3C STATISTICS++++++++',    19990005
                   '++++++++++++'                                       20000005
           DISPLAY '++++++++++++++++++++++++++++++++++++++++++++++',    20010005
                   '++++++++++++'                                       20020005
           DISPLAY '+ GRAND TOTAL OF $ AMOUNT                   : '     20030005
                    GRAND-TOT-ADJ-AMOUNT-X ' +'                         20040005
                                                                        20050005
                                                                        20060005
ANI612     DISPLAY  '  AOPTM HEAD COUNT       :'                        20070005
                     WK-AOPTM-HEAD-CNT                                  20080005
ANI612     DISPLAY  '  AOPTM DETAIL COUNT     :'                        20090005
                     WK-AOPTM-DETL-CNT                                  20100005
ANI612     DISPLAY  '  AOPTM TRAILER COUNT    :'                        20110005
                     WK-AOPTM-TRLR-CNT                                  20120005
ANI612     DISPLAY  ' '                                                 20130005
                                                                        20140005
AN1560                                                                  20150005
AN1560     DISPLAY  '  AOPTR HEAD COUNT       :'                        20160005
AN1560               WK-AOPTR-HEAD-CNT                                  20170005
AN1560     DISPLAY  '  AOPTR DETAIL COUNT     :'                        20180005
AN1560               WK-AOPTR-DETL-CNT                                  20190005
AN1560     DISPLAY  '  AOPTR TRAILER COUNT    :'                        20200005
AN1560               WK-AOPTR-TRLR-CNT                                  20210005
AN1560     DISPLAY  ' '                                                 20220005
AN1560                                                                  20230005
AN1781                                                                  20231005
AN1781     DISPLAY  '  AARCH HEAD COUNT       :'                        20232005
AN1781               WK-AARCH-HEAD-CNT                                  20233005
AN1781     DISPLAY  '  AARCH DETAIL COUNT     :'                        20234005
AN1781               WK-AARCH-DETL-CNT                                  20235005
AN1781     DISPLAY  '  AARCH TRAILER COUNT    :'                        20236005
AN1781               WK-AARCH-TRLR-CNT                                  20237005
AN1781     DISPLAY  ' '                                                 20238005
AN1781                                                                  20239005
AN1589     DISPLAY  '  MAGLN HEAD COUNT       :'                        20239105
AN1589               WK-MAGLN-HEAD-CNT                                  20239205
AN1589     DISPLAY  '  MAGLN DETAIL COUNT     :'                        20239305
AN1589               WK-MAGLN-DETL-CNT                                  20239405
AN1589     DISPLAY  '  MAGLN TRAILER COUNT    :'                        20239505
AN1589               WK-MAGLN-TRLR-CNT                                  20239605
AN1589     DISPLAY  ' '                                                 20239705
AN1589                                                                  20239805
AN0127     DISPLAY  '  BEACN HEAD COUNT       :'                        20239905
AN0127               WK-BEACN-HEAD-CNT                                  20240005
AN0127     DISPLAY  '  BEACN DETAIL COUNT     :'                        20250005
AN0127               WK-BEACN-DETL-CNT                                  20260005
AN0127     DISPLAY  '  BEACN TRAILER COUNT    :'                        20270005
AN0127               WK-BEACN-TRLR-CNT                                  20280005
AN0127     DISPLAY  ' '                                                 20290005
AN0127                                                                  20300005
ANI612     DISPLAY  '  ACARE HEAD COUNT       :'                        20310005
                     WK-ACARE-HEAD-CNT                                  20320005
ANI612     DISPLAY  '  ACARE DETAIL COUNT     :'                        20330005
                     WK-ACARE-DETL-CNT                                  20340005
ANI612     DISPLAY  '  ACARE TRAILER COUNT    :'                        20350005
                     WK-ACARE-TRLR-CNT                                  20360005
ANI612     DISPLAY  ' '                                                 20370005
                                                                        20380005
ANI612     DISPLAY  '  CCARE HEAD COUNT       :'                        20390005
                     WK-CCARE-HEAD-CNT                                  20400005
ANI612     DISPLAY  '  CCARE DETAIL COUNT     :'                        20410005
                     WK-CCARE-DETL-CNT                                  20420005
ANI612     DISPLAY  '  CCARE TRAILER COUNT    :'                        20430005
                     WK-CCARE-TRLR-CNT                                  20440005
ANI612     DISPLAY  ' '                                                 20450005
                                                                        20460005
MD6619     DISPLAY  '  CVSCM HEAD COUNT       :'                        20470005
MD6619               WK-CVSCM-HEAD-CNT                                  20480005
MD6619     DISPLAY  '  CVSCM DETAIL COUNT     :'                        20490005
MD6619               WK-CVSCM-DETL-CNT                                  20500005
MD6619     DISPLAY  '  CVSCM TRAILER COUNT    :'                        20510005
MD6619               WK-CVSCM-TRLR-CNT                                  20520005
MD6619     DISPLAY  ' '                                                 20530005
                                                                        20540005
ANI612     DISPLAY  '  MCARE HEAD COUNT       :'                        20550005
                     WK-MCARE-HEAD-CNT                                  20560005
ANI612     DISPLAY  '  MCARE DETAIL COUNT     :'                        20570005
                     WK-MCARE-DETL-CNT                                  20580005
ANI612     DISPLAY  '  MCARE TRAILER COUNT    :'                        20590005
                     WK-MCARE-TRLR-CNT                                  20600005
ANI612     DISPLAY  ' '                                                 20610005
                                                                        20620005
ANI612     DISPLAY  '  MACMK HEAD COUNT       :'                        20630005
                     WK-MACMK-HEAD-CNT                                  20640005
ANI612     DISPLAY  '  MACMK DETAIL COUNT     :'                        20650005
                     WK-MACMK-DETL-CNT                                  20660005
ANI612     DISPLAY  '  MACMK TRAILER COUNT    :'                        20670005
                     WK-MACMK-TRLR-CNT                                  20680005
ANI612     DISPLAY  ' '                                                 20690005
                                                                        20700005
ANI612     DISPLAY  '  NJCDH HEAD COUNT       :'                        20710005
                     WK-NJCDH-HEAD-CNT                                  20720005
ANI612     DISPLAY  '  NJCDH DETAIL COUNT     :'                        20730005
                     WK-NJCDH-DETL-CNT                                  20740005
ANI612     DISPLAY  '  NJCDH TRAILER COUNT    :'                        20750005
                     WK-NJCDH-TRLR-CNT                                  20760005
           DISPLAY ' '                                                  20770005
                                                                        20780005
ANI612     DISPLAY  '  WYETH HEAD COUNT       :'                        20790005
                     WK-WYETH-HEAD-CNT                                  20800005
ANI612     DISPLAY  '  WYETH DETAIL COUNT     :'                        20810005
                     WK-WYETH-DETL-CNT                                  20820005
ANI612     DISPLAY  '  WYETN TRAILER COUNT    :'                        20830005
                     WK-WYETH-TRLR-CNT                                  20840005
           DISPLAY ' '                                                  20850005
           DISPLAY '++++++++++++++++++++++++++++++++++++++++++++++',    20860005
                   '++++++++++++'                                       20870005
           .                                                            20880005
PERIOD****/ \                                                           20890005
                                                                        20900005
      ******************************************************************20910005
      *                  WRITE VENDOR FILE                            **20920005
      ******************************************************************20930005
       WRITE-VENDOR-FILE.                                               20940005
                                                                        20950005
           INITIALIZE CAREMARK-HORIZON-RECORD                           20960005
                                                                        20970005
ANI612     SET  DETAIL-REC,                                             20980005
ANI612          CAR-MED,                                                20990005
ANI612          CAR-DTL-REQUEST           TO TRUE                       21000005
                                                                        21010005
                                                                        21020005
           MOVE PURG-ICN                  TO W-PURG-ICN                 21030005
           MOVE LIN-ID-NO (LINE-INDEX)    TO W-HOLD-LINE                21040005
                                                                        21050005
NJUE03     SET  W-RELATIVE-LINE-NO        TO LINE-INDEX                 21060005
                                                                        21070005
NJUE03     MOVE W-RELATIVE-LINE-NO        TO CAR-CLM-SEQ-NUMBER         21080005
                                                                        21090005
NJ0192     STRING W-PURG-ICN-X (2:14) DELIMITED BY SIZE                 21100005
NJ0192            CAR-REC-ID-TIMEJ    DELIMITED BY SIZE                 21110005
NJ0192            CAR-REC-ID-TIMEH    DELIMITED BY SIZE                 21120005
NJ0192            W-HOLD-LINE         DELIMITED BY SIZE                 21130005
NJ0192     INTO CAR-REC-ID                                              21140005
                                                                        21150005
NJ0192     END-STRING                                                   21160005
                                                                        21170005
NR0099     MOVE ACTIV-PLAN-CODE        TO WS-ACTIV-PLAN-CODE-NEW        21180005
NR0099     MOVE WS-ACTIV-PLAN-CODE-NEW TO CAR-REC-ID(48:3)              21190005
AN1308     IF (GENO-VENDOR-ACARE OR  GENO-VENDOR-AOPTM OR               21200005
AN1560         GENO-VENDOR-AOPTR OR                                     21210005
AN1781         GENO-VENDOR-AARCH OR                                     21220005
AN1308         GENO-VENDOR-MAGLN OR  GENO-VENDOR-BEACN)                 21230005
AN1308         MOVE ACTIV-PLAN-CODE    TO WS-ACTIV-PLAN-CODE            21240005
AN1308         MOVE WS-ACTIV-PLAN-CODE TO CAR-REC-ID(48:3)              21250005
AN1308     END-IF                                                       21260005
           MOVE SPACES                    TO CAR-REC-RESPOND-STATUS     21270005
                                             CAR-REC-REJ-CODE           21280005
                                                                        21290005
                                                                        21300005
******* WHEN TESTING IN MO   USE THE TESTSW                             21310005
******* WHEN MOVING  TO PROD USE THE PRODSW                             21320005
                                                                        21330005
AN4993     SET  CAR-REC-PRODUTION-DATA,                                 21340005
                CAR-REC-INIT-TRANS,                                     21350005
                CAR-REC-FIRST-TRANS       TO TRUE                       21360005
                                                                        21370005
MA1368     IF CLAIM-CTL-NO    =       W-HOLD-CLAIM-CTL-NO               21380005
MA1368       CONTINUE                                                   21390005
MA1368     ELSE                                                         21400005
MA1368        SET  S-ZERO-DOL-HAD-DOL-N          TO TRUE                21410005
MA1368        MOVE CLAIM-CTL-NO                  TO                     21420005
MA1368             W-HOLD-CLAIM-CTL-NO                                  21430005
MA1368     END-IF                                                       21440005
                                                                        21450005
AN4993                                                                  21460005
AN4993     EVALUATE TRUE                                                21470005
                                                                        21480005
NR0007         WHEN GENO-VENDOR-ACARE                                   21490005
AN882B              MOVE SUB-CERT-BASE    TO CAR-PATIENT-ID(1:9)        21500005
AN4993              MOVE A-SENDER-ID      TO CAR-REC-SENDER-ID          21510005
AN4993              MOVE A-SENDER-NAME    TO CAR-REC-SENDER-NAME        21520005
AN4993              MOVE A-RECEIVER-ID    TO CAR-REC-RECIEVER-ID        21530005
AN4993              MOVE A-RECEIVER-NAME  TO CAR-REC-RECIEVER-NAME      21540005
AN1855              PERFORM 3000-START-MAPPING-FILE                     21550005
AN1855                                                                  21560005
AN1855              IF MAPPING-FOUND                                    21570005
                                                                        21580005
AN1855                 SET   NOT-CHANGE-OF-KEY TO TRUE                  21590005
                                                                        21600005
AN1855                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         21610005
AN1855                         CHANGE-OF-KEY OR CLINT-ID-FND            21620005
AN1855              END-IF                                              21630005
                                                                        21640005
NR0007         WHEN GENO-VENDOR-MCARE                                   21650005
                                                                        21660005
AN8824              MOVE SUB-SOC-SEC-NO     TO CAR-PATIENT-ID           21670005
MI1323              MOVE CHR-SENDER-ID      TO CAR-REC-SENDER-ID        21680005
MI1323              MOVE CHR-SENDER-NAME    TO CAR-REC-SENDER-NAME      21690005
MI1323              MOVE CHR-RECEIVER-ID    TO CAR-REC-RECIEVER-ID      21700005
MI1323              MOVE CHR-RECEIVER-NAME  TO CAR-REC-RECIEVER-NAME    21710005
MI1323              MOVE CHR-ID-NAME        TO CAR-REC-ID-NAME          21720005
                                                                        21730005
NR0007         WHEN GENO-VENDOR-NJCDH                                   21740005
                                                                        21750005
NJ1164              MOVE SUB-SOC-SEC-NO    TO CAR-PATIENT-ID            21760005
NJ1164              MOVE NJ-SENDER-ID      TO CAR-REC-SENDER-ID         21770005
NJ1164              MOVE NJ-SENDER-NAME    TO CAR-REC-SENDER-NAME       21780005
NJ1164              MOVE NJ-RECEIVER-ID    TO CAR-REC-RECIEVER-ID       21790005
NJ1164              MOVE NJ-RECEIVER-NAME  TO CAR-REC-RECIEVER-NAME     21800005
NJ1164              MOVE NJ-ID-NAME        TO CAR-REC-ID-NAME           21810005
                                                                        21820005
NR0007         WHEN GENO-VENDOR-AOPTM                                   21830005
                                                                        21840005
AN1887              MOVE SUB-CERT-BASE       TO CAR-PATIENT-ID(1:9)     21850005
AN1887              MOVE AN-SENDER-ID        TO CAR-REC-SENDER-ID       21860005
AN1887              MOVE AN-SENDER-NAME      TO CAR-REC-SENDER-NAME     21870005
AN1887              MOVE AN-RECEIVER-ID      TO CAR-REC-RECIEVER-ID     21880005
AN1887              MOVE AN-RECEIVER-NAME    TO CAR-REC-RECIEVER-NAME   21890005
AN1887                                                                  21900005
AN1887              PERFORM 3000-START-MAPPING-FILE                     21910005
AN1887                                                                  21920005
AN1887              IF MAPPING-FOUND                                    21930005
                                                                        21940005
AN1855                 SET   NOT-CHANGE-OF-KEY TO TRUE                  21950005
                                                                        21960005
AN1887                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         21970005
AN1887                      CHANGE-OF-KEY OR CLINT-ID-FND               21980005
AN1887              END-IF                                              21990005
AN1887                                                                  22000005
AN1560         WHEN GENO-VENDOR-AOPTR                                   22010005
AN1560                                                                  22020005
AN1560              MOVE SUB-CERT-BASE        TO CAR-PATIENT-ID(1:9)    22030005
AN1560              MOVE ANR-SENDER-ID        TO CAR-REC-SENDER-ID      22040005
AN1560              MOVE ANR-SENDER-NAME      TO CAR-REC-SENDER-NAME    22050005
AN1560              MOVE ANR-RECEIVER-ID      TO CAR-REC-RECIEVER-ID    22060005
AN1560              MOVE ANR-RECEIVER-NAME    TO CAR-REC-RECIEVER-NAME  22070005
AN1560                                                                  22080005
AN1560              PERFORM 3000-START-MAPPING-FILE                     22090005
AN1560                                                                  22100005
AN1560              IF MAPPING-FOUND                                    22110005
AN1560                                                                  22120005
AN1560                 SET   NOT-CHANGE-OF-KEY TO TRUE                  22130005
AN1560                                                                  22140005
AN1560                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         22150005
AN1560                      CHANGE-OF-KEY OR CLINT-ID-FND               22160005
AN1560              END-IF                                              22170005
AN1560                                                                  22180005
AN1781         WHEN GENO-VENDOR-AARCH                                   22190005
AN1781                                                                  22200005
AN1781              MOVE SUB-CERT-BASE        TO CAR-PATIENT-ID(1:9)    22210005
AN1781              MOVE AAR-SENDER-ID        TO CAR-REC-SENDER-ID      22220005
AN1781              MOVE AAR-SENDER-NAME      TO CAR-REC-SENDER-NAME    22230005
AN1781              MOVE AAR-RECEIVER-ID      TO CAR-REC-RECIEVER-ID    22240005
AN1781              MOVE AAR-RECEIVER-NAME    TO CAR-REC-RECIEVER-NAME  22250005
AN1781                                                                  22260005
AN1781              PERFORM 3000-START-MAPPING-FILE                     22270005
AN1781                                                                  22280005
AN1781              IF MAPPING-FOUND                                    22290005
AN1781                                                                  22300005
AN1781                 SET   NOT-CHANGE-OF-KEY TO TRUE                  22310005
AN1781                                                                  22320005
AN1781                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         22330005
AN1781                      CHANGE-OF-KEY OR CLINT-ID-FND               22340005
AN1781              END-IF                                              22350005
AN1781                                                                  22360005
AN1589         WHEN GENO-VENDOR-MAGLN                                   22370005
AN1589                                                                  22380005
AN1589              MOVE SUB-CERT-BASE        TO CAR-PATIENT-ID(1:9)    22390005
AN1589              MOVE AN-SENDER-ID-MAG     TO CAR-REC-SENDER-ID      22400005
AN1589              MOVE AN-SENDER-NAME-MAG   TO CAR-REC-SENDER-NAME    22410005
AN1589              MOVE AN-RECEIVER-ID-MAG   TO CAR-REC-RECIEVER-ID    22420005
AN1589              MOVE AN-RECEIVER-NAME-MAG TO CAR-REC-RECIEVER-NAME  22430005
AN1589                                                                  22440005
AN1589              PERFORM 3000-START-MAPPING-FILE                     22450005
AN1589                                                                  22460005
AN1589              IF MAPPING-FOUND                                    22470005
AN1589                                                                  22480005
AN1589                 SET   NOT-CHANGE-OF-KEY TO TRUE                  22490005
AN1589                                                                  22500005
AN1589                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         22510005
AN1589                      CHANGE-OF-KEY OR CLINT-ID-FND               22520005
AN1589              END-IF                                              22530005
AN1589                                                                  22540005
AN0127         WHEN GENO-VENDOR-BEACN                                   22550005
AN0127                                                                  22560005
AN0127              MOVE SUB-CERT-BASE        TO CAR-PATIENT-ID(1:9)    22570005
AN0127              MOVE AN-SENDER-ID-BCN     TO CAR-REC-SENDER-ID      22580005
AN0127              MOVE AN-SENDER-NAME-BCN   TO CAR-REC-SENDER-NAME    22590005
AN0127              MOVE AN-RECEIVER-ID-BCN   TO CAR-REC-RECIEVER-ID    22600005
AN0127              MOVE AN-RECEIVER-NAME-BCN TO CAR-REC-RECIEVER-NAME  22610005
AN0127                                                                  22620005
AN0127              PERFORM 3000-START-MAPPING-FILE                     22630005
AN0127                                                                  22640005
AN0127              IF MAPPING-FOUND                                    22650005
AN0127                                                                  22660005
AN0127                 SET   NOT-CHANGE-OF-KEY TO TRUE                  22670005
AN0127                                                                  22680005
AN0127                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         22690005
AN0127                      CHANGE-OF-KEY OR CLINT-ID-FND               22700005
AN0127              END-IF                                              22710005
AN0127                                                                  22720005
NR0007         WHEN GENO-VENDOR-MACMK                                   22730005
                                                                        22740005
MA2010              INITIALIZE HRHSMASN-PARMS                           22750005
                                                                        22760005
MA2010              SET  L-PROCESSING-CALL,                             22770005
MA2010                   L-GET-XREF-ID       TO TRUE                    22780005
                                                                        22790005
MA2010*MA5057       MOVE PND-SUB-NO          TO L-INPUT-SSN             22800005
MA5057              MOVE GENO-VENDOR-ID      TO L-VENDOR-FMT            22810005
MA8679              IF SUB-SOC-SEC-NO = ZEROS                           22820005
MA8679                IF PND-SUB-NO (3:1) NOT NUMERIC                   22830005
MA5057                 MOVE PND-SUB-NO       TO L-INPUT-SSN             22840005
MA8679                ELSE                                              22850005
MA5057                 MOVE PND-SUB-NO(1:9)  TO L-INPUT-SSN(1:9)        22860005
MA5057                 MOVE SPACES           TO L-INPUT-SSN(10:4)       22870005
MA8679                END-IF                                            22880005
                                                                        22890005
MA8679                CALL W-HRHSMASN USING HRHSMASN-PARMS              22900005
                                                                        22910005
MA8679                IF L-RETURN-CODE = '00'                           22920005
MA2010                 MOVE L-RETURN-SSN     TO CAR-PATIENT-ID          22930005
MA8679                ELSE                                              22940005
MA8679                 MOVE PND-SUB-NO (1:9) TO CAR-PATIENT-ID          22950005
MA2010                END-IF                                            22960005
MA8679              ELSE                                                22970005
MA8679                MOVE SUB-SOC-SEC-NO    TO CAR-PATIENT-ID          22980005
MA8679              END-IF                                              22990005
                                                                        23000005
MA2010              MOVE CMK-SENDER-ID       TO CAR-REC-SENDER-ID       23010005
MA2010              MOVE CMK-SENDER-NAME     TO CAR-REC-SENDER-NAME     23020005
MA2010              MOVE CMK-RECEIVER-ID     TO CAR-REC-RECIEVER-ID     23030005
MA2010              MOVE CMK-RECEIVER-NAME   TO CAR-REC-RECIEVER-NAME   23040005
MA2010                                                                  23050005
                                                                        23060005
MA2010              INITIALIZE HRHSMASN-PARMS                           23070005
                                                                        23080005
MA2010              SET  L-PROCESSING-CALL,                             23090005
MA2010                   L-GET-CLIENT-ID     TO TRUE                    23100005
                                                                        23110005
MA2010              MOVE SUBSCRIBER-PLAN     TO L-SUB-PLAN              23120005
MA2010              MOVE GENO-VENDOR-ID      TO L-VENDOR-FMT            23130005
                                                                        23140005
MA2010              MOVE GRP-BASE(LINE-INDEX)TO L-GP-BASE               23150005
MA2010              MOVE GRP-SEC(LINE-INDEX) TO L-GP-SEC                23160005
                                                                        23170005
MA2010              MOVE PACKAGE-CODE(LINE-INDEX)  TO W-PKG-LOW-ORDER   23180005
MA2010              MOVE W-PKG-BINARY        TO W-PACKAGE-CODE-X        23190005
MA2010              MOVE W-PKG-CODE-X        TO L-GP-PKG                23200005
                                                                        23210005
MA2010              CALL W-HRHSMASN USING HRHSMASN-PARMS                23220005
                                                                        23230005
MA2010              IF L-RETURN-CODE = '00'                             23240005
MA2010                 MOVE L-RETURN-CLIENT-ID                          23250005
MA2010                                       TO CAR-REC-ID-NAME         23260005
MA2010              END-IF                                              23270005
                                                                        23280005
NR0007         WHEN GENO-VENDOR-CCARE                                   23290005
MD7568              IF SUB-SOC-SEC-NO IS EQUAL TO ZEROES                23300005
MD7568                 PERFORM GET-SSN-FROM-ELIGIBILITY                 23310005
MD7568              ELSE                                                23320005
MD2689                 MOVE SUB-SOC-SEC-NO      TO W-PEND-SSN           23330005
MD7568              END-IF                                              23340005
MD2689              MOVE W-PEND-SSN          TO CAR-PATIENT-ID          23350005
MD2689              MOVE CCK-SENDER-ID       TO CAR-REC-SENDER-ID       23360005
MD2689              MOVE CCK-SENDER-NAME     TO CAR-REC-SENDER-NAME     23370005
MD2689              MOVE CCK-RECEIVER-ID     TO CAR-REC-RECIEVER-ID     23380005
MD2689              MOVE CCK-RECEIVER-NAME   TO CAR-REC-RECIEVER-NAME   23390005
MD2689                                                                  23400005
MD2689              PERFORM 3000-START-MAPPING-FILE                     23410005
MD2689                                                                  23420005
MD2689              IF MAPPING-FOUND                                    23430005
                                                                        23440005
MD2689                 SET   NOT-CHANGE-OF-KEY TO TRUE                  23450005
MD2689                 PERFORM 3100-READ-NEXT-MAPPING-REC UNTIL         23460005
MD2689                      CHANGE-OF-KEY OR CLINT-ID-FND               23470005
MD2689              END-IF                                              23480005
                                                                        23490005
NR0007         WHEN GENO-VENDOR-CVSCM                                   23500005
MD6619*             BUILDS CAR-PATIEND-ID BY CALLING HRHSID01           23510005
MD6619              PERFORM POPULATE-MBR-ID-NO-MBR-S                    23520005
MD6619              MOVE CVS-SENDER-ID       TO CAR-REC-SENDER-ID       23530005
MD6619              MOVE CVS-SENDER-NAME     TO CAR-REC-SENDER-NAME     23540005
MD6619              MOVE CVS-RECEIVER-ID     TO CAR-REC-RECIEVER-ID     23550005
MD6619              MOVE CVS-RECEIVER-NAME   TO CAR-REC-RECIEVER-NAME   23560005
MD6619              MOVE CVS-CAR-REC-ID-NAME TO CAR-REC-ID-NAME         23570005
MD6619                                                                  23580005
AN4993         WHEN OTHER                                               23590005
                                                                        23600005
AN882A              MOVE SUB-SOC-SEC-NO   TO CAR-PATIENT-ID             23610005
AN4993              MOVE C-SENDER-ID      TO CAR-REC-SENDER-ID          23620005
AN4993              MOVE C-SENDER-NAME    TO CAR-REC-SENDER-NAME        23630005
AN4993              MOVE C-RECEIVER-ID    TO CAR-REC-RECIEVER-ID        23640005
AN4993              MOVE C-RECEIVER-NAME  TO CAR-REC-RECIEVER-NAME      23650005
AN4993              MOVE C-ID-NAME        TO CAR-REC-ID-NAME            23660005
                                                                        23670005
AN4993     END-EVALUATE                                                 23680005
AN4993                                                                  23690005
                                                                        23700005
   *****                                                                23710005
   ***** PATIENT                                                        23720005
   *****                                                                23730005
           MOVE PAT-DOB                   TO W-DATE-1-FW                23740005
           MOVE C-BINARY-DATE             TO W-FORMAT-1                 23750005
           MOVE C-YYYYMMDD                TO W-FORMAT-2                 23760005
           MOVE C-CONVERT-CODE            TO W-FUNCTION-CODE            23770005
                                                                        23780005
           PERFORM CONVERT-DATE                                         23790005
                                                                        23800005
           MOVE W-DATE-2-8                TO CAR-PATIENT-DOB-YYCCMMDD   23810005
                                                                        23820005
           UNSTRING  PAT-NAME DELIMITED BY '*'                          23830005
              INTO CAR-PATIENT-FIRST-NAME                               23840005
                   CAR-PATIENT-MIDDLE-INIT                              23850005
                   CAR-PATIENT-LAST-NAME                                23860005
           END-UNSTRING                                                 23870005
                                                                        23880005
           MOVE SEX-REL                   TO W-SEX-REL                  23890005
           MOVE W-SEX-REL                 TO BYTE-WORK                  23900005
           SET  BX                        TO BINARY-BYTE                23910005
           MOVE BIT-BYTE (BX)             TO SEX-REL-SAVE               23920005
                                                                        23930005
           EVALUATE TRUE                                                23940005
              WHEN SEX-REL-MALE                                         23950005
                 SET PATIENT-MALE         TO TRUE                       23960005
              WHEN SEX-REL-FEMALE                                       23970005
                 SET PATIENT-FEMALE       TO TRUE                       23980005
              WHEN OTHER                                                23990005
                 SET PATIENT-UNKNOWN      TO TRUE                       24000005
           END-EVALUATE                                                 24010005
                                                                        24020005
           EVALUATE TRUE                                                24030005
              WHEN SEX-REL-DEPENDENT                                    24040005
                 SET PATIENT-CHILD        TO TRUE                       24050005
              WHEN SEX-REL-SPOUSE                                       24060005
                 SET PATIENT-SPOUSE       TO TRUE                       24070005
              WHEN SEX-REL-SUBSCRIBER                                   24080005
                 SET PATIENT-CARDHOLDER   TO TRUE                       24090005
              WHEN OTHER                                                24100005
                 SET PATIENT-OTHER        TO TRUE                       24110005
           END-EVALUATE                                                 24120005
                                                                        24130005
MA5098*    WHEN THE PATIENT RELATIONSHIP IS SPOUSE CHECK MEMBERSHIP     24140005
MA5098*    FOR DOMESTIC PARTNER, MEMBER STATUS '05'.  DOMESTIC          24150005
MA5098*    PARTNERS SHOULD MAP F4 OR M4.                                24160005
MA5098*                                                                 24170005
MA5098                                                                  24180005
NR0007     IF GENO-VENDOR-MACMK AND SUBSCRIBER-PLAN = 700               24190005
MA5098                                                                  24200005
MA5098       IF SEX-REL-SPOUSE                                          24210005
MA5098                                                                  24220005
MA5098         MOVE 'P'                       TO L-REQUEST-TYPE         24230005
MA5098         MOVE 'CMK'                     TO L-REQUEST-FUNC         24240005
MA5098                                                                  24250005
MA5098         MOVE SUBSCRIBER-PLAN           TO WS-SUBSCRIBER-PLAN     24260005
MA5098         MOVE WS-SUBSCRIBER-PLAN-X      TO CMK-CONTROL-PLAN-CODE  24270005
MA5098         MOVE PND-SUB-NO                TO CMK-SUBSCRIBER-ID      24280005
MA5098         MOVE 'X'                       TO CMK-FILE-TYPE          24290005
MA5098         MOVE PAT-DOB                   TO CMK-PAT-DOB            24300005
MA5098         MOVE W-SEX-REL                 TO CMK-PAT-SEX-REL        24310005
MA5098         MOVE FST-SVC-DATE(LINE-INDEX)  TO CMK-DOS                24320005
MA5098                                                                  24330005
MA5098         MOVE RECORD-TYPE-CM            TO L-INPUT-PARMS-CVS      24340005
MA5098                                                                  24350005
MA5098         CALL C-ENROLL-PROGRAM USING       HRHSID01-PARM          24360005
MA5098                                           HRHSID01-VALUES        24370005
MA5098                                                                  24380005
MA5098         IF L-RETURN-CODE-CVS           =  '000'                  24390005
MA5098                                                                  24400005
MA5098           IF L-RETURN-AREA-CVS(1:02)   =  ZEROS                  24410005
MA5098                                                                  24420005
MA5098             CONTINUE                                             24430005
MA5098                                                                  24440005
MA5098           ELSE                                                   24450005
MA5098                                                                  24460005
MA5098             MOVE L-RETURN-AREA-CVS(1:01) TO CAR-PATIENT-GENDER   24470005
MA5098             MOVE L-RETURN-AREA-CVS(2:01) TO                      24480005
MA5098                                        CAR-PATIENT-RELATIONSHIP  24490005
MA5098           END-IF                                                 24500005
MA5098         ELSE                                                     24510005
MA5098           CONTINUE                                               24520005
MA5098         END-IF                                                   24530005
MA5098       ELSE                                                       24540005
MA5098          CONTINUE                                                24550005
MA5098       END-IF                                                     24560005
MA5098     ELSE                                                         24570005
MA5098        CONTINUE                                                  24580005
MA5098     END-IF                                                       24590005
MA5098                                                                  24600005
           MOVE SPACES                    TO CAR-CARRIER-NUMBER         24610005
                                             CAR-PARTICIPANT-ACCT-NUMBER24620005
                                                                        24630005
           MOVE MEMBERSHIP-GROUP-NO(LINE-INDEX)                         24640005
                                          TO CAR-GROUP-NUMBER           24650005
NJ0192     STRING W-PURG-ICN-X (2:14) DELIMITED BY SIZE                 24660005
NJ0192            CAR-CLM-ID-TIMEJ DELIMITED BY SIZE                    24670005
NJ0192            CAR-CLM-ID-TIMEH DELIMITED BY SIZE                    24680005
NJ0192     INTO CAR-CLM-ID                                              24690005
NJ0192     END-STRING                                                   24700005
                                                                        24710005
           MOVE LIN-ID-NO (LINE-INDEX)    TO W-HOLD-LINE                24720005
NJUE03     SET  W-RELATIVE-LINE-NO        TO LINE-INDEX                 24730005
NJUE03     MOVE W-RELATIVE-LINE-NO        TO CAR-CLM-SEQ-NUMBER         24740005
                                                                        24750005
           IF CPE-NEG-ADJ OR CPE-POS-ADJ THEN                           24760005
              SET  CAR-CLM-ADJUSTMENT     TO TRUE                       24770005
           ELSE                                                         24780005
              SET  CAR-CLM-PAY            TO TRUE                       24790005
           END-IF                                                       24800005
                                                                        24810005
********** FIRST DATE OF SERVICE                                        24820005
                                                                        24830005
           MOVE FST-SVC-DATE(LINE-INDEX)  TO W-DATE-1-FW                24840005
           MOVE C-BINARY-DATE             TO W-FORMAT-1                 24850005
           MOVE C-YYYYMMDD                TO W-FORMAT-2                 24860005
           MOVE C-CONVERT-CODE            TO W-FUNCTION-CODE            24870005
                                                                        24880005
           PERFORM CONVERT-DATE                                         24890005
                                                                        24900005
           MOVE W-DATE-2-8           TO CAR-CLM-DTE-SERVICE-CCYYMMDD    24901005
                                                                        24902005
           ACCEPT W-YYMMDD FROM DATE                                    24903005
           ACCEPT W-HHMMSS FROM TIME                                    24904005
           MOVE W-HHMMSS-REDEF            TO W-HHMMSS-X                 24905005
           MOVE W-TIME-HOLD               TO CAR-CLM-TME-POST-HHMMSSMMMM24906005
                                                                        24907005
           MOVE C-YYMMDD                  TO W-FORMAT-1                 24908005
           MOVE C-YYYYMMDD                TO W-FORMAT-2                 24909005
           MOVE C-CONVERT-CODE            TO W-FUNCTION-CODE            24910005
           MOVE W-YYMMDD                  TO W-DATE-1                   24920005
           PERFORM CONVERT-DATE                                         24930005
           MOVE W-DATE-2(1:8) TO CAR-CLM-DTE-POST-CCYYMMDD              24940005
                                                                        24950005
           SET  CAR-DOLLAR-AMT            TO TRUE                       24960005
                                                                        24970005
           MOVE SPACES            TO   CAR-TOT-MED-PHR-DED-AMT          24980005
                                       CAR-TOT-MED-PHAR-MOOP-AMT        24990005
                                       CAR-PARTICIPANT-DEDUCT-FLAG      25000005
                                       CAR-FAMILY-DEDUCT-FLAG           25010005
                                       CAR-PARTICIPANT-OOP-FLAG         25020005
                                       CAR-FAMILY-OOP-FLAG              25030005
                                       CAR-PARTICIPANT-DEDUCT-FLAG      25040005
                                                                        25050005
           MOVE +0                TO   CAR-BENEFIT-BEG-DTE-CCYYMMDD     25060005
                                       CAR-BENEFIT-END-DTE-CCYYMMDD     25070005
                                       CAR-REC-TRANS-COUNTER            25080005
                                       CAR-PATIENT-PAY-EXCL-DED-AMT     25081005
                                       CAR-DED-AMT                      25082005
                                       CAR-TOT-MED-PHARMACY-DED-AMT     25083005
                                       CAR-OOP-AMT                      25084005
                                       CAR-TOT-MED-PHR-MOOP-AMT         25085005
                                       W-COINSURANCE                    25086005
                                       W-COPAYMENT-AMT                  25087005
                                       W-DED-AMT-DOLLAR                 25088005
                                       W-OUT-OF-POCKET                  25089005
                                       W-LIFETIME-MAX                   25089105
                                       W-COPAYMENT-AMT                  25089205
                                       W-DED-AMT-DOLLAR                 25089305
                                       W-OUT-OF-POCKET                  25089405
                                       W-LIFETIME-MAX                   25089505
                                                                        25089605
           PERFORM 2750-PROCESS-CLAIM-LINE                              25089705
                                                                        25089805
NJ9032**                                                                25089905
NJ9032** DON'T CALL THE 4001-CALL-HRHSEHNC AGAIN                        25090005
NJ9032** IF LINES HAVE DUPLICATE KEY DATA  USE WHAT IT                  25100005
NJ9032** RETURNED BEFORE FOR PREVIOUS LINE                              25110005
NJ9032**                                                                25120005
NJ9032     IF GENO-VENDOR-NJCDH                                         25130005
NJ9032        MOVE PACKAGE-CODE(LINE-INDEX) TO W-PKG-LOW-ORDER          25140005
NJ9032        IF SUBSCRIBER-PLAN           = L-MOOP-HNDI-SUB-PLAN   AND 25150005
NJ9032           GRP-BASE(LINE-INDEX)      = L-MOOP-HNDI-GROUP-BASE AND 25160005
NJ9032           GRP-SEC(LINE-INDEX)       = L-MOOP-HNDI-GROUP-SECT AND 25170005
NJ9032           W-PKG-BINARY              = L-MOOP-HNDI-PACK-CODE  AND 25180005
NJ9032           GENO-VENDOR-ID            = L-MOOP-HNDI-VENDOR-ID  AND 25190005
NJ9032           CAR-CLM-DTE-SERVICE-CCYYMMDD = L-MOOP-HNDI-DOS         25200005
NJ9032           CONTINUE                                               25210005
NJ9032        ELSE                                                      25220005
NJ9032           PERFORM 4001-CALL-HRHSEHNC                             25230005
NJ9032           IF L-MOOP-HNDO-MATCH                                   25240005
NJ9032              MOVE L-MOOP-HNDO-RENEWAL-DATE TO                    25250005
NJ9032                          HOLD-RENEWAL-DATE                       25260005
NJ9032           END-IF                                                 25270005
NJ9032        END-IF                                                    25280005
NJ9032        IF L-MOOP-HNDO-4THQTR-IND = 'Y'                           25290005
NJ9032           MOVE W-DED-AMT-DOLLAR TO W-OUT-OF-POCKET               25300005
NJ9032           MOVE 0 TO W-DED-AMT-DOLLAR                             25310005
NJ9032        END-IF                                                    25320005
NJ9032     END-IF                                                       25330005
NJ9032                                                                  25340005
NR0007     IF (CPE-NEG-ADJ AND GENO-POST-CALL)      OR                  25350005
NR0007        (GENO-BACKOUT-CALL AND NOT CPE-NEG-ADJ)                   25360005
                                                                        25370005
               COMPUTE CAR-PATIENT-PAY-EXCL-DED-AMT =                   25380005
                   CAR-PATIENT-PAY-EXCL-DED-AMT * -1                    25390005
                                                                        25400005
               COMPUTE CAR-DED-AMT = W-DED-AMT-DOLLAR * -1              25410005
                                                                        25420005
               COMPUTE CAR-OOP-AMT = W-OUT-OF-POCKET * -1               25430005
                                                                        25440005
               COMPUTE CAR-SPONSOR-PLAN-PAID-AMT =                      25450005
                       W-LIFETIME-MAX            * -1                   25460005
           ELSE                                                         25470005
               COMPUTE CAR-DED-AMT = W-DED-AMT-DOLLAR                   25480005
               COMPUTE CAR-OOP-AMT = W-OUT-OF-POCKET                    25490005
               COMPUTE CAR-SPONSOR-PLAN-PAID-AMT =                      25500005
                       W-LIFETIME-MAX                                   25510005
           END-IF                                                       25520005
                                                                        25530005
           COMPUTE CAR-PATIENT-PAY-EXCL-DED-AMT = +0                    25540005
MA1368                                                                  25550005
MA1368     IF CAR-DED-AMT EQUAL +0 AND                                  25560005
MA1368        CAR-OOP-AMT EQUAL +0 AND                                  25570005
MA1368        CAR-SPONSOR-PLAN-PAID-AMT = +0                            25580005
MA1368                                                                  25590005
                                                                        25600005
MA1368        IF    S-ZERO-DOL-HAD-DOL-Y                                25610005
MA1368           CONTINUE                                               25620005
MA1368        ELSE                                                      25630005
MA1368           IF BNCH-ID  =  'R' OR 'Z'                              25640005
MA1368              SET  S-ZERO-DOL-HAD-DOL-N          TO TRUE          25650005
MA1368              MOVE  'Z'               TO                          25660005
MA1368                    BNCH-ID                                       25670005
MA1368           ELSE                                                   25680005
MA1368              CONTINUE                                            25690005
MA1368           END-IF                                                 25700005
MA1368        END-IF                                                    25710005
MA1368     ELSE                                                         25720005
MA1368                                                                  25730005
MA1368        IF BNCH-ID  =  'Z'                                        25740005
MA1368           SET  S-ZERO-DOL-HAD-DOL-Y          TO TRUE             25750005
MA1368           MOVE  'R'               TO                             25760005
MA1368                 BNCH-ID                                          25770005
MA1368        END-IF                                                    25780005
                                                                        25790005
MA1368        SET  S-ZERO-DOL-HAD-DOL-Y          TO TRUE                25800005
MA1368                                                                  25810005
MA1368        MOVE CLAIM-CTL-NO       TO W-ICN                          25820005
MA1368                                                                  25830005
MA1368                                                                  25840005
MA1368*                                                                 25850005
MA1368*    IF CAR-DED-AMT EQUAL 0 AND                                   25860005
MA1368*       CAR-OOP-AMT EQUAL 0 AND                                   25870005
MA1368*       CAR-SPONSOR-PLAN-PAID-AMT = +0                            25880005
MA1368*           CONTINUE                                              25890005
MA1368*    ELSE                                                         25900005
MA1368*      MOVE CLAIM-CTL-NO        TO W-ICN                          25910005
MA2010       EVALUATE TRUE                                              25920005
NR0007          WHEN GENO-VENDOR-ACARE                                  25930005
MA2010               ADD +1          TO A-BAT-DETAIL-RECORDS-AN         25940005
NR0007          WHEN GENO-VENDOR-AOPTM                                  25950005
MA2010               ADD +1          TO A-BAT-DETAIL-REC-ANOPT          25960005
AN1589          WHEN GENO-VENDOR-MAGLN                                  25970005
AN1589               ADD +1          TO AN-BAT-DETAIL-RECMAGL-MG        25980005
AN0127          WHEN GENO-VENDOR-BEACN                                  25990005
AN0127               ADD +1          TO AN-BAT-DETAIL-RECBCN-BC         26000005
AN1560          WHEN GENO-VENDOR-AOPTR                                  26010005
AN1560               ADD +1          TO AN-BAT-DETAIL-RECOPTR-OR        26020005
AN1781          WHEN GENO-VENDOR-AARCH                                  26030005
AN1781               ADD +1          TO AN-BAT-DETAIL-RECARCH-OR        26040005
NR0007          WHEN GENO-VENDOR-CCARE                                  26041005
MD2689               ADD +1          TO A-BAT-DETAIL-RECORDS-CF         26042005
NR0007          WHEN GENO-VENDOR-CVSCM                                  26043005
MD6619               ADD +1          TO A-BAT-DETAIL-RECORDS-CV         26044005
NR0007          WHEN GENO-VENDOR-MCARE                                  26045005
MA2010               ADD +1          TO A-BAT-DETAIL-RECORDS-CH         26046005
NR0007          WHEN GENO-VENDOR-MACMK                                  26047005
MA2010               ADD +1          TO A-BAT-DETAIL-RECORDS-CK         26048005
NR0007          WHEN GENO-VENDOR-NJCDH                                  26049005
MA2010               ADD +1          TO A-BAT-DETAIL-RECORDS-CD         26050005
MA2010          WHEN OTHER                                              26060005
MA2010               ADD +1          TO A-BAT-DETAIL-RECORDS-NJ         26070005
MA2010       END-EVALUATE                                               26080005
MA2010                                                                  26090005
              ADD +1                       TO A-TOT-CRMK-OUT-RECORDS    26100005
NR0007*                                       GENO-TYPE7-WRITTEN        26110005
MD7684       IF SUBSCRIBER-PLAN = 580 OR 690  AND GENO-VENDOR-CVSCM     26120005
MD7684          PERFORM 4000-MAP-FROM-HDRT5W2K                          26130005
MD7684       END-IF                                                     26140005
MD7684                                                                  26150005
MA2010       EVALUATE TRUE                                              26160005
NR0007          WHEN GENO-VENDOR-ACARE                                  26170005
MA2010               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-AN        26180005
NR0007          WHEN GENO-VENDOR-AOPTM                                  26190005
MA2010               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-ANOPT     26200005
AN1589          WHEN GENO-VENDOR-MAGLN                                  26210005
AN1589               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-MG        26220005
AN0127          WHEN GENO-VENDOR-BEACN                                  26230005
AN0127               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-BCN       26240005
AN1560          WHEN GENO-VENDOR-AOPTR                                  26250005
AN1560               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-AOR       26260005
AN1781          WHEN GENO-VENDOR-AARCH                                  26270005
AN1781               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-AAR       26280005
NR0007          WHEN GENO-VENDOR-CCARE                                  26290005
MD2689               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-CF        26300005
NR0007          WHEN GENO-VENDOR-CVSCM                                  26310005
MD7684             IF SW-T5W2K-MATCH-FOUND                              26320005
MD6619               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-CV        26330005
MD7684             END-IF                                               26340005
NR0007          WHEN GENO-VENDOR-MCARE                                  26350005
MA2010               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-CH        26360005
NR0007          WHEN GENO-VENDOR-MACMK                                  26370005
MA2010               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-CK        26380005
NR0007          WHEN GENO-VENDOR-NJCDH                                  26390005
MA2010               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-CD        26400005
MA2010          WHEN OTHER                                              26410005
MA2010               ADD CAR-OOP-AMT     TO A-BAT-TRL-OOP-AMT-NJ        26420005
MA2010       END-EVALUATE                                               26430005
MA2010                                                                  26440005
MD8008        IF SUBSCRIBER-PLAN = 780 OR 580 OR 690                    26450005
NJ0983           PERFORM 4000-MAP-FROM-INVENOMP                         26460005
NJ0983        END-IF                                                    26470005
MI2783        IF SUBSCRIBER-PLAN = 710                                  26480005
MI2783           PERFORM 4000-MAP-FROM-INVENOMP                         26490005
MI2783        END-IF                                                    26500005
AN4993        EVALUATE TRUE                                             26510005
NR0007            WHEN GENO-VENDOR-ACARE                                26520005
AN4993                 MOVE CAREMARK-HORIZON-RECORD TO                  26530005
NR0007                      OSM-OUTBOUND-REC                            26540005
AN4993                 MOVE GENO-PLAN-CODE          TO                  26550005
NR0007                      OSM-PREF-PLAN                               26560005
AN4993                 MOVE GENO-VENDOR-ID          TO                  26570005
NR0007                      OSM-PREF-VENDOR-CODE                        26580005
NJ9171                 MOVE GENO-TIMINGS(1)         TO                  26590005
NR0007                      OSM-PREF-CYCLE                              26600005
NJ9171                 MOVE 'C'                     TO                  26610005
NR0007                      OSM-PREF-FORMAT                             26620005
                                                                        26630005
ANI612                 ADD  +1                      TO                  26640005
ANI612                      WK-ACARE-DETL-CNT                           26650005
                                                                        26660005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               26670005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             26680005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  26690005
NR0007                                                                  26700005
NR0007                 CALL W-SUBPROGRAM USING                          26710005
NR0007                      OSM-DATA-AREA                               26720005
NR0007                                                                  26730005
NR0007                 IF GENO-POST-CALL                                26740005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  26750005
NR1362                 END-IF                                           26760005
NR0007                 IF GENO-BACKOUT-CALL                             26770005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  26780005
NR1362                 END-IF                                           26790005
NR0007            WHEN GENO-VENDOR-MCARE                                26800005
MI1323                 MOVE CAREMARK-HORIZON-RECORD TO                  26810005
NR0007                      OSM-OUTBOUND-REC                            26820005
MI1323                 MOVE GENO-PLAN-CODE          TO                  26830005
NR0007                      OSM-PREF-PLAN                               26840005
MI1323                 MOVE GENO-VENDOR-ID          TO                  26850005
NR0007                      OSM-PREF-VENDOR-CODE                        26860005
MI1323                 MOVE GENO-TIMINGS(1)         TO                  26870005
NR0007                      OSM-PREF-CYCLE                              26880005
MI1323                 MOVE 'C'                     TO                  26890005
NR0007                      OSM-PREF-FORMAT                             26900005
                                                                        26910005
ANI612                 ADD  +1                      TO                  26920005
ANI612                      WK-MCARE-DETL-CNT                           26930005
                                                                        26940005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               26950005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             26960005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  26970005
NR0007                                                                  26980005
NR0007                 CALL W-SUBPROGRAM USING                          26990005
NR0007                      OSM-DATA-AREA                               27000005
NR0007                                                                  27010005
NR0007                 IF GENO-POST-CALL                                27020005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  27030005
NR1362                 END-IF                                           27040005
NR0007                 IF GENO-BACKOUT-CALL                             27050005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  27060005
NR1362                 END-IF                                           27070005
                                                                        27080005
NR0007            WHEN GENO-VENDOR-NJCDH                                27090005
NJ9032                 IF L-MOOP-HNDO-MATCH                             27100005
NJ9032                     MOVE HOLD-RENEWAL-DATE TO                    27110005
NJ9032                       CAR-CLM-DTE-SERVICE-CCYYMMDD               27120005
NJ9032                 END-IF                                           27130005
NJ1348                                                                  27140005
NJ5584                 MOVE BENEFIT-FLAGS-32TO48 (LINE-INDEX) TO        27150005
NJ5584                       W-BENEFIT-FLAG                             27160005
NJ5584                 MOVE W-BENEFIT-FLAG-17 TO                        27170005
NJ5584                    THE-UNTWIDDLED-BITS                           27180005
NJ5584                                                                  27190005
NJ5584                 CALL C-BIT-TWIDDLER    USING                     27200005
NJ5584                    THE-UNTWIDDLED-BITS                           27210005
NJ5584                    THE-TWIDDLED-BITS                             27220005
NJ5584                                                                  27230005
NJ5584                 MOVE BENE-FLAG-401-560 (LINE-INDEX) TO           27240005
NJ5584                       W-BENEFIT-FLAG-401-560                     27250005
NJ5584                 MOVE W1-BENEFIT-FLAG-15 TO                       27260005
NJ5584                    THE-UNTWIDDLED-BITS-W1                        27270005
NJ5584                                                                  27280005
NJ5584                 CALL C-BIT-TWIDDLER USING                        27290005
NJ5584                    THE-UNTWIDDLED-BITS-W1                        27300005
NJ5584                    THE-TWIDDLED-BITS-W1                          27310005
NJ5584*********   FLAG 399 IF BIT-7 EQUALS '1'   *********              27320005
NJ5584*********   FLAG 400 IF BIT-8 EQUALS '1'   *********              27330005
NJ5584*********   FLAG 515 IF BIT-3-W1 EQUALS '1'*********              27340005
NJ7476*********   FLAG 516 IF BIT-4-W1 EQUALS '1'*********              27350005
NJ324D*********   FLAG 518 IF BIT-6-W1 EQUALS '1'*********              27360005
NJ5584                                                                  27370005
NJ3424                 EVALUATE TRUE                                    27380005
NJ3424                   WHEN CST-SHR-TIER-STAT(LINE-INDEX)(2:1) = '2'  27390005
NJ7476                     MOVE SPACES              TO                  27400005
NJ7476                       CAR-TIER2-BENEFIT-FLAG                     27410005
NJ5584                     IF BIT-8 = '1'                               27420005
NJ5584                       MOVE 'T2'              TO                  27430005
NJ5584                         CAR-TIER2-BENEFIT-FLAG                   27440005
NJ5584                     ELSE                                         27450005
NJ5584                       IF BIT-4-W1 = '1'                          27460005
NJ5584                         MOVE 'IT'              TO                27470005
NJ5584                           CAR-TIER2-BENEFIT-FLAG                 27480005
NJ5584                       END-IF                                     27490005
NJ5584                     END-IF                                       27500005
NJ3424                   WHEN CST-SHR-TIER-STAT(LINE-INDEX)(2:1) = '1'  27510005
NJ3424                     MOVE SPACES            TO                    27520005
NJ3424                       CAR-TIER2-BENEFIT-FLAG                     27530005
NJ5584                   WHEN CST-SHR-TIER-STAT(LINE-INDEX)(2:1) = '9'  27540005
NJ5584                     MOVE 'T9'              TO                    27550005
NJ5584                       CAR-TIER2-BENEFIT-FLAG                     27560005
NJ8469                   WHEN CST-SHR-TIER-STAT(LINE-INDEX)(2:1) = '3'  27570005
NJ8469                     MOVE 'T3'              TO                    27580005
NJ8469                       CAR-TIER2-BENEFIT-FLAG                     27590005
NJ324D                   WHEN CST-SHR-TIER-STAT(LINE-INDEX)(2:1) = '8'  27600005
NJ324D                     MOVE 'T8'              TO                    27610005
NJ324D                       CAR-TIER2-BENEFIT-FLAG                     27620005
NJ3424                   WHEN OTHER                                     27630005
NJ3424                                                                  27631005
NJ3424                     IF BIT-8 = '1' AND BIT-7 = '1'               27632005
NJ3424                       MOVE 'T2'              TO                  27633005
NJ3424                       CAR-TIER2-BENEFIT-FLAG                     27634005
NJ3424                     ELSE                                         27635005
NJ3424                       IF BIT-8 = '1'                             27636005
NJ3424                         MOVE 'T2'            TO                  27637005
NJ3424                         CAR-TIER2-BENEFIT-FLAG                   27638005
NJ3424                       ELSE                                       27639005
NJ3424                         IF BIT-7 = '1'                           27640005
NJ3424                           MOVE SPACES        TO                  27650005
NJ3424                           CAR-TIER2-BENEFIT-FLAG                 27660005
NJ3424                         ELSE                                     27670005
NJ5584                           IF BIT-3-W1 = '1'                      27680005
NJ5584                             MOVE 'T9'        TO                  27690005
NJ5584                             CAR-TIER2-BENEFIT-FLAG               27700005
NJ5584                           ELSE                                   27710005
NJ5584                             IF BIT-4-W1 = '1'                    27720005
NJ5584                               MOVE 'IT'        TO                27730005
NJ5584                               CAR-TIER2-BENEFIT-FLAG             27740005
NJ5584                             ELSE                                 27750005
NJ324D                               IF BIT-6-W1 = '1'                  27760005
NJ324D                                 MOVE 'T8'        TO              27770005
NJ324D                                 CAR-TIER2-BENEFIT-FLAG           27780005
NJ324D                               ELSE                               27790005
NJ5584                                 MOVE SPACES        TO            27800005
NJ5584                                 CAR-TIER2-BENEFIT-FLAG           27810005
NJ324D                               END-IF                             27820005
NJ5584                             END-IF                               27830005
NJ5584                           END-IF                                 27840005
NJ3424                         END-IF                                   27850005
NJ3424                       END-IF                                     27860005
NJ3424                     END-IF                                       27870005
NJ3424                 END-EVALUATE                                     27880005
NJ3424                                                                  27890005
TR354I             INITIALIZE W-GENO-POS                                27900006
TR354I             IF GENO-ERROR-MESSAGE(48:1) = 'I' OR 'O'             27910006
TR354I                MOVE GENO-ERROR-MESSAGE(48:1) TO W-GENO-POS       27920006
TR354I                MOVE SPACES            TO GENO-ERROR-MESSAGE(48:1)27930006
TR354I             END-IF                                               27940006
TR354I                                                                  27950006
NJ476C             IF (CLAIM-PROCESS-METH-2 = 'S' OR 'Y')               27960005
TR354I             , IF W-GENO-POS NOT = SPACES                         27970006
NJ476C             ,  INITIALIZE HRHSTPOS-PARMS                         27980005
NJ476C             ,  SET  L-TPOS-PROCESSING-CALL  TO TRUE              27990005
NJ476C             ,  MOVE W-ICN                   TO L-TPOS-CLAIM-NO   28000005
NJ476C             ,  MOVE GENO-VENDOR-ID          TO L-TPOS-VENDOR-ID  28010005
NJ476C             ,  MOVE W-SPGMPARM-TIMINGS(1)   TO L-TPOS-FREQUENCY  28011005
NJ476C             ,  MOVE HMO-POS-TIER-LVL(LINE-INDEX)                 28012005
NJ476C             ,                               TO L-TPOS-TIER-VALUE 28013005
NJ476C             ,  CALL W-HRHSTPOS USING HRHSTPOS-PARMS              28014005
TR354I             , END-IF                                             28015006
NJ476C             END-IF                                               28016005
NJ476C                                                                  28017005
NJ1164                 MOVE CAREMARK-HORIZON-RECORD TO                  28018005
NR0007                      OSM-OUTBOUND-REC                            28019005
NJ1164                 MOVE 'Y' TO                                      28019105
NR0007                      OSM-OUTBOUND-REC(420:1)                     28019205
NJ1164                 MOVE GENO-PLAN-CODE          TO                  28019305
NR0007                      OSM-PREF-PLAN                               28019405
NJ1164                 MOVE GENO-VENDOR-ID          TO                  28019505
NR0007                      OSM-PREF-VENDOR-CODE                        28019605
NJ1164                 MOVE GENO-TIMINGS(1)         TO                  28019705
NR0007                      OSM-PREF-CYCLE                              28019805
NJ1164                 MOVE 'C'                     TO                  28019905
NR0007                      OSM-PREF-FORMAT                             28020005
NJ9032                 ADD  +1                      TO                  28030005
NJ9032                      WK-NJCDH-DETL-CNT                           28040005
NJ7557             MOVE PACKAGE-CODE(LINE-INDEX) TO W-PKG-LOW-ORDER     28050005
NJ7557             MOVE W-PKG-BINARY      TO W-PACKAGE-CODE-X           28060005
NJ7557             MOVE W-PKG-CODE-X      TO WS-PACKAGE-CODE            28070005
NJ7557             MOVE GRP-BASE(LINE-INDEX) TO W-HOLD-SUFF-GRP-BASE    28080005
NJ7557             MOVE GRP-SEC(LINE-INDEX)  TO W-HOLD-SUFF-SECTION     28090005
NJ7557             MOVE WS-PACKAGE-CODE   TO W-HOLD-SUFF-PKG            28100005
NJ7557                 MOVE PND-SUB-NO              TO                  28110005
NJ7557                      W-HOLD-SUFF-SUBID                           28120005
NJ7557                 MOVE W-HOLD-SUFF-INFO        TO                  28130005
NJ7557                      OSM-OUTBOUND-REC(2872:29)                   28140005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               28150005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             28160005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  28170005
NR0007                                                                  28180005
NR0007                 CALL W-SUBPROGRAM USING                          28190005
NR0007                      OSM-DATA-AREA                               28200005
NR0007                                                                  28210005
NR0007                 IF GENO-POST-CALL                                28220005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  28230005
NR1362                 END-IF                                           28240005
NR0007                 IF GENO-BACKOUT-CALL                             28250005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  28260005
NR1362                 END-IF                                           28270005
NR0007            WHEN GENO-VENDOR-AOPTM                                28280005
AN1887                 MOVE CAREMARK-HORIZON-RECORD TO                  28290005
NR0007                      OSM-OUTBOUND-REC                            28300005
ANI612                 IF HMO-IN-OUT-NETWORK-INDIC(LINE-INDEX) = 'O'    28310005
NR0007                   MOVE 'O'   TO OSM-OUTBOUND-REC(388:1)          28320005
ANI612                 ELSE                                             28330005
NR0007                   MOVE 'I'   TO OSM-OUTBOUND-REC(388:1)          28340005
ANI612                 END-IF                                           28350005
                                                                        28360005
AN1887                 MOVE GENO-PLAN-CODE          TO                  28370005
NR0007                      OSM-PREF-PLAN                               28380005
AN1887                 MOVE GENO-VENDOR-ID          TO                  28390005
NR0007                      OSM-PREF-VENDOR-CODE                        28400005
AN1887                 MOVE GENO-TIMINGS(1)         TO                  28410005
NR0007                      OSM-PREF-CYCLE                              28420005
AN1887                 MOVE 'C'                     TO                  28430005
NR0007                      OSM-PREF-FORMAT                             28440005
                                                                        28450005
ANI612                 ADD  +1                      TO                  28460005
ANI612                      WK-AOPTM-DETL-CNT                           28470005
                                                                        28480005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               28490005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             28500005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  28510005
NR0007                                                                  28520005
NR0007                 CALL W-SUBPROGRAM USING                          28530005
NR0007                      OSM-DATA-AREA                               28540005
NR0007                                                                  28550005
NR0007                 IF GENO-POST-CALL                                28560005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  28570005
NR1362                 END-IF                                           28580005
NR0007                 IF GENO-BACKOUT-CALL                             28590005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  28600005
NR1362                 END-IF                                           28610005
AN1589                                                                  28620005
AN1589            WHEN GENO-VENDOR-MAGLN                                28630005
AN1589                 MOVE CAREMARK-HORIZON-RECORD TO                  28640005
AN1589                      OSM-OUTBOUND-REC                            28650005
AN1589                                                                  28660005
AN1589                 IF HMO-IN-OUT-NETWORK-INDIC(LINE-INDEX) = 'O'    28670005
AN1589                   MOVE 'O'   TO OSM-OUTBOUND-REC(388:1)          28680005
AN1589                 ELSE                                             28690005
AN1589                   MOVE 'I'   TO OSM-OUTBOUND-REC(388:1)          28700005
AN1589                 END-IF                                           28710005
AN1589                                                                  28720005
AN1589                 MOVE GENO-PLAN-CODE          TO                  28730005
AN1589                      OSM-PREF-PLAN                               28740005
AN1589                 MOVE GENO-VENDOR-ID          TO                  28750005
AN1589                      OSM-PREF-VENDOR-CODE                        28760005
AN1589                 MOVE GENO-TIMINGS(1)         TO                  28770005
AN1589                      OSM-PREF-CYCLE                              28780005
AN1589                 MOVE 'C'                     TO                  28790005
AN1589                      OSM-PREF-FORMAT                             28800005
AN1589                                                                  28810005
AN1589                 ADD  +1                      TO                  28820005
AN1589                      WK-MAGLN-DETL-CNT                           28830005
AN1589                                                                  28840005
AN1589                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               28850005
AN1589                 MOVE 'W'        TO OSM-CALL-FUNCTION             28860005
AN1589                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  28870005
AN1589                                                                  28880005
AN1589                 CALL W-SUBPROGRAM USING                          28890005
AN1589                      OSM-DATA-AREA                               28900005
AN1589                                                                  28910005
AN1589                 IF GENO-POST-CALL                                28920005
AN1589                    MOVE 1 TO GENO-TYPE7-WRITTEN                  28930005
AN1589                 END-IF                                           28940005
AN1589                 IF GENO-BACKOUT-CALL                             28950005
AN1589                    MOVE 1 TO GENO-FLUSH-WRITTEN                  28960005
AN1589                 END-IF                                           28970005
AN0127                                                                  28980005
AN0127            WHEN GENO-VENDOR-BEACN                                28990005
AN0127                 MOVE CAREMARK-HORIZON-RECORD TO                  29000005
AN0127                      OSM-OUTBOUND-REC                            29010005
AN0127                                                                  29020005
AN0127                 IF HMO-IN-OUT-NETWORK-INDIC(LINE-INDEX) = 'O'    29030005
AN0127                   MOVE 'O'   TO OSM-OUTBOUND-REC(388:1)          29040005
AN0127                 ELSE                                             29050005
AN0127                   MOVE 'I'   TO OSM-OUTBOUND-REC(388:1)          29060005
AN0127                 END-IF                                           29070005
AN0127                                                                  29080005
AN0127                 MOVE GENO-PLAN-CODE          TO                  29090005
AN0127                      OSM-PREF-PLAN                               29100005
AN0127                 MOVE GENO-VENDOR-ID          TO                  29110005
AN0127                      OSM-PREF-VENDOR-CODE                        29120005
AN0127                 MOVE GENO-TIMINGS(1)         TO                  29130005
AN0127                      OSM-PREF-CYCLE                              29140005
AN0127                 MOVE 'C'                     TO                  29150005
AN0127                      OSM-PREF-FORMAT                             29160005
AN0127                                                                  29170005
AN0127                 ADD  +1                      TO                  29180005
AN0127                      WK-BEACN-DETL-CNT                           29190005
AN0127                                                                  29200005
AN0127                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               29210005
AN0127                 MOVE 'W'        TO OSM-CALL-FUNCTION             29220005
AN0127                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  29230005
AN0127                                                                  29240005
AN0127                 CALL W-SUBPROGRAM USING                          29250005
AN0127                      OSM-DATA-AREA                               29260005
AN0127                                                                  29270005
AN0127                 IF GENO-POST-CALL                                29280005
AN0127                    MOVE 1 TO GENO-TYPE7-WRITTEN                  29290005
AN0127                 END-IF                                           29300005
AN0127                 IF GENO-BACKOUT-CALL                             29310005
AN0127                    MOVE 1 TO GENO-FLUSH-WRITTEN                  29320005
AN0127                 END-IF                                           29330005
AN0127                                                                  29340005
AN1560                                                                  29350005
AN1560            WHEN GENO-VENDOR-AOPTR                                29360005
AN1560                 MOVE CAREMARK-HORIZON-RECORD TO                  29370005
AN1560                      OSM-OUTBOUND-REC                            29380005
AN1560                                                                  29390005
AN1560                                                                  29400005
AN1560                 MOVE GENO-PLAN-CODE          TO                  29410005
AN1560                      OSM-PREF-PLAN                               29420005
AN1560                 MOVE GENO-VENDOR-ID          TO                  29430005
AN1560                      OSM-PREF-VENDOR-CODE                        29440005
AN1560                 MOVE GENO-TIMINGS(1)         TO                  29450005
AN1560                      OSM-PREF-CYCLE                              29460005
AN1560                 MOVE 'C'                     TO                  29470005
AN1560                      OSM-PREF-FORMAT                             29480005
AN1560                                                                  29490005
AN1560                 ADD  +1                      TO                  29500005
AN1560                      WK-AOPTR-DETL-CNT                           29510005
AN1560                                                                  29520005
AN1560                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               29530005
AN1560                 MOVE 'W'        TO OSM-CALL-FUNCTION             29540005
AN1560                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  29550005
AN1560                                                                  29560005
AN1560                 CALL W-SUBPROGRAM USING                          29570005
AN1560                      OSM-DATA-AREA                               29580005
AN1560                                                                  29590005
AN1560                 IF GENO-POST-CALL                                29600005
AN1560                    MOVE 1 TO GENO-TYPE7-WRITTEN                  29610005
AN1560                 END-IF                                           29610105
AN1560                 IF GENO-BACKOUT-CALL                             29610205
AN1560                    MOVE 1 TO GENO-FLUSH-WRITTEN                  29610305
AN1560                 END-IF                                           29610405
AN1560                                                                  29610505
AN1781                                                                  29610605
AN1781            WHEN GENO-VENDOR-AARCH                                29610705
AN1781                 MOVE CAREMARK-HORIZON-RECORD TO                  29610805
AN1781                      OSM-OUTBOUND-REC                            29610905
AN1781                                                                  29611005
AN1781                                                                  29611105
AN1781                 MOVE GENO-PLAN-CODE          TO                  29611205
AN1781                      OSM-PREF-PLAN                               29611305
AN1781                 MOVE GENO-VENDOR-ID          TO                  29611405
AN1781                      OSM-PREF-VENDOR-CODE                        29611505
AN1781                 MOVE GENO-TIMINGS(1)         TO                  29611605
AN1781                      OSM-PREF-CYCLE                              29611705
AN1781                 MOVE 'C'                     TO                  29611805
AN1781                      OSM-PREF-FORMAT                             29611905
AN1781                                                                  29612005
AN1781                 ADD  +1                      TO                  29612105
AN1781                      WK-AARCH-DETL-CNT                           29612205
AN1781                                                                  29612305
AN1781                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               29612405
AN1781                 MOVE 'W'        TO OSM-CALL-FUNCTION             29612505
AN1781                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  29612605
AN1781                                                                  29612705
AN1781                 CALL W-SUBPROGRAM USING                          29612805
AN1781                      OSM-DATA-AREA                               29612905
AN1781                                                                  29613005
AN1781                 IF GENO-POST-CALL                                29613105
AN1781                    MOVE 1 TO GENO-TYPE7-WRITTEN                  29613205
AN1781                 END-IF                                           29613305
AN1781                 IF GENO-BACKOUT-CALL                             29613405
AN1781                    MOVE 1 TO GENO-FLUSH-WRITTEN                  29613505
AN1781                 END-IF                                           29613605
AN1781                                                                  29613705
NR0007            WHEN GENO-VENDOR-MACMK                                29613805
MA2010                 MOVE CAREMARK-HORIZON-RECORD TO                  29613905
NR0007                      OSM-OUTBOUND-REC                            29614005
MA0815                 MOVE HMO-IN-OUT-NETWORK-INDIC(LINE-INDEX) TO     29615005
MA0815                             OSM-HMO-IN-OUT-NETWORK-INDIC         29616005
MA0815                 MOVE PND-SUB-NO           TO OSM-PND-SUB-NO      29617005
MA0815                 MOVE DOCUMENT-CONTROL-NO  TO                     29618005
MA0815                             OSM-DOCUMENT-CONTROL-NO              29619005
MA0815                 MOVE CLAIM-PROCESS-TYPE   TO                     29620005
MA0815                             OSM-CLAIM-PROCESS-TYPE               29630005
MA0815                 MOVE CLAIM-PROCESS-STATUS TO                     29640005
MA0815                             OSM-CLAIM-PROCESS-STATUS             29650005
MA0815                 MOVE CLAIM-PROCESS-EFF-2  TO                     29660005
MA0815                             OSM-CLAIM-PROCESS-EFF-2              29670005
MA0815                 MOVE CLM-SUB-NAME         TO OSM-CLM-SUB-NAME    29680005
MA0815                 MOVE GRP-BASE(LINE-INDEX) TO OSM-GRP-BASE        29690005
MA0815                 MOVE GRP-SEC(LINE-INDEX)  TO OSM-GRP-SEC         29700005
MA0815                 MOVE PACKAGE-CODE(LINE-INDEX) TO W-PKG-LOW-ORDER 29710005
MA0815                 MOVE W-PKG-BINARY         TO W-PACKAGE-CODE-X    29720005
MA0815                 MOVE W-PKG-CODE-X         TO L-GP-PKG            29730005
MA0815                 MOVE L-GP-PKG             TO OSM-L-GP-PKG        29740005
MA2010                 MOVE GENO-PLAN-CODE          TO                  29750005
NR0007                      OSM-PREF-PLAN                               29760005
MA2010                 MOVE GENO-VENDOR-ID          TO                  29770005
NR0007                      OSM-PREF-VENDOR-CODE                        29780005
MA2010                 MOVE GENO-TIMINGS(1)         TO                  29790005
NR0007                      OSM-PREF-CYCLE                              29800005
MA1079                 MOVE GRP-BASE(LINE-INDEX) TO                     29810005
MA1079                      W-HLD-SUFF-GRP-BASE-M                       29820005
MA1079                 MOVE GRP-SEC(LINE-INDEX)  TO                     29830005
MA1079                      W-HLD-SUFF-SECTION-M                        29840005
MA1079                 MOVE W-PKG-CODE-X        TO                      29850005
MA1079                      W-HLD-SUFF-PKG-M                            29860005
MA1079                 MOVE PND-SUB-NO          TO                      29870005
MA1079                      W-HLD-SUFF-SUBID-M                          29880005
MA1079                 MOVE MSLN-EMPLOYEE-DEPT(LINE-INDEX)              29890005
MA1079                  TO  W-HLD-SUFF-EMP-DPT-M                        29900005
                                                                        29910005
MA1368                 IF BNCH-ID    = 'R'  OR 'Z'                      29920005
MA1368                    MOVE 'Y'                  TO                  29930005
MA1368                      OSM-OUTBOUND-REC(2861:1)                    29940005
MA1368                 END-IF                                           29950005
                                                                        29960005
MA1079                 MOVE W-HOLD-SUFF-INFO-MACMK TO                   29970005
MA1079                      OSM-OUTBOUND-REC(2863:38)                   29980005
                                                                        29990005
MA2010                 MOVE 'C'                     TO                  30000005
NR0007                      OSM-PREF-FORMAT                             30010005
                                                                        30020005
ANI612                 ADD  +1                      TO                  30030005
ANI612                      WK-MACMK-DETL-CNT                           30040005
                                                                        30050005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               30060005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             30070005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  30080005
NR0007                                                                  30090005
NR0007                 CALL W-SUBPROGRAM USING                          30100005
NR0007                      OSM-DATA-AREA                               30110005
NR0007                                                                  30120005
NR0007                 IF GENO-POST-CALL                                30130005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  30140005
NR1362                 END-IF                                           30150005
NR0007                 IF GENO-BACKOUT-CALL                             30160005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  30170005
NR1362                 END-IF                                           30180005
NR0007            WHEN GENO-VENDOR-CCARE                                30190005
MD2689                 MOVE CAREMARK-HORIZON-RECORD TO                  30200005
NR0007                      OSM-OUTBOUND-REC                            30210005
MD2689                 MOVE GENO-PLAN-CODE          TO                  30220005
NR0007                      OSM-PREF-PLAN                               30230005
MD2689                 MOVE GENO-VENDOR-ID          TO                  30240005
NR0007                      OSM-PREF-VENDOR-CODE                        30250005
MD2689                 MOVE GENO-TIMINGS(1)         TO                  30260005
NR0007                      OSM-PREF-CYCLE                              30270005
MD2689                 MOVE 'C'                     TO                  30280005
NR0007                      OSM-PREF-FORMAT                             30290005
                                                                        30300005
ANI612                 ADD  +1                      TO                  30310005
ANI612                      WK-CCARE-DETL-CNT                           30320005
                                                                        30330005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               30340005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             30350005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  30360005
NR0007                                                                  30370005
NR0007                 CALL W-SUBPROGRAM USING                          30380005
NR0007                      OSM-DATA-AREA                               30390005
NR0007                                                                  30400005
NR0007                 IF GENO-POST-CALL                                30410005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  30420005
NR1362                 END-IF                                           30430005
NR0007                 IF GENO-BACKOUT-CALL                             30440005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  30450005
NR1362                 END-IF                                           30460005
NR0007            WHEN GENO-VENDOR-CVSCM                                30470005
MD7684               INITIALIZE OUT-CVSCM-VEND-RECORDS                  30480005
MD7684               IF SW-T5W2K-MATCH-FOUND                            30490005
MD6619                 MOVE CAREMARK-HORIZON-RECORD TO                  30500005
NR0007                      OSM-OUTBOUND-REC                            30510005
MD6619                 MOVE GENO-PLAN-CODE          TO                  30520005
NR0007                      OSM-PREF-PLAN                               30530005
MD6619                 MOVE GENO-VENDOR-ID          TO                  30540005
NR0007                      OSM-PREF-VENDOR-CODE                        30550005
MD6619                 MOVE GENO-TIMINGS(1)         TO                  30560005
NR0007                      OSM-PREF-CYCLE                              30570005
MD6619                 MOVE 'C'                     TO                  30580005
NR0007                      OSM-PREF-FORMAT                             30590005
MD6619                                                                  30600005
MD6619                 ADD  +1                      TO                  30610005
MD6619                      WK-CVSCM-DETL-CNT                           30620005
MD6619                                                                  30630005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               30640005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             30650005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  30660005
NR0007                                                                  30670005
NR0007                 CALL W-SUBPROGRAM USING                          30680005
NR0007                      OSM-DATA-AREA                               30690005
NR0007                                                                  30700005
NR0007                 IF GENO-POST-CALL                                30710005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  30720005
NR1362                 END-IF                                           30730005
NR0007                 IF GENO-BACKOUT-CALL                             30740005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  30750005
NR1362                 END-IF                                           30760005
MD7684               ELSE                                               30770005
MD7684                 WRITE OUT-CVSCM-VEND-RECORDS FROM                30780005
MD7684                               CAREMARK-HORIZON-RECORD            30790005
MD7684               END-IF                                             30800005
AN4993            WHEN OTHER                                            30810005
TR354I                                                                  30820006
TR354I               INITIALIZE W-GENO-POS                              30830006
TR354I               IF GENO-ERROR-MESSAGE(48:1) = 'I' OR 'O'           30840006
TR354I                  MOVE GENO-ERROR-MESSAGE(48:1) TO W-GENO-POS     30850006
TR354I                  MOVE SPACES          TO GENO-ERROR-MESSAGE(48:1)30860006
TR354I               END-IF                                             30870006
TR354I                                                                  30880006
NJ476C               IF ((SUBSCRIBER-PLAN = 780) AND                    30880105
NJ476C               ,        (CLAIM-PROCESS-METH-2 = 'S' OR 'Y'))      30880205
TR354I               , IF W-GENO-POS NOT = SPACES                       30880306
NJ476C               ,  INITIALIZE HRHSTPOS-PARMS                       30880405
NJ476C               ,  SET L-TPOS-PROCESSING-CALL  TO TRUE             30880505
NJ476C               ,  MOVE W-ICN                  TO L-TPOS-CLAIM-NO  30880605
NJ476C               ,  MOVE GENO-VENDOR-ID         TO L-TPOS-VENDOR-ID 30880705
NJ476C               ,  MOVE W-SPGMPARM-TIMINGS(1)  TO L-TPOS-FREQUENCY 30880805
NJ476C               ,  MOVE HMO-POS-TIER-LVL(LINE-INDEX)               30880905
NJ476C               ,                              TO L-TPOS-TIER-VALUE30881005
NJ476C               ,  CALL W-HRHSTPOS USING HRHSTPOS-PARMS            30882005
TR354I               , END-IF                                           30883006
NJ476C               END-IF                                             30884005
NJ476C                                                                  30885005
AN4993                 MOVE CAREMARK-HORIZON-RECORD TO                  30886005
NR0007                      OSM-OUTBOUND-REC                            30887005
AN4993                 MOVE GENO-PLAN-CODE          TO                  30888005
NR0007                      OSM-PREF-PLAN                               30889005
AN4993                 MOVE GENO-VENDOR-ID          TO                  30890005
NR0007                      OSM-PREF-VENDOR-CODE                        30900005
NJ9171                 MOVE GENO-TIMINGS(1)         TO                  30910005
NR0007                      OSM-PREF-CYCLE                              30920005
NJ9171                 MOVE 'C'                     TO                  30930005
NR0007                      OSM-PREF-FORMAT                             30940005
                                                                        30950005
ANI612                 ADD  +1                      TO                  30960005
ANI612                      WK-WYETH-DETL-CNT                           30970005
                                                                        30980005
NR0007                 MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               30990005
NR0007                 MOVE 'W'        TO OSM-CALL-FUNCTION             31000005
NR0007                 MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  31010005
NR0007                                                                  31020005
NR0007                 CALL W-SUBPROGRAM USING                          31030005
NR0007                      OSM-DATA-AREA                               31040005
NR0007                                                                  31050005
NR0007                 IF GENO-POST-CALL                                31060005
NR0007                    MOVE 1 TO GENO-TYPE7-WRITTEN                  31070005
NR1362                 END-IF                                           31080005
NR0007                 IF GENO-BACKOUT-CALL                             31090005
NR0007                    MOVE 1 TO GENO-FLUSH-WRITTEN                  31100005
NR1362                 END-IF                                           31110005
AN4993        END-EVALUATE                                              31120005
MA1368     END-IF                                                       31130005
AN0609                                                                  31140005
AN0609                                                                  31150005
AN0609                                                                  31160005
                                                                        31170005
AN0609     INITIALIZE HRHS4QTR-PARMS                                    31180005
AN0609                                                                  31190005
AN0609     IF GENO-VENDOR-ID = 'ACARE' OR 'AOPTM' OR 'MAGLN' OR         31200005
AN1560     ,                   'AOPTR' OR                               31210005
AN1781     ,                   'AARCH' OR                               31220005
AN0609     ,                   'BEACN'                                  31230005
AN0609     ,     AND  GENO-PLAN-CODE  =  834                            31240005
AN0609     ,                                                            31241005
AN0609     ,  MOVE PACKAGE-CODE(LINE-INDEX) TO W-PKG-LOW-ORDER          31242005
AN0609     ,                                                            31243005
AN0609     ,  IF SUBSCRIBER-PLAN    =                                   31244005
AN0609     ,  ,  L-4QTR-HNDI-SUB-PLAN   AND                             31245005
AN0609     ,  ,                                                         31246005
AN0609     ,  ,  GRP-BASE(LINE-INDEX)           =                       31247005
AN0609     ,  ,  L-4QTR-HNDI-GROUP-BASE AND                             31248005
AN0609     ,  ,                                                         31249005
AN0609     ,  ,  GRP-SEC(LINE-INDEX)            =                       31250005
AN0609     ,  ,  L-4QTR-HNDI-GROUP-SECT AND                             31260005
AN0609     ,  ,                                                         31270005
AN0609     ,  ,  W-PKG-BINARY                   =                       31280005
AN0609     ,  ,  L-4QTR-HNDI-PACK-CODE  AND                             31290005
AN0609     ,  ,                                                         31300005
AN0609     ,  ,  GENO-VENDOR-ID                 =                       31310005
AN0609     ,  ,  L-4QTR-HNDI-VENDOR-ID  AND                             31320005
AN0609     ,  ,                                                         31330005
AN0609     ,  ,  CAR-CLM-DTE-SERVICE-CCYYMMDD   =                       31340005
AN0609     ,  ,  L-4QTR-HNDI-DOS                                        31350005
AN0609     ,  ,                                                         31360005
AN0609     ,  ,  CONTINUE                                               31370005
AN0609     ,  ELSE                                                      31380005
AN0609     ,                                                            31390005
AN0609     ,  ,  PERFORM 4001-CALL-HRHS4QTR                             31400005
AN0609     ,  ,                                                         31410005
AN0609     ,  ,  IF L-4QTR-HNDO-MATCH                                   31420005
AN0609     ,  ,  , AND L-4QTR-HNDO-4THQTR-IND = 'Y'                     31430005
AN0609     ,  ,  ,    AND SW-COMINGLE-NT-L-O-N                          31440005
AN0609     ,  ,  ,                                                      31450005
AN0609     ,  ,  ,  IF   DED-AMT-DOLLARS(LINE-INDEX)  >  +0             31460005
AN0609     ,  ,  ,  ,                                                   31470005
AN0609     ,  ,  ,  ,  MOVE L-4QTR-HNDO-RENEWAL-DATE    TO              31480005
AN0609     ,  ,  ,  ,       CAR-CLM-DTE-SERVICE-CCYYMMDD                31490005
AN0609     ,  ,  ,  ,                                                   31500005
AN0609     ,  ,  ,  ,  MOVE 'Y' TO   CAR-4QTR-DQ                        31510005
AN0609     ,  ,  ,  ,  IF DED-AMT-DOLLARS(LINE-INDEX) >  +0             31520005
AN0609     ,  ,  ,  ,     MOVE DED-AMT-DOLLARS(LINE-INDEX) TO           31530005
AN0609     ,  ,  ,  ,          CAR-OOP-AMT                              31540005
AN0609     ,  ,  ,  ,     MOVE +0                          TO           31550005
AN0609     ,  ,  ,  ,          CAR-DED-AMT                              31560005
AN0609     ,  ,  ,  ,  END-IF                                           31570005
AN0609     ,  ,  ,  ,                                                   31580005
AN0609     ,  ,  ,  ,  IF (CPE-NEG-ADJ AND GENO-POST-CALL)  OR          31590005
AN0609     ,  ,  ,  ,     (GENO-BACKOUT-CALL AND NOT CPE-NEG-ADJ)       31600005
AN0609     ,  ,  ,  ,                                                   31610005
AN0609     ,  ,  ,  ,     COMPUTE CAR-OOP-AMT   =                       31620005
AN0609     ,  ,  ,  ,             CAR-OOP-AMT   * -1                    31630005
AN0609     ,  ,  ,  ,     END-COMPUTE                                   31640005
AN0609     ,  ,  ,  ,  END-IF                                           31650005
AN0609     ,  ,  ,  ,                                                   31660005
AN1105     ,  ,  ,  ,  MOVE '4Q'                        TO              31670005
AN1105     ,  ,  ,  ,       CAR-REC-ID(15:2)                            31680005
AN1105     ,  ,  ,  ,       CAR-CLM-ID(15:2)                            31690005
AN0609     ,  ,  ,  ,  MOVE CAREMARK-HORIZON-RECORD     TO              31700005
AN0609     ,  ,  ,  ,       OSM-OUTBOUND-REC                            31710005
AN0609     ,  ,  ,  ,                                                   31720005
AN0609     ,  ,  ,  ,  MOVE GENO-PLAN-CODE              TO              31730005
AN0609     ,  ,  ,  ,       OSM-PREF-PLAN                               31740005
AN0609     ,  ,  ,  ,                                                   31750005
AN0609     ,  ,  ,  ,  MOVE GENO-VENDOR-ID              TO              31760005
AN0609     ,  ,  ,  ,       OSM-PREF-VENDOR-CODE                        31770005
AN0609     ,  ,  ,  ,                                                   31780005
AN0609     ,  ,  ,  ,  MOVE GENO-TIMINGS(1)             TO              31790005
AN0609     ,  ,  ,  ,       OSM-PREF-CYCLE                              31800005
AN0609     ,  ,  ,  ,                                                   31810005
AN0609     ,  ,  ,  ,  MOVE 'C'                         TO              31820005
AN0609     ,  ,  ,  ,       OSM-PREF-FORMAT                             31830005
AN0609     ,  ,  ,  ,                                                   31840005
AN0609     ,  ,  ,  ,  EVALUATE TRUE                                    31850005
AN0609     ,  ,  ,  ,     WHEN GENO-VENDOR-ID = 'ACARE'                 31860005
AN0609     ,  ,  ,  ,        ADD  +1                          TO        31870005
AN0609     ,  ,  ,  ,             WK-ACARE-DETL-CNT                     31880005
AN0609     ,  ,  ,  ,                                                   31890005
AN0609     ,  ,  ,  ,     WHEN GENO-VENDOR-ID = 'AOPTM'                 31900005
AN0609     ,  ,  ,  ,        ADD  +1                          TO        31910005
AN0609     ,  ,  ,  ,             WK-AOPTM-DETL-CNT                     31920005
AN0609     ,  ,  ,  ,                                                   31930005
AN0609     ,  ,  ,  ,     WHEN GENO-VENDOR-ID = 'MAGLN'                 31940005
AN0609     ,  ,  ,  ,        ADD  +1                          TO        31950005
AN0609     ,  ,  ,  ,             WK-MAGLN-DETL-CNT                     31960005
AN0609     ,  ,  ,  ,                                                   31970005
AN0127     ,  ,  ,  ,     WHEN GENO-VENDOR-ID = 'BEACN'                 31980005
AN0127     ,  ,  ,  ,        ADD  +1                          TO        31990005
AN0127     ,  ,  ,  ,             WK-BEACN-DETL-CNT                     32000005
AN1560     ,  ,  ,  ,                                                   32010005
AN1560     ,  ,  ,  ,     WHEN GENO-VENDOR-ID = 'AOPTR'                 32020005
AN1560     ,  ,  ,  ,        ADD  +1                          TO        32030005
AN1560     ,  ,  ,  ,             WK-AOPTR-DETL-CNT                     32040005
AN1781     ,  ,  ,  ,                                                   32050005
AN1781     ,  ,  ,  ,     WHEN GENO-VENDOR-ID = 'AARCH'                 32060005
AN1781     ,  ,  ,  ,        ADD  +1                          TO        32070005
AN1781     ,  ,  ,  ,             WK-AARCH-DETL-CNT                     32080005
AN0609     ,  ,  ,  ,  END-EVALUATE                                     32090005
AN0609     ,  ,  ,  ,                                                   32100005
AN0609     ,  ,  ,  ,  MOVE 'HRHSAO3C' TO OSM-CALL-MODULE               32110005
AN0609     ,  ,  ,  ,  MOVE 'W'        TO OSM-CALL-FUNCTION             32120005
AN0609     ,  ,  ,  ,  MOVE 'HRHSAO3X' TO W-SUBPROGRAM                  32130005
AN0609     ,  ,  ,  ,                                                   32140005
AN0609     ,  ,  ,  ,  CALL W-SUBPROGRAM USING                          32150005
AN0609     ,  ,  ,  ,       OSM-DATA-AREA                               32160005
AN0609     ,  ,  ,  ,                                                   32170005
AN0609     ,  ,  ,  ,  IF GENO-POST-CALL                                32180005
AN0609     ,  ,  ,  ,     MOVE 1 TO GENO-TYPE7-WRITTEN                  32190005
AN0609     ,  ,  ,  ,  END-IF                                           32200005
AN0609     ,  ,  ,  ,                                                   32210005
AN0609     ,  ,  ,  ,  IF GENO-BACKOUT-CALL                             32220005
AN0609     ,  ,  ,  ,     MOVE 1 TO GENO-FLUSH-WRITTEN                  32230005
AN0609     ,  ,  ,  ,  END-IF                                           32240005
AN0609     ,  ,  ,  END-IF                                              32250005
AN0609     ,  ,  ELSE                                                   32260005
AN0609     ,  ,  ,  SET  L-4QTR-HNDO-NO-MATCH        TO TRUE            32270005
AN0609     ,  ,  END-IF                                                 32280005
AN0609     ,  END-IF                                                    32290005
AN0609     END-IF                                                       32300005
AN0609                                                                  32310005
AN0609     INITIALIZE CAR-4QTR-DQ                                       32320005
AN0609                                                                  32330005
PERIOD     .                                                            32340005
PERIOD**--/ \                                                           32350005
                                                                        32360005
                                                                        32370005
MD7568******************************************************************32380005
MD7568*                   GET-SSN-FROM-ELIGIBILITY                      32390005
MD7568******************************************************************32400005
MD7568 GET-SSN-FROM-ELIGIBILITY.                                        32410005
MD7568                                                                  32420005
MD7568     MOVE PND-SUB-NO               TO WS-MEM-SUB-NO               32430005
MD7568     MOVE SUBSCRIBER-PLAN          TO WS-MEM-CONTROL-PLAN-CODE    32440005
MD7568     MOVE SPACES                   TO WS-MEM-FIRST-NAME           32450005
MD7568     MOVE ZEROES                   TO WS-MEM-BIRTH-DATE           32460005
MD7568     MOVE 01                       TO WS-MEM-NUMBER               32470005
MD7568                                                                  32480005
MD7568     PERFORM CALL-MEMBER                                          32490005
MD7568                                                                  32500005
MD7568     IF NO-MEMBERSHIP-ERROR                                       32510005
MD7568       IF NO-ELIG-ERRORS                                          32520005
MD7568         IF SSN-9-MBR-S = LOW-VALUES OR SPACES OR ZEROES          32530005
MD7568            MOVE ZEROES            TO W-PEND-SSN                  32540005
MD7568         ELSE                                                     32550005
MD7568            MOVE SSN-9-MBR-S       TO W-PEND-SSN                  32560005
MD7568         END-IF                                                   32570005
MD7568       END-IF                                                     32580005
MD7568     END-IF                                                       32590005
MD7568                                                                  32600005
PERIOD     .                                                            32610005
PERIOD**--/ \                                                           32620005
MD7568                                                                  32630005
MD7568***************************************************************** 32640005
MD7568*         GET THE PATIENT NAME AND DOB FROM MEMBER SEGMENT        32650005
MD7568***************************************************************** 32660005
MD7568 CALL-MEMBER.                                                     32670005
MD7568                                                                  32680005
MD7568     PERFORM READ-GROUP-KEY-SEGMENT                               32690005
MD7568                                                                  32700005
MD7568     IF MEMBERSHIP-ERROR                                          32710005
MD7568        CONTINUE                                                  32720005
MD7568     ELSE                                                         32730005
MD7568       PERFORM READ-MEMBER-SEGMENT                                32740005
MD7568     END-IF                                                       32750005
MD7568                                                                  32760005
PERIOD     .                                                            32770005
PERIOD****/ \                                                           32780005
MD7568                                                                  32790005
MD7568***************************************************************** 32800005
MD7568*                 READ GROUP KEY SEGMENT                        * 32810005
MD7568***************************************************************** 32820005
MD7568 READ-GROUP-KEY-SEGMENT.                                          32830005
MD7568                                                                  32840005
MD7568     MOVE +0                     TO NUMBER-OF-COV-SEGMENTS        32850005
MD7568     MOVE +0                     TO NUMBER-OF-MBR-SEGS            32860005
MD7568     MOVE WS-MEM-SUB-NO          TO MEM-CERT-NO                   32870005
MD7568     MOVE WS-MEM-CONTROL-PLAN-CODE                                32880005
MD7568                                 TO MEM-SUB-PLAN                  32890005
MD7568     SET  MEM-SEG-IS-KEY-BASE    TO TRUE                          32900005
MD7568     MOVE HIGH-VALUES            TO MEM-EFF-DATE-TXT              32910005
MD7568     MOVE ZERO                   TO MEM-SEGMENT-SEQ               32920005
MD7568     SET  MEM-REQ-SEG-IS-BLUE    TO TRUE                          32930005
MD7568                                                                  32940005
MD7568     CALL MEMBERSHIP-ACCESSOR USING MEMBERSHIP-ACCESSOR-WORKAREA  32950005
MD7568     IF RETURN-CODE NOT EQUAL +0                                  32960005
MD7568        SET MEMBERSHIP-ERROR TO TRUE                              32970005
MD7568     ELSE                                                         32980005
MD7568        MOVE MEM-REQUESTED-SEGMENT TO KEY-BASE-SEGMENT-S          32990005
MD7568        MOVE SEG-CT-COV-KEY-S      TO NUMBER-OF-COV-SEGMENTS      33000005
MD7568        MOVE SEG-CT-MBR-KEY-S      TO NUMBER-OF-MBR-SEGS          33010005
MD7568        SET NO-MEMBERSHIP-ERROR TO TRUE                           33020005
MD7568     END-IF                                                       33030005
MD7568     .                                                            33040005
PERIOD****/ \                                                           33050005
MD7568                                                                  33060005
MD7568***************************************************************** 33070005
MD7568*                 READ MEMBER SEGMENT                           * 33080005
MD7568***************************************************************** 33090005
MD7568 READ-MEMBER-SEGMENT.                                             33100005
MD7568                                                                  33110005
MD7568     SET NO-ELIG-ERRORS          TO TRUE                          33120005
MD7568     MOVE WS-MEM-SUB-NO          TO MEM-CERT-NO                   33130005
MD7568     MOVE WS-MEM-CONTROL-PLAN-CODE                                33140005
MD7568                                 TO MEM-SUB-PLAN                  33150005
MD7568     MOVE MEMBER-SEGMENT-SEQ     TO MEM-SEGMENT-SEQ               33160005
MD7568     SET  MEM-SEG-IS-MEMBER      TO TRUE                          33170005
MD7568     MOVE HIGH-VALUES            TO MEM-EFF-DATE-TXT              33180005
MD7568     MOVE WS-MEM-FIRST-NAME      TO MEM-FIRST-NAME                33190005
MD7568     MOVE WS-MEM-BIRTH-DATE      TO MEM-BIRTH-DATE                33200005
MD7568                                                                  33210005
MD7568     MOVE WS-MEM-NUMBER          TO MEM-MEMBER-NUMBER             33220005
MD7568     SET  MEM-REQ-SEG-IS-BLUE    TO TRUE                          33230005
MD7568                                                                  33240005
MD7568     CALL MEMBERSHIP-ACCESSOR USING MEMBERSHIP-ACCESSOR-WORKAREA  33250005
MD7568                                                                  33260005
MD7568     IF RETURN-CODE EQUAL +0                                      33270005
MD7568        MOVE MEM-REQUESTED-SEGMENT TO MEMBER-SEGMENT-S            33280005
MD7568        SET NO-ELIG-ERRORS          TO TRUE                       33290005
MD7568     ELSE                                                         33300005
MD7568        SET ELIG-ERRORS            TO TRUE                        33310005
MD7568     END-IF                                                       33320005
MD7568     .                                                            33330005
PERIOD****/ \                                                           33340005
MD7568                                                                  33350005
MD6619******************************************************************33360005
MD6619*                   POPULATE-MBR-ID-NO-MBR-S                      33370005
MD6619******************************************************************33380005
MD6619 POPULATE-MBR-ID-NO-MBR-S.                                        33390005
MD6619                                                                  33400005
MD6619     MOVE 'P'                   TO L-REQUEST-TYPE                 33410005
MD6619     MOVE 'CVS'                 TO L-REQUEST-FUNC                 33420005
MD6619*                                                                 33430005
MD6619     MOVE SUBSCRIBER-PLAN       TO WS-SUBSCRIBER-PLAN             33440005
MD6619     MOVE WS-SUBSCRIBER-PLAN-X  TO CVS-CONTROL-PLAN-CODE          33450005
MD6619     MOVE PND-SUB-NO            TO CVS-SUBSCRIBER-ID              33460005
MD6619     MOVE 'U'                   TO CVS-FILE-TYPE                  33470005
MD6619     MOVE 'CVSCM'               TO CVS-VENDOR-NAME                33480005
MD6619     UNSTRING  PAT-NAME DELIMITED BY '*'                          33490005
MD6619               INTO WS-PAT-FIRST-NAME                             33500005
MD6619                    WS-PAT-MIDDLE-NAME                            33510005
MD6619                    WS-PAT-LAST-NAME                              33520005
MD6619     END-UNSTRING                                                 33530005
MD6619     MOVE WS-PAT-FIRST-NAME     TO CVS-PAT-FST-FULL-NAME          33540005
MD6619     MOVE PAT-DOB               TO CVS-PAT-DOB                    33550005
MD6619     MOVE FST-SVC-DATE(LINE-INDEX)                                33560005
MD6619                                TO CVS-DOS                        33570005
MD6619                                                                  33580005
MD6619     MOVE RECORD-TYPE-CA        TO L-INPUT-PARMS-CVS              33590005
MD6619*                                                                 33600005
MD6619     CALL C-ENROLL-PROGRAM USING HRHSID01-PARM                    33610005
MD6619                                 HRHSID01-VALUES                  33620005
MD6619                                                                  33630005
MD6619                                                                  33640005
MD6619     IF L-RETURN-CODE-CVS = '000'                                 33650005
MD6619      MOVE L-RETURN-AREA-CVS(1:13) TO CAR-PATIENT-ID(1:13)        33660005
MD6619      MOVE L-RETURN-AREA-CVS(15:2) TO CAR-PATIENT-ID(14:2)        33670005
MD6619      MOVE SPACES                  TO CAR-PATIENT-ID(16:5)        33680005
MD6619     ELSE                                                         33690005
MD6619      MOVE L-RETURN-AREA-CVS(1:13) TO CAR-PATIENT-ID(1:13)        33700005
MD6619      MOVE ZEROES                  TO CAR-PATIENT-ID(14:2)        33710005
MD6619      MOVE SPACES                  TO CAR-PATIENT-ID(16:5)        33720005
MD6619     END-IF                                                       33730005
PERIOD     .                                                            33740005
PERIOD**--/ \                                                           33750005
                                                                        33760005
       2750-PROCESS-CLAIM-LINE.                                         33770005
                                                                        33780005
AN0609     SET SW-COMINGLE-NT-L-O-N                TO TRUE              33790005
                                                                        33800005
           COMPUTE CAR-PATIENT-PAY-EXCL-DED-AMT =                       33810005
               CAR-PATIENT-PAY-EXCL-DED-AMT +                           33820005
               (COPAYMENT-AMT(LINE-INDEX) +                             33830005
               COINSURANCE-AMT(LINE-INDEX))                             33840005
                                                                        33850005
           IF LIN-STATUS-IND(LINE-INDEX) = '4'                          33860005
                                                                        33870005
NR0007         PERFORM VARYING GENO-CO-MIN-INDEX FROM +1 BY +1          33880005
NR0007                     UNTIL   GENO-CO-MIN-INDEX > +6               33890005
                 EVALUATE TRUE                                          33900005
NR0007              WHEN GENO-DEDUCTIBLE(GENO-CO-MIN-INDEX)             33910005
                                                                        33920005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          33930005
                                                                        33940005
                       COMPUTE W-DED-AMT-DOLLAR = W-DED-AMT-DOLLAR      33950005
                                      + DED-AMT-DOLLARS(LINE-INDEX)     33960005
                                                                        33970005
NR0007              WHEN GENO-DED-EQ-OOP(GENO-CO-MIN-INDEX)             33980005
ANI547                                                                  33990005
                                                                        34000005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34010005
                                                                        34020005
ANI547                 COMPUTE W-DED-AMT-DOLLAR = W-DED-AMT-DOLLAR      34030005
ANI547                                + DED-AMT-DOLLARS(LINE-INDEX)     34040005
ANI547                                                                  34050005
ANI547                 MOVE    W-DED-AMT-DOLLAR          TO             34060005
ANI547                         W-OUT-OF-POCKET                          34070005
                                                                        34080005
                                                                        34090005
NR0007              WHEN GENO-OUTOFPOCKET(GENO-CO-MIN-INDEX)            34100005
                       COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34110005
                                      + DED-AMT-DOLLARS(LINE-INDEX)     34120005
                                      + COINSURANCE-AMT(LINE-INDEX)     34130005
                                      + COPAYMENT-AMT(LINE-INDEX)       34140005
NRB062                                                                  34150005
NRB062              WHEN GENO-OUTOFPOCKET-RBB(GENO-CO-MIN-INDEX)        34160005
                                                                        34170005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34180005
                                                                        34190005
NRB062                 COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34200005
NRB062                                + DED-AMT-DOLLARS(LINE-INDEX)     34210005
NRB062                                + COINSURANCE-AMT(LINE-INDEX)     34220005
NRB062                                + RBB-EXCESS-OOP(LINE-INDEX)      34230005
NRB062                                                                  34240005
NRB062              WHEN GENO-OOP-COPAY-RBB(GENO-CO-MIN-INDEX)          34250005
NRB062                                                                  34260005
NRB062                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34270005
NRB062                                                                  34280005
NRB062                 COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34290005
NRB062                                + DED-AMT-DOLLARS(LINE-INDEX)     34300005
NRB062                                + COINSURANCE-AMT(LINE-INDEX)     34310005
NRB062                                + COPAYMENT-AMT(LINE-INDEX)       34320005
NRB062                                + RBB-EXCESS-OOP(LINE-INDEX)      34330005
NRB062                                                                  34340005
NR0007              WHEN GENO-LTIMEMAXPAID(GENO-CO-MIN-INDEX)           34350005
ANO609                                                                  34360005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34370005
                                                                        34380005
                       COMPUTE W-LIFETIME-MAX =  W-LIFETIME-MAX         34390005
                                      + PRORT-AMT(LINE-INDEX)           34400005
ANI612                 SET   SW-PAID-YES             TO TRUE            34410005
                                                                        34420005
NR0007              WHEN GENO-COINSURANCE(GENO-CO-MIN-INDEX)            34430005
                                                                        34440005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34450005
                                                                        34460005
                       COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34470005
                                      + COINSURANCE-AMT(LINE-INDEX)     34480005
                       COMPUTE W-COINSURANCE = W-COINSURANCE            34490005
                                      + COINSURANCE-AMT(LINE-INDEX)     34500005
NRB066                                                                  34510005
NRB066              WHEN GENO-OOP-COPAY-RBB(GENO-CO-MIN-INDEX)          34520005
NRB066                 COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34530005
NRB066                                + DED-AMT-DOLLARS(LINE-INDEX)     34540005
NRB066                                + COINSURANCE-AMT(LINE-INDEX)     34550005
NRB066                                + COPAYMENT-AMT(LINE-INDEX)       34560005
NRB066                                + RBB-EXCESS-OOP(LINE-INDEX)      34570005
                                                                        34580005
NR0007              WHEN GENO-CO-PAY(GENO-CO-MIN-INDEX)                 34590005
                                                                        34600005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34610005
                                                                        34620005
                       COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34630005
                                      + COPAYMENT-AMT(LINE-INDEX)       34640005
                       COMPUTE W-COPAYMENT-AMT = W-COPAYMENT-AMT        34650005
                                      + COPAYMENT-AMT(LINE-INDEX)       34660005
                                                                        34670005
NR0007              WHEN GENO-COINS-COPAY(GENO-CO-MIN-INDEX)            34680005
                                                                        34690005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34700005
                                                                        34710005
                       COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34720005
                                      + COPAYMENT-AMT(LINE-INDEX)       34730005
                                      + COINSURANCE-AMT(LINE-INDEX)     34740005
                                                                        34750005
NR0007              WHEN GENO-DED-COINS  (GENO-CO-MIN-INDEX)            34760005
                                                                        34770005
                       COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET        34780005
                                      + COINSURANCE-AMT(LINE-INDEX)     34790005
                                      + DED-AMT-DOLLARS(LINE-INDEX)     34800005
                                                                        34810005
NR0007              WHEN GENO-UNITS(GENO-CO-MIN-INDEX)                  34820005
                                                                        34830005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34840005
                                                                        34850005
                       COMPUTE W-SERV-UNITS = W-SERV-UNITS              34860005
                                      + SERV-UNITS-3-BYTES(LINE-INDEX)  34870005
                                                                        34880005
NR0007              WHEN GENO-ALL-COMINGLE(GENO-CO-MIN-INDEX)           34890005
                                                                        34900005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          34910005
                                                                        34920005
                       COMPUTE CAR-OOP-AMT = CAR-OOP-AMT                34930005
                                      + DED-AMT-DOLLARS(LINE-INDEX)     34940005
                                      + COINSURANCE-AMT(LINE-INDEX)     34950005
                                      + COPAYMENT-AMT(LINE-INDEX)       34960005
                                                                        34970005
                       COMPUTE W-LIFETIME-MAX =  W-LIFETIME-MAX         34980005
                                      + PRORT-AMT(LINE-INDEX)           34990005
ANI612                 SET  SW-PAID-YES             TO TRUE             35000005
                                                                        35010005
                       COMPUTE W-COINSURANCE = W-COINSURANCE            35020005
                                      + COINSURANCE-AMT(LINE-INDEX)     35030005
                                                                        35040005
                       COMPUTE W-COPAYMENT-AMT = W-COPAYMENT-AMT        35050005
                                      + COPAYMENT-AMT(LINE-INDEX)       35060005
                                                                        35070005
                       COMPUTE W-SERV-UNITS = W-SERV-UNITS              35080005
                                      + SERV-UNITS-3-BYTES(LINE-INDEX)  35090005
                                                                        35100005
AN9308                                                                  35110005
NR0007              WHEN GENO-DED-COINS-F (GENO-CO-MIN-INDEX)           35120005
                                                                        35130005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          35140005
                                                                        35150005
AN9308                IF HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = 'I'    35160005
AN9308                  OR HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = SPACE35170005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35180005
AN9308                                + DED-AMT-DOLLARS (LINE-INDEX)    35190005
AN9308                                + COINSURANCE-AMT (LINE-INDEX)    35200005
AN9308                ELSE                                              35210005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35220005
AN9308                                + DED-AMT-DOLLARS (LINE-INDEX)    35230005
AN7535                    MOVE +0  TO  W-OUT-OF-POCKET                  35240005
AN9308                END-IF                                            35250005
AN9308                                                                  35260005
NR0007              WHEN GENO-DED-COINS-G (GENO-CO-MIN-INDEX)           35270005
                                                                        35280005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          35290005
                                                                        35300005
AN9308                IF HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = 'I'    35310005
AN9308                  OR HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = SPACE35320005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35330005
AN9308                                + COINSURANCE-AMT (LINE-INDEX)    35340005
AN9308                                + DED-AMT-DOLLARS (LINE-INDEX)    35350005
AN9308                ELSE                                              35360005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35370005
AN9308                                + COINSURANCE-AMT (LINE-INDEX)    35380005
AN7535                                + DED-AMT-DOLLARS (LINE-INDEX)    35390005
AN9308                    MOVE +0  TO  W-DED-AMT-DOLLAR                 35400005
AN9308                END-IF                                            35410005
AN9308                                                                  35420005
NR0007              WHEN GENO-DED-COINS-COPAY-H (GENO-CO-MIN-INDEX)     35430005
                                                                        35440005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          35450005
                                                                        35460005
AN9308                IF HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = 'I'    35470005
AN9308                  OR HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = SPACE35480005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35490005
AN9308                                + DED-AMT-DOLLARS (LINE-INDEX)    35500005
AN9308                                + COPAYMENT-AMT (LINE-INDEX)      35510005
AN9308                                + COINSURANCE-AMT (LINE-INDEX)    35520005
AN9308                ELSE                                              35530005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35540005
AN9308                                + DED-AMT-DOLLARS (LINE-INDEX)    35550005
AN9308                                + COPAYMENT-AMT (LINE-INDEX)      35560005
AN7535                    MOVE +0  TO  W-OUT-OF-POCKET                  35570005
AN9308                END-IF                                            35580005
AN9308                                                                  35590005
NR0007              WHEN GENO-DED-COINS-COPAY-J (GENO-CO-MIN-INDEX)     35600005
                                                                        35610005
AN0609                 SET SW-COMINGLE-NT-L-O-Y        TO TRUE          35620005
                                                                        35630005
AN9308                IF HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = 'I'    35640005
AN9308                  OR HMO-IN-OUT-NETWORK-INDIC (LINE-INDEX) = SPACE35650005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35660005
AN9308                                + COINSURANCE-AMT (LINE-INDEX)    35670005
AN9308                                + DED-AMT-DOLLARS (LINE-INDEX)    35680005
AN9308                                + COPAYMENT-AMT (LINE-INDEX)      35690005
AN9308                ELSE                                              35700005
AN9308                    COMPUTE W-OUT-OF-POCKET = W-OUT-OF-POCKET     35710005
AN9308                                + COINSURANCE-AMT (LINE-INDEX)    35720005
AN7535                                + DED-AMT-DOLLARS (LINE-INDEX)    35730005
AN7535                                + COPAYMENT-AMT (LINE-INDEX)      35740005
AN9308                    MOVE +0  TO  W-DED-AMT-DOLLAR                 35750005
AN9308                END-IF                                            35760005
AN9308                                                                  35770005
                                                                        35780005
                    WHEN OTHER                                          35790005
                       CONTINUE                                         35800005
                 END-EVALUATE                                           35810005
               END-PERFORM                                              35820005
           END-IF                                                       35830005
PERIOD     .                                                            35840005
PERIOD**--/ \                                                           35850005
                                                                        35860005
                                                                        35870005
MI2155***************************************************************** 35880005
MI2155*              START THE GENO(VSAM) MAPPING FILE                  35890005
MI2155***************************************************************** 35900005
MI2155 3000-START-MAPPING-FILE.                                         35910005
MI2155                                                                  35920005
MI2155     SET  MAPPING-NOT-FOUND         TO TRUE                       35930005
MI2155                                                                  35940005
MI2155     MOVE ZEROES                    TO GENO-SEQUENCE-NUM          35950005
MI2155     MOVE SUBSCRIBER-PLAN           TO GENO-PLAN-CODEA            35960005
MI2155     MOVE GENO-VENDOR-ID            TO GENO-VENDOR-CODE           35970005
MI2155     MOVE GRP-BASE(LINE-INDEX)      TO GENO-GROUP-BASE-NO         35980005
MI2155     MOVE GRP-SEC(LINE-INDEX)       TO GENO-GROUP-SECTION-NO      35990005
MI2155     MOVE PACKAGE-CODE(LINE-INDEX)  TO W-PACK-PICX                36000005
MI2155     MOVE W-PACK-CODE-BINARY        TO GENO-PACKAGE-CODEA         36010005
MI2155                                                                  36020005
MI2155     START GENO-FIELD-MAPPING-FILE KEY >= GENO-VENDOR-KEY         36030005
MI2155                                                                  36040005
MI2155          INVALID KEY SET MAPPING-NOT-FOUND TO TRUE               36050005
MI2155      NOT INVALID KEY SET MAPPING-FOUND     TO TRUE               36060005
MI2155     END-START                                                    36070005
MI2155                                                                  36080005
MI2155     IF  W-HRBNGENO-REC-NOT-FND OR W-HRBNGENO-GOOD-READ           36090005
MI2155         SET NOT-CLINT-ID-FND  TO TRUE                            36100005
MI2155     ELSE                                                         36110005
MI2155         DISPLAY 'RETURN CODE OF FILE OPERATION START : '         36120005
MI2155                   W-HRBNGENO-STATUS                              36130005
MI2155         MOVE BAD-RD-HRBNGENO-ABND  TO ABEND-CODE                 36140005
MI2155         MOVE BAD-RD-HRBNGENO-MSG   TO ABEND-MESSAGE              36150005
MI2155         PERFORM PROGRAM-ABEND                                    36160005
MI2155     END-IF                                                       36170005
MI2155     .                                                            36180005
PERIOD****/ \                                                           36190005
                                                                        36200005
MI2155 3000-EXIT.                                                       36210005
MI2155     EXIT.                                                        36220005
MI2155                                                                  36230005
MI2155***************************************************************** 36240005
MI2155*              READING THE GENO(VSAM) MAPPING FILE                36250005
MI2155***************************************************************** 36260005
MI2155 3100-READ-NEXT-MAPPING-REC.                                      36270005
MI2155                                                                  36280005
MI2155     READ GENO-FIELD-MAPPING-FILE NEXT RECORD                     36290005
MI2155          AT END                                                  36300005
MI2155             SET CHANGE-OF-KEY TO TRUE                            36310005
MI2155     END-READ                                                     36320005
MI2155                                                                  36330005
MI2155     IF  W-HRBNGENO-GOOD-READ                                     36340005
MI2155               IF GENO-GROUP-BASE-NO = GRP-BASE(LINE-INDEX)       36350005
MI2155                                                                  36360005
MI2155                 EVALUATE GENO-FIELD-NAME                         36370005
MI2155                    WHEN 'CLINTID '                               36380005
AN1887                      IF SUBSCRIBER-PLAN = 834                    36390005
AN1887                         IF GENO-VENDOR-ID = GENO-VENDOR-CODE     36400005
NX1471                            AND (GENO-GROUP-SECTION-NO =          36410005
NX1471                            GRP-SEC(LINE-INDEX) OR                36420005
NX1471                            GENO-GROUP-SECTION-NO = '****')       36430005
NX1471                            AND (GENO-PACKAGE-CODEA =             36440005
NX1471                                 W-PACK-CODE-BINARY OR            36450005
NX1471                                 GENO-PACKAGE-CODEA = 000)        36460005
AN1887                            MOVE GENO-FIELD-VALUE TO              36470005
AN1887                                             CAR-REC-ID-NAME      36480005
AN1887                            SET CLINT-ID-FND TO TRUE              36490005
AN1887                         END-IF                                   36500005
AN1887                      ELSE                                        36510005
MI2155                         MOVE GENO-FIELD-VALUE TO CAR-REC-ID-NAME 36520005
MI2155                         SET CLINT-ID-FND  TO TRUE                36530005
AN1887                      END-IF                                      36540005
MI2155                                                                  36550005
MI2155                    WHEN OTHER                                    36560005
MI2155                         CONTINUE                                 36570005
MI2155                 END-EVALUATE                                     36580005
MI2155               END-IF                                             36590005
MI2155     ELSE                                                         36600005
MI2155         IF  W-HRBNGENO-REC-NOT-FND OR W-HRBNGENO-INV-NEXT-REC    36610005
MI2155            SET CHANGE-OF-KEY TO TRUE                             36620005
MI2155         ELSE                                                     36630005
MI2155             DISPLAY 'RETURN CODE OF FILE OPERATION READ: '       36640005
MI2155                        W-HRBNGENO-STATUS                         36650005
MI2155             MOVE BAD-RD-HRBNGENO-ABND  TO ABEND-CODE             36660005
MI2155             MOVE BAD-RD-HRBNGENO-MSG   TO ABEND-MESSAGE          36670005
MI2155             PERFORM PROGRAM-ABEND                                36680005
MI2155         END-IF                                                   36690005
MI2155     END-IF                                                       36700005
MI2155     .                                                            36710005
PERIOD****/ \                                                           36720005
                                                                        36730005
MI2155 3100-EXIT.                                                       36740005
MI2155     EXIT                                                         36750005
MI2155     .                                                            36760005
PERIOD****/ \                                                           36770005
NJ0983                                                                  36780005
NJ0983/*****************************************************************36790005
NJ0983*         4 0 0 0 - M A P - F R O M - I N V E N O M P             36800005
NJ0983* - PASS THE CAREMARK RECORD TO MAP VALUES FROM INVENOMP          36810005
NJ0983******************************************************************36820005
NJ0983                                                                  36830005
NJ0983 4000-MAP-FROM-INVENOMP.                                          36840005
NJ0983                                                                  36850005
NJ0983     INITIALIZE HRHSINVM-PARMS                                    36860005
                                                                        36870005
NJ0983     SET L-INVM-PROCESSING-CALL      TO TRUE                      36880005
                                                                        36890005
NJ0983     MOVE SUBSCRIBER-PLAN            TO L-INVM-SUB-PLAN           36900005
NJ0983     MOVE GENO-VENDOR-ID             TO L-INVM-VENDOR-ID          36910005
NJ0983     MOVE GRP-BASE(LINE-INDEX)       TO L-INVM-GP-BASE            36920005
NJ0983     MOVE GRP-SEC(LINE-INDEX)        TO L-INVM-GP-SEC             36930005
NJ0983     MOVE PACKAGE-CODE(LINE-INDEX)   TO W-PKG-LOW-ORDER           36940005
NJ0983     MOVE W-PKG-BINARY               TO L-INVM-GP-PKG             36950005
NJ0983     MOVE CAR-CLM-DTE-SERVICE-CCYYMMDD                            36960005
NJ0983                                     TO L-INVM-FIRST-DOS          36970005
NJ0983     MOVE CAREMARK-HORIZON-RECORD    TO                           36980005
NJ0983          L-INVM-RETURN-OUTBOUND-RECORD                           36990005
NJ0983     CALL W-HRHSINVM USING HRHSINVM-PARMS                         37000005
                                                                        37010005
NJ0983     IF L-INVM-RETURN-CODE = ZEROES                               37020005
                                                                        37030005
NJ0983        MOVE L-INVM-RETURN-OUTBOUND-RECORD                        37040005
NJ0983             (1:LENGTH OF CAREMARK-HORIZON-RECORD)                37050005
NJ0983                                     TO CAREMARK-HORIZON-RECORD   37060005
NJ0983     END-IF                                                       37070005
NJ0983     .                                                            37080005
PERIOD****/ \                                                           37090005
NJ9032******************************************************************37100005
NJ9032*         4 0 0 1 - C A L L - H R H S E H N C                     37110005
NJ9032******************************************************************37120005
NJ9032                                                                  37130005
NJ9032 4001-CALL-HRHSEHNC.                                              37140005
NJ9032                                                                  37150005
NJ9032     INITIALIZE HRHSEHNC-PARMS                                    37160005
NJ9032                HOLD-RENEWAL-DATE                                 37170005
NJ9032     SET L-MOOP-HNDI-PROCESSING-CALL   TO TRUE                    37180005
NJ9032     SET L-MOOP-HNDO-NO-MATCH          TO TRUE                    37190005
NJ9032     MOVE SUBSCRIBER-PLAN              TO L-MOOP-HNDI-SUB-PLAN    37200005
NJ9032     MOVE GRP-BASE(LINE-INDEX)         TO L-MOOP-HNDI-GROUP-BASE  37210005
NJ9032     MOVE GRP-SEC(LINE-INDEX)          TO L-MOOP-HNDI-GROUP-SECT  37220005
NJ9032*    CHANGE THIS TO THE VENDOR-ID FROM HRHPAO2K (FRM INVENOUT)    37230005
NJ9032     MOVE GENO-VENDOR-ID               TO L-MOOP-HNDI-VENDOR-ID   37240005
NJ9032     MOVE PACKAGE-CODE(LINE-INDEX)     TO W-PKG-LOW-ORDER         37250005
NJ9032     MOVE W-PKG-BINARY                 TO L-MOOP-HNDI-PACK-CODE   37260005
NJ9032*    DATE OF SERVICE SHOULD BE IN CCYYMMDD FORMAT                 37270005
NJ9032     MOVE CAR-CLM-DTE-SERVICE-CCYYMMDD TO L-MOOP-HNDI-DOS         37280005
NJ9032                                                                  37290005
NJ9032*    RENEWAL DATE RETURNED IS IN CCYYMMDD FORMAT                  37300005
NJ9032     CALL W-HRHSEHNC USING HRHSEHNC-PARMS                         37310005
NJ9032     .                                                            37320005
PERIOD****/ \                                                           37330005
MD7684/*****************************************************************37340005
MD7684*         4 0 0 0 - M A P - F R O M - H D R T 5 W 2 K             37350005
MD7684* - PASS PLAN CODE, GROUP BASE, GROUP SECTION AND PACKAGE CODE    37360005
MD7684* - IN ORDER TO READ HDRT5W2K TABLE AND IF ALL MATCHES, THEN      37370005
MD7684* - GET THE VALUE BACK FROM CALLED MODULE                         37380005
MD7684******************************************************************37390005
MD7684                                                                  37400005
MD7684 4000-MAP-FROM-HDRT5W2K.                                          37410005
MD7684                                                                  37420005
MD7684     INITIALIZE HRHST5W2-PARMS                                    37430005
MD7684                                                                  37440005
MD7684     SET SW-T5W2K-MATCH-NOT-FOUND    TO TRUE                      37450005
MD7684     SET L-T5W2-PROCESSING-CALL      TO TRUE                      37460005
MD7684                                                                  37470005
MD7684     MOVE SUBSCRIBER-PLAN            TO L-T5W2-SUB-PLAN           37480005
MD7684     MOVE GRP-BASE(LINE-INDEX)       TO L-T5W2-GP-BASE            37490005
MD7684     MOVE GRP-SEC(LINE-INDEX)        TO L-T5W2-GP-SEC             37500005
MD7684     MOVE PACKAGE-CODE(LINE-INDEX)   TO W-PKG-LOW-ORDER           37510005
MD7684     MOVE W-PKG-BINARY               TO W-PACKAGE-CODE-X          37520005
MD7684     MOVE W-PKG-CODE-X               TO L-T5W2-GP-PKG             37530005
MD7684     MOVE CAR-CLM-DTE-SERVICE-CCYYMMDD                            37540005
MD7684                                     TO L-T5W2-FIRST-DOS          37550005
MD7684     CALL W-HRHST5W2 USING HRHST5W2-PARMS                         37560005
MD7684                                                                  37570005
MD7684     IF L-T5W2-RETURN-CODE = ZEROES                               37580005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(1:4)                   37590005
MD7684                                     TO CAR-CARRIER-NUMBER        37600005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(6:7)                   37610005
MD7684                              TO CAR-PARTICIPANT-ACCT-NUMBER(1:7) 37620005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(13:4)                  37630005
MD7684                              TO CAR-PARTICIPANT-ACCT-NUMBER(8:4) 37640005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(17:3)                  37650005
MD7684                              TO CAR-PARTICIPANT-ACCT-NUMBER(12:3)37660005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(20:2)                  37670005
MD7684                              TO CAR-GROUP-NUMBER(1:2)            37680005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(22:2)                  37690005
MD7684                              TO CAR-GROUP-NUMBER(3:2)            37700005
MD7684        MOVE L-T5W2-RETURN-OUTBOUND-RECORD(24:2)                  37710005
MD7684                              TO CAR-GROUP-NUMBER(5:2)            37720005
MD7684        MOVE SPACES           TO CAR-GROUP-NUMBER(7:9)            37730005
MD7684        INITIALIZE L-T5W2-RETURN-OUTBOUND-RECORD                  37740005
MD7684        SET SW-T5W2K-MATCH-FOUND     TO TRUE                      37750005
MD7684     ELSE                                                         37760005
MD7684        SET SW-T5W2K-MATCH-NOT-FOUND TO TRUE                      37770005
MD7684     END-IF                                                       37780005
MD7684     .                                                            37790005
PERIOD****/ \                                                           37800005
      /**************************************************************** 37810005
      *          W R I T E - H E A D E R                                37820005
      * - POPULATE CREATION-DATE WITH CURRENT SYSTEM DATE IN CYYMMDD    37830005
      * - WRITE HEADER                                                  37840005
      * - CHECK FOR GOOD WRITE AND KEEP TRACK OF COUNT                  37850005
      ******************************************************************37860005
                                                                        37870005
       WRITE-HEADER.                                                    37880005
                                                                        37890005
           MOVE SPACES                    TO CAREMARK-HEADER-RECORD     37900005
                                             CAR-BAT-RESPOND-STATUS     37910005
                                             CAR-BAT-REJ-CODE           37920005
                                                                        37930005
PRODSW     SET  CAR-BAT-PRODUTION-DATA,                                 37940005
                CAR-BAT-REQUEST,                                        37950005
                HEADER-REC,                                             37960005
                CAR-FIRST-TRANS           TO TRUE                       37970005
                                                                        37980005
           ACCEPT W-YYMMDD FROM DATE                                    37990005
           ACCEPT W-HHMMSS FROM TIME                                    38000005
                                                                        38010005
           MOVE W-HHMMSS-REDEF            TO W-HHMMSS-X                 38020005
           MOVE W-TIME-HOLD               TO CAR-CREATE-TME-HHMMSSMMMM  38030005
           MOVE CAR-CREATE-TME-HHMMSSMMMM TO                            38040005
                CAR-PROC-END-TME-HHMMSSMMMM                             38050005
                                                                        38060005
           MOVE C-YYMMDD                  TO W-FORMAT-1                 38070005
           MOVE C-YYYYMMDD                TO W-FORMAT-2                 38080005
           MOVE C-CONVERT-CODE            TO W-FUNCTION-CODE            38090005
           MOVE W-YYMMDD                  TO W-DATE-1                   38100005
                                                                        38110005
           PERFORM CONVERT-DATE                                         38120005
                                                                        38130005
           MOVE W-DATE-2(1:8)             TO CAR-CREATE-DTE-CCYYMMDD    38140005
           MOVE CAR-CREATE-DTE-CCYYMMDD   TO CAR-PROC-END-DTE-CCYYMMDD  38150005
                                                                        38160005
           MOVE C-FOUR-ZEROS              TO CAR-BAT-ID(1:4)            38170005
           MOVE W-DATE-2(1:8)             TO CAR-BAT-ID(5:8)            38180005
           MOVE W-TIME-HOLD               TO CAR-BAT-ID(13:10)          38190005
           MOVE W-DATE-2(1:8)             TO CAR-PROC-STR-DTE-CCYYMMDD  38200005
           MOVE W-TIME-HOLD               TO CAR-PROC-STR-TME-HHMMSSMMMM38210005
                                                                        38220005
                                                                        38230005
AN4993                                                                  38240005
AN4993     EVALUATE TRUE                                                38250005
                                                                        38260005
NR0007        WHEN  GENO-FIRST-CALL                                     38270005
                                                                        38280005
AN4993           MOVE C-SENDER-ID     TO CAR-BAT-SENDER-ID              38290005
                                                                        38300005
AN4993           MOVE C-SENDER-NAME   TO CAR-BAT-SENDER-NAME            38310005
AN4993           MOVE C-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID            38320005
AN4993           MOVE C-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME          38330005
ANI612           ADD +1               TO                                38340005
ANI612               WK-WYETH-HEAD-CNT                                  38350005
AN4993           PERFORM WRITE-HEADER-WYETH                             38360005
AN4993                                                                  38370005
AN4993           MOVE A-SENDER-ID     TO CAR-BAT-SENDER-ID              38380005
AN4993           MOVE A-SENDER-NAME   TO CAR-BAT-SENDER-NAME            38390005
AN4993           MOVE A-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID            38400005
AN4993           MOVE A-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME          38410005
                                                                        38420005
ANI612           ADD +1               TO                                38430005
ANI612               WK-ACARE-HEAD-CNT                                  38440005
                                                                        38450005
                                                                        38460005
AN4993           PERFORM WRITE-HEADER-ACARE                             38470005
                                                                        38480005
MI1323           MOVE CHR-SENDER-ID     TO CAR-BAT-SENDER-ID            38490005
MI1323           MOVE CHR-SENDER-NAME   TO CAR-BAT-SENDER-NAME          38500005
MI1323           MOVE CHR-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID          38510005
MI1323           MOVE CHR-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME        38520005
                                                                        38530005
ANI612           ADD +1                TO                               38540005
ANI612               WK-MCARE-HEAD-CNT                                  38550005
                                                                        38560005
MI1323           PERFORM WRITE-HEADER-MCARE                             38570005
                                                                        38580005
NJ1164           MOVE NJ-SENDER-ID     TO CAR-BAT-SENDER-ID             38590005
NJ1164           MOVE NJ-SENDER-NAME   TO CAR-BAT-SENDER-NAME           38600005
NJ1164           MOVE NJ-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID           38610005
NJ1164           MOVE NJ-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME         38620005
                                                                        38630005
ANI612           ADD +1                TO                               38640005
ANI612               WK-NJCDH-HEAD-CNT                                  38650005
                                                                        38660005
NJ1164           PERFORM WRITE-HEADER-NJCDH                             38670005
AN1887                                                                  38680005
AN1887           MOVE AN-SENDER-ID     TO CAR-BAT-SENDER-ID             38690005
AN1887           MOVE AN-SENDER-NAME   TO CAR-BAT-SENDER-NAME           38700005
AN1887           MOVE AN-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID           38710005
AN1887           MOVE AN-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME         38720005
                                                                        38730005
ANI612           ADD +1               TO                                38740005
ANI612               WK-AOPTM-HEAD-CNT                                  38750005
                                                                        38760005
AN1887           PERFORM WRITE-HEADER-AOPTM                             38770005
AN1589                                                                  38780005
AN1589           MOVE AN-SENDER-ID-MAG     TO CAR-BAT-SENDER-ID         38790005
AN1589           MOVE AN-SENDER-NAME-MAG   TO CAR-BAT-SENDER-NAME       38800005
AN1589           MOVE AN-RECEIVER-ID-MAG   TO CAR-BAT-RECIEVER-ID       38810005
AN1589           MOVE AN-RECEIVER-NAME-MAG TO CAR-BAT-RECIEVER-NAME     38820005
AN1589           ADD +1               TO                                38830005
AN1589               WK-MAGLN-HEAD-CNT                                  38840005
AN1589           PERFORM WRITE-HEADER-MAGLN                             38850005
AN1589                                                                  38860005
AN0127           MOVE AN-SENDER-ID-BCN     TO CAR-BAT-SENDER-ID         38870005
AN0127           MOVE AN-SENDER-NAME-BCN   TO CAR-BAT-SENDER-NAME       38880005
AN0127           MOVE AN-RECEIVER-ID-BCN   TO CAR-BAT-RECIEVER-ID       38890005
AN0127           MOVE AN-RECEIVER-NAME-BCN TO CAR-BAT-RECIEVER-NAME     38900005
AN0127           ADD +1               TO                                38910005
AN0127               WK-BEACN-HEAD-CNT                                  38920005
AN0127           PERFORM WRITE-HEADER-BEACN                             38930005
AN0127                                                                  38940005
AN1560                                                                  38950005
AN1560           MOVE ANR-SENDER-ID        TO CAR-BAT-SENDER-ID         38960005
AN1560           MOVE ANR-SENDER-NAME      TO CAR-BAT-SENDER-NAME       38970005
AN1560           MOVE ANR-RECEIVER-ID      TO CAR-BAT-RECIEVER-ID       38980005
AN1560           MOVE ANR-RECEIVER-NAME    TO CAR-BAT-RECIEVER-NAME     38990005
AN1560           ADD +1               TO                                39000005
AN1560               WK-AOPTR-HEAD-CNT                                  39010005
AN1560           PERFORM WRITE-HEADER-AOPTR                             39020005
AN1560                                                                  39030005
AN1781                                                                  39040005
AN1781           MOVE AAR-SENDER-ID        TO CAR-BAT-SENDER-ID         39050005
AN1781           MOVE AAR-SENDER-NAME      TO CAR-BAT-SENDER-NAME       39060005
AN1781           MOVE AAR-RECEIVER-ID      TO CAR-BAT-RECIEVER-ID       39061005
AN1781           MOVE AAR-RECEIVER-NAME    TO CAR-BAT-RECIEVER-NAME     39062005
AN1781           ADD +1               TO                                39063005
AN1781               WK-AARCH-HEAD-CNT                                  39064005
AN1781           PERFORM WRITE-HEADER-AARCH                             39065005
AN1781                                                                  39066005
MA2010           MOVE CMK-SENDER-ID     TO CAR-BAT-SENDER-ID            39067005
MA2010           MOVE CMK-SENDER-NAME   TO CAR-BAT-SENDER-NAME          39068005
MA2010           MOVE CMK-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID          39069005
MA2010           MOVE CMK-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME        39070005
                                                                        39080005
ANI612           ADD +1               TO                                39090005
ANI612               WK-MACMK-HEAD-CNT                                  39100005
                                                                        39110005
MA2010           PERFORM WRITE-HEADER-MACMK                             39120005
MD2689           MOVE CCK-SENDER-ID     TO CAR-BAT-SENDER-ID            39130005
MD2689           MOVE CCK-SENDER-NAME   TO CAR-BAT-SENDER-NAME          39140005
MD2689           MOVE CCK-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID          39150005
MD2689           MOVE CCK-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME        39160005
MD2689           PERFORM WRITE-HEADER-CCARE                             39170005
                                                                        39180005
MD6619           MOVE CVS-SENDER-ID     TO CAR-BAT-SENDER-ID            39190005
MD6619           MOVE CVS-SENDER-NAME   TO CAR-BAT-SENDER-NAME          39200005
MD6619           MOVE CVS-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID          39210005
MD6619           MOVE CVS-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME        39220005
MD6619                                                                  39230005
MD6619           ADD +1               TO                                39240005
MD6619               WK-CVSCM-HEAD-CNT                                  39250005
MD6619                                                                  39260005
MD6619           PERFORM WRITE-HEADER-CVSCM                             39270005
                                                                        39280005
NR0007        WHEN  GENO-PROCESSING-CALL                                39290005
NR0007             IF GENO-VENDOR-ACARE                                 39300005
AN4993                MOVE A-SENDER-ID     TO CAR-BAT-SENDER-ID         39310005
AN4993                MOVE A-SENDER-NAME   TO CAR-BAT-SENDER-NAME       39320005
AN4993                MOVE A-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID       39330005
AN4993                MOVE A-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME     39340005
                                                                        39350005
ANI612                ADD +1               TO                           39360005
ANI612                    WK-ACARE-HEAD-CNT                             39370005
                                                                        39380005
AN4993                PERFORM WRITE-HEADER-ACARE                        39390005
AN1887             ELSE                                                 39400005
NR0007              IF GENO-VENDOR-AOPTM THEN                           39410005
AN1887                MOVE AN-SENDER-ID     TO CAR-BAT-SENDER-ID        39420005
AN1887                MOVE AN-SENDER-NAME   TO CAR-BAT-SENDER-NAME      39430005
AN1887                MOVE AN-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID      39440005
AN1887                MOVE AN-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME    39450005
                                                                        39460005
ANI612                ADD +1               TO                           39470005
ANI612                    WK-AOPTM-HEAD-CNT                             39480005
                                                                        39490005
AN1887                PERFORM WRITE-HEADER-AOPTM                        39500005
AN1589             ELSE                                                 39510005
AN1589              IF GENO-VENDOR-MAGLN THEN                           39520005
AN1589                MOVE AN-SENDER-ID-MAG     TO CAR-BAT-SENDER-ID    39530005
AN1589                MOVE AN-SENDER-NAME-MAG   TO CAR-BAT-SENDER-NAME  39540005
AN1589                MOVE AN-RECEIVER-ID-MAG   TO CAR-BAT-RECIEVER-ID  39550005
AN1589                MOVE AN-RECEIVER-NAME-MAG TO CAR-BAT-RECIEVER-NAME39560005
AN1589                                                                  39570005
AN1589                ADD +1               TO                           39580005
AN1589                    WK-MAGLN-HEAD-CNT                             39590005
AN1589                                                                  39600005
AN1589                PERFORM WRITE-HEADER-MAGLN                        39610005
AN0127             ELSE                                                 39620005
AN0127              IF GENO-VENDOR-BEACN THEN                           39630005
AN0127                MOVE AN-SENDER-ID-BCN     TO CAR-BAT-SENDER-ID    39640005
AN0127                MOVE AN-SENDER-NAME-BCN   TO CAR-BAT-SENDER-NAME  39650005
AN0127                MOVE AN-RECEIVER-ID-BCN   TO CAR-BAT-RECIEVER-ID  39660005
AN0127                MOVE AN-RECEIVER-NAME-BCN TO CAR-BAT-RECIEVER-NAME39670005
AN0127                                                                  39680005
AN0127                ADD +1               TO                           39690005
AN0127                    WK-BEACN-HEAD-CNT                             39700005
AN0127                                                                  39710005
AN0127                PERFORM WRITE-HEADER-BEACN                        39720005
AN1560             ELSE                                                 39730005
AN1560              IF GENO-VENDOR-AOPTR THEN                           39740005
AN1560                MOVE ANR-SENDER-ID     TO CAR-BAT-SENDER-ID       39750005
AN1560                MOVE ANR-SENDER-NAME   TO CAR-BAT-SENDER-NAME     39751005
AN1560                MOVE ANR-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID     39752005
AN1560                MOVE ANR-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME   39753005
AN1560                                                                  39754005
AN1560                ADD +1               TO                           39755005
AN1560                    WK-AOPTR-HEAD-CNT                             39756005
AN1560                                                                  39757005
AN1560                PERFORM WRITE-HEADER-AOPTR                        39758005
AN1781             ELSE                                                 39759005
AN1781              IF GENO-VENDOR-AARCH THEN                           39759105
AN1781                MOVE AAR-SENDER-ID     TO CAR-BAT-SENDER-ID       39759205
AN1781                MOVE AAR-SENDER-NAME   TO CAR-BAT-SENDER-NAME     39759305
AN1781                MOVE AAR-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID     39759405
AN1781                MOVE AAR-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME   39759505
AN1781                                                                  39759605
AN1781                ADD +1               TO                           39759705
AN1781                    WK-AARCH-HEAD-CNT                             39759805
AN1781                                                                  39759905
AN1781                PERFORM WRITE-HEADER-AARCH                        39760005
AN4993             ELSE                                                 39760105
NR0007              IF GENO-VENDOR-MCARE THEN                           39760205
MI1323                MOVE CHR-SENDER-ID     TO CAR-BAT-SENDER-ID       39760305
MI1323                MOVE CHR-SENDER-NAME   TO CAR-BAT-SENDER-NAME     39760405
MI1323                MOVE CHR-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID     39760505
MI1323                MOVE CHR-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME   39760605
                                                                        39760705
ANI612                ADD +1               TO                           39760805
ANI612                    WK-MCARE-HEAD-CNT                             39760905
                                                                        39761005
MI1323                PERFORM WRITE-HEADER-MCARE                        39762005
MI1323              ELSE                                                39763005
NR0007               IF GENO-VENDOR-NJCDH THEN                          39764005
NJ1164                MOVE NJ-SENDER-ID     TO CAR-BAT-SENDER-ID        39765005
NJ1164                MOVE NJ-SENDER-NAME   TO CAR-BAT-SENDER-NAME      39766005
NJ1164                MOVE NJ-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID      39767005
NJ1164                MOVE NJ-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME    39768005
                                                                        39769005
ANI612                ADD +1               TO                           39770005
ANI612                    WK-NJCDH-HEAD-CNT                             39780005
                                                                        39790005
NJ1164                PERFORM WRITE-HEADER-NJCDH                        39800005
NJ1164               ELSE                                               39810005
NR0007                IF GENO-VENDOR-MACMK THEN                         39820005
MA2010                 MOVE CMK-SENDER-ID     TO CAR-BAT-SENDER-ID      39830005
MA2010                 MOVE CMK-SENDER-NAME   TO CAR-BAT-SENDER-NAME    39840005
MA2010                 MOVE CMK-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID    39850005
MA2010                 MOVE CMK-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME  39860005
                                                                        39870005
ANI612                 ADD +1               TO                          39880005
ANI612                     WK-MACMK-HEAD-CNT                            39890005
                                                                        39900005
MA2010                 PERFORM WRITE-HEADER-MACMK                       39910005
MA2010                ELSE                                              39920005
NR0007                 IF GENO-VENDOR-CCARE THEN                        39930005
MD2689                  MOVE CCK-SENDER-ID     TO CAR-BAT-SENDER-ID     39940005
MD2689                  MOVE CCK-SENDER-NAME   TO CAR-BAT-SENDER-NAME   39950005
MD2689                  MOVE CCK-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID   39960005
MD2689                  MOVE CCK-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME 39970005
                                                                        39980005
ANI612                  ADD +1               TO                         39990005
ANI612                      WK-CCARE-HEAD-CNT                           40000005
                                                                        40010005
MD2689                  PERFORM WRITE-HEADER-CCARE                      40020005
MD2689                 ELSE                                             40030005
NR0007                 IF GENO-VENDOR-CVSCM THEN                        40040005
MD6619                  MOVE CVS-SENDER-ID     TO CAR-BAT-SENDER-ID     40050005
MD6619                  MOVE CVS-SENDER-NAME   TO CAR-BAT-SENDER-NAME   40060005
MD6619                  MOVE CVS-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID   40070005
MD6619                  MOVE CVS-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME 40080005
MD6619                                                                  40090005
MD6619                  ADD +1               TO                         40100005
MD6619                      WK-CVSCM-HEAD-CNT                           40110005
MD6619                                                                  40120005
MD6619                  PERFORM WRITE-HEADER-CVSCM                      40130005
MD6619                 ELSE                                             40140005
AN4993                  MOVE C-SENDER-ID     TO CAR-BAT-SENDER-ID       40150005
AN4993                  MOVE C-SENDER-NAME   TO CAR-BAT-SENDER-NAME     40160005
AN4993                  MOVE C-RECEIVER-ID   TO CAR-BAT-RECIEVER-ID     40170005
AN4993                  MOVE C-RECEIVER-NAME TO CAR-BAT-RECIEVER-NAME   40180005
                                                                        40190005
ANI612                  ADD +1               TO                         40200005
ANI612                      WK-WYETH-HEAD-CNT                           40210005
                                                                        40220005
AN4993                  PERFORM WRITE-HEADER-WYETH                      40230005
MD2689                 END-IF                                           40240005
MD6619                 END-IF                                           40250005
MA2010                END-IF                                            40260005
NJ1164               END-IF                                             40270005
MI1323              END-IF                                              40280005
AN4993             END-IF                                               40290005
AN1589             END-IF                                               40300005
AN0127             END-IF                                               40310005
AN1887             END-IF                                               40320005
AN1560             END-IF                                               40330005
AN1781             END-IF                                               40340005
AN4993     END-EVALUATE                                                 40350005
AN4993                                                                  40360005
                                                                        40370005
           .                                                            40380005
PERIOD****/ \                                                           40390005
                                                                        40400005
                                                                        40410005
AN0609******************************************************************40420005
AN0609*         4 0 0 1 - C A L L - H R H S 4 Q T R                     40430005
AN0609******************************************************************40440005
AN0609                                                                  40450005
AN0609 4001-CALL-HRHS4QTR.                                              40460005
AN0609                                                                  40470005
AN0609     INITIALIZE HRHS4QTR-PARMS                                    40480005
AN0609                W-4QTR-RENEWAL-DATE                               40490005
AN0609                                                                  40500005
AN0609     SET L-4QTR-HNDI-PROCESSING-CALL   TO TRUE                    40510005
AN0609     SET L-4QTR-HNDO-NO-MATCH          TO TRUE                    40520005
AN0609                                                                  40530005
AN0609     MOVE SUBSCRIBER-PLAN              TO L-4QTR-HNDI-SUB-PLAN    40540005
AN0609                                                                  40550005
AN0609     MOVE GRP-BASE(LINE-INDEX)         TO L-4QTR-HNDI-GROUP-BASE  40560005
AN0609                                                                  40570005
AN0609     MOVE GRP-SEC(LINE-INDEX)          TO L-4QTR-HNDI-GROUP-SECT  40580005
AN0609                                                                  40590005
AN0609     MOVE GENO-VENDOR-ID               TO L-4QTR-HNDI-VENDOR-ID   40600005
AN0609                                                                  40610005
AN0609     MOVE PACKAGE-CODE(LINE-INDEX)     TO W-PKG-LOW-ORDER         40620005
AN0609     MOVE W-PKG-BINARY                 TO L-4QTR-HNDI-PACK-CODE   40630005
AN0609                                                                  40640005
AN0609*****                                                             40650005
AN0609***** CONVERT FST-SVC-DATE YYYYMMDD  FORMAT                       40660005
AN0609*****                                                             40670005
AN0609                                                                  40680005
AN0609     MOVE FST-SVC-DATE (LINE-INDEX)    TO  W-DATE-1-FW            40690005
AN0609     MOVE DATE-FUNCTION-CONVERT        TO  W-FUNCTION-CODE        40700005
AN0609     MOVE DATE-FORMAT-BINARY           TO  W-FORMAT-1             40710005
AN0609     MOVE DATE-FORMAT-YYYYMMDD         TO  W-FORMAT-2             40720005
AN0609     PERFORM CONVERT-DATE                                         40730005
AN0609                                                                  40740005
AN0609     MOVE W-DATE-2-8                   TO WS-FST-SVC-DATE-YYYYMMDD40750005
AN0609                                                                  40760005
AN0609     MOVE WS-FST-SVC-DATE-YYYYMMDD     TO L-4QTR-HNDI-DOS         40770005
AN0609                                                                  40780005
AN0609     CALL W-HRHS4QTR USING HRHS4QTR-PARMS                         40790005
AN0609                                                                  40800005
AN0609                                                                  40810005
AN0609                                                                  40820005
AN0609     .                                                            40830005
PERIOD****/ \                                                           40840005
                                                                        40850005
                                                                        40860005
AN4993/**************************************************************** 40870005
AN4993*          W R I T E - H E A D E R - W Y E T H                    40880005
AN4993* - WRITE HEADER FOR CAREMARK WYETH                               40890005
AN4993******************************************************************40900005
AN4993 WRITE-HEADER-WYETH.                                              40910005
AN4993                                                                  40920005
NR0007     MOVE '780'                   TO OSM-PREF-PLAN                40930005
NR0007     MOVE 'WYETH'                 TO OSM-PREF-VENDOR-CODE         40940005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               40950005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              40960005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             40970005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          40980005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        40990005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              41000005
NR0007                                                                  41010005
NR0007     CALL W-SUBPROGRAM USING                                      41020005
NR0007          OSM-DATA-AREA                                           41030005
NR0007                                                                  41040005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             41050005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           41060005
AN4993     .                                                            41070005
PERIOD****/ \                                                           41080005
AN4993                                                                  41090005
AN4993/**************************************************************** 41100005
AN4993*          W R I T E - H E A D E R - A C A R E                    41110005
AN4993* - WRITE HEADER FOR CAREMARK ACARE                               41120005
AN4993******************************************************************41130005
AN4993 WRITE-HEADER-ACARE.                                              41140005
AN4993                                                                  41150005
NR0007     MOVE '834'                   TO OSM-PREF-PLAN                41160005
NR0007     MOVE 'ACARE'                 TO OSM-PREF-VENDOR-CODE         41170005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               41180005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              41190005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             41200005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          41210005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        41220005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              41230005
NR0007                                                                  41240005
NR0007     CALL W-SUBPROGRAM USING                                      41250005
NR0007          OSM-DATA-AREA                                           41260005
NR0007                                                                  41270005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             41280005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           41290005
AN4993     .                                                            41300005
PERIOD****/ \                                                           41310005
AN1887                                                                  41320005
AN1887/**************************************************************** 41330005
AN1887*          W R I T E - H E A D E R - O P T U M                    41340005
AN1887* - WRITE HEADER FOR CAREMARK OPTUM                               41350005
AN1887******************************************************************41360005
AN1887 WRITE-HEADER-AOPTM.                                              41370005
AN1887                                                                  41380005
NR0007     MOVE '834'                   TO OSM-PREF-PLAN                41390005
NR0007     MOVE 'AOPTM'                 TO OSM-PREF-VENDOR-CODE         41400005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               41410005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              41420005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             41430005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          41440005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        41450005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              41460005
NR0007                                                                  41470005
NR0007     CALL W-SUBPROGRAM USING                                      41480005
NR0007          OSM-DATA-AREA                                           41490005
NR0007                                                                  41500005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             41510005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           41520005
AN1887     .                                                            41530005
PERIOD****/ \                                                           41540005
AN1589                                                                  41550005
AN1589/**************************************************************** 41560005
AN1589*          W R I T E - H E A D E R - M A G L N                    41570005
AN1589* - WRITE HEADER FOR ANTHEM MAGELLEN                              41580005
AN1589******************************************************************41590005
AN1589 WRITE-HEADER-MAGLN.                                              41600005
AN1589                                                                  41610005
AN1589     MOVE '834'                   TO OSM-PREF-PLAN                41620005
AN1589     MOVE 'MAGLN'                 TO OSM-PREF-VENDOR-CODE         41630005
AN1589     MOVE '1'                     TO OSM-PREF-CYCLE               41640005
AN1589     MOVE 'C'                     TO OSM-PREF-FORMAT              41650005
AN1589     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             41660005
AN1589     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          41670005
AN1589     MOVE 'W'                         TO OSM-CALL-FUNCTION        41680005
AN1589     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              41690005
AN1589                                                                  41700005
AN1589     CALL W-SUBPROGRAM USING                                      41710005
AN1589          OSM-DATA-AREA                                           41720005
AN1589                                                                  41730005
AN1589     ADD +1                   TO A-TOT-HEADER-RECORDS             41740005
AN1589     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           41750005
AN1589     .                                                            41760005
PERIOD****/ \                                                           41770005
AN0127                                                                  41780005
AN0127/**************************************************************** 41790005
AN0127*          W R I T E - H E A D E R - B E A C N                    41800005
AN0127* - WRITE HEADER FOR ANTHEM BEACON                                41810005
AN0127******************************************************************41820005
AN0127 WRITE-HEADER-BEACN.                                              41830005
AN0127                                                                  41840005
AN0127     MOVE '834'                   TO OSM-PREF-PLAN                41850005
AN0127     MOVE 'BEACN'                 TO OSM-PREF-VENDOR-CODE         41860005
AN0127     MOVE '1'                     TO OSM-PREF-CYCLE               41870005
AN0127     MOVE 'C'                     TO OSM-PREF-FORMAT              41880005
AN0127     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             41890005
AN0127     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          41900005
AN0127     MOVE 'W'                         TO OSM-CALL-FUNCTION        41910005
AN0127     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              41920005
AN0127                                                                  41930005
AN0127     CALL W-SUBPROGRAM USING                                      41940005
AN0127          OSM-DATA-AREA                                           41950005
AN0127                                                                  41960005
AN0127     ADD +1                   TO A-TOT-HEADER-RECORDS             41970005
AN0127     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           41980005
AN0127     .                                                            41990005
PERIOD****/ \                                                           42000005
AN1560                                                                  42010005
AN1560/**************************************************************** 42020005
AN1560*          W R I T E - H E A D E R - A O P T R                    42030005
AN1560* - WRITE HEADER FOR ANTHEM AOPTR                                 42040005
AN1560******************************************************************42050005
AN1560 WRITE-HEADER-AOPTR.                                              42060005
AN1560                                                                  42070005
AN1560     MOVE '834'                   TO OSM-PREF-PLAN                42080005
AN1560     MOVE 'AOPTR'                 TO OSM-PREF-VENDOR-CODE         42090005
AN1560     MOVE '1'                     TO OSM-PREF-CYCLE               42100005
AN1560     MOVE 'C'                     TO OSM-PREF-FORMAT              42110005
AN1560     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             42120005
AN1560     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          42130005
AN1560     MOVE 'W'                         TO OSM-CALL-FUNCTION        42140005
AN1560     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              42150005
AN1560                                                                  42151005
AN1560     CALL W-SUBPROGRAM USING                                      42152005
AN1560          OSM-DATA-AREA                                           42153005
AN1560                                                                  42154005
AN1560     ADD +1                   TO A-TOT-HEADER-RECORDS             42155005
AN1560     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           42156005
AN1560     .                                                            42157005
PERIOD****/ \                                                           42158005
AN1781                                                                  42159005
AN1781/**************************************************************** 42160005
AN1781*          W R I T E - H E A D E R - A A R C H                    42160105
AN1781* - WRITE HEADER FOR ANTHEM AARCH                                 42160205
AN1781******************************************************************42160305
AN1781 WRITE-HEADER-AARCH.                                              42160405
AN1781                                                                  42160505
AN1781     MOVE '834'                   TO OSM-PREF-PLAN                42160605
AN1781     MOVE 'AARCH'                 TO OSM-PREF-VENDOR-CODE         42160705
AN1781     MOVE '1'                     TO OSM-PREF-CYCLE               42160805
AN1781     MOVE 'C'                     TO OSM-PREF-FORMAT              42160905
AN1781     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             42161005
AN1781     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          42161105
AN1781     MOVE 'W'                         TO OSM-CALL-FUNCTION        42161205
AN1781     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              42161305
AN1781                                                                  42161405
AN1781     CALL W-SUBPROGRAM USING                                      42161505
AN1781          OSM-DATA-AREA                                           42161605
AN1781                                                                  42161705
AN1781     ADD +1                   TO A-TOT-HEADER-RECORDS             42161805
AN1781     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           42161905
AN1781     .                                                            42162005
PERIOD****/ \                                                           42162105
MI1323                                                                  42162205
MI1323/**************************************************************** 42162305
MI1323*          W R I T E - H E A D E R - M C A R E                    42162405
MI1323* - WRITE HEADER FOR CAREMARK ACARE                               42162505
MI1323******************************************************************42162605
MI1323 WRITE-HEADER-MCARE.                                              42162705
MI1323                                                                  42162805
NR0007     MOVE '710'                   TO OSM-PREF-PLAN                42162905
NR0007     MOVE 'MCARE'                 TO OSM-PREF-VENDOR-CODE         42163005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               42164005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              42165005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             42166005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          42167005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        42168005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              42169005
NR0007                                                                  42170005
NR0007     CALL W-SUBPROGRAM USING                                      42180005
NR0007          OSM-DATA-AREA                                           42190005
NR0007                                                                  42200005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             42210005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           42220005
MI1323     .                                                            42230005
PERIOD****/ \                                                           42240005
                                                                        42250005
NJ1164/**************************************************************** 42260005
NJ1164*          W R I T E - H E A D E R - N J C D H                    42270005
NJ1164*                                                                 42280005
NJ1164******************************************************************42290005
NJ1164 WRITE-HEADER-NJCDH.                                              42300005
NJ1164                                                                  42310005
NR0007     MOVE '780'                   TO OSM-PREF-PLAN                42320005
NR0007     MOVE 'NJCDH'                 TO OSM-PREF-VENDOR-CODE         42330005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               42340005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              42350005
NJ7557     MOVE SPACES                  TO                              42360005
NJ7557          OSM-OUTBOUND-REC(2872:29)                               42370005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             42380005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          42390005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        42400005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              42410005
NR0007                                                                  42420005
NR0007     CALL W-SUBPROGRAM USING                                      42430005
NR0007          OSM-DATA-AREA                                           42440005
NR0007                                                                  42450005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             42460005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           42470005
NJ1164     .                                                            42480005
PERIOD****/ \                                                           42490005
MA2010                                                                  42500005
MA2010/**************************************************************** 42510005
MA2010*         W R I T E - H E A D E R - M A C M K                     42520005
MA2010* - WRITE HEADER FOR CAREMARK MACMK                               42530005
MA2010***************************************************************** 42540005
MA2010 WRITE-HEADER-MACMK.                                              42550005
MA2010                                                                  42560005
NR0007     MOVE '700'                   TO OSM-PREF-PLAN                42570005
NR0007     MOVE 'MACMK'                 TO OSM-PREF-VENDOR-CODE         42580005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               42590005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              42600005
MA1079     MOVE SPACES                  TO                              42610005
MA1079          OSM-OUTBOUND-REC(2863:38)                               42620005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             42630005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          42640005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        42650005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              42660005
NR0007                                                                  42670005
NR0007     CALL W-SUBPROGRAM USING                                      42680005
NR0007          OSM-DATA-AREA                                           42690005
NR0007                                                                  42700005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             42710005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           42720005
MA2010     .                                                            42730005
PERIOD****/ \                                                           42740005
MD2689                                                                  42750005
MD2689/**************************************************************** 42760005
MD2689*         W R I T E - H E A D E R - C C A R E                     42770005
MD2689* - WRITE HEADER FOR CAREMARK CCARE                               42780005
MD2689***************************************************************** 42790005
MD2689 WRITE-HEADER-CCARE.                                              42800005
MD2689                                                                  42810005
NR0007     MOVE '690'                   TO OSM-PREF-PLAN                42820005
NR0007     MOVE 'CCARE'                 TO OSM-PREF-VENDOR-CODE         42830005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               42840005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              42850005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             42860005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          42870005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        42880005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              42890005
NR0007                                                                  42900005
NR0007     CALL W-SUBPROGRAM USING                                      42910005
NR0007          OSM-DATA-AREA                                           42920005
NR0007                                                                  42930005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             42940005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           42950005
MD2689     .                                                            42960005
PERIOD****/ \                                                           42970005
                                                                        42980005
MD6619                                                                  42990005
MD6619/**************************************************************** 43000005
MD6619*         W R I T E - H E A D E R - C V S C M                     43010005
MD6619* - WRITE HEADER FOR CAREMARK CVSCM                               43020005
MD6619***************************************************************** 43030005
MD6619 WRITE-HEADER-CVSCM.                                              43040005
MD6619                                                                  43050005
NR0007     MOVE '690'                   TO OSM-PREF-PLAN                43060005
NR0007     MOVE 'CVSCM'                 TO OSM-PREF-VENDOR-CODE         43070005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               43080005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              43090005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             43100005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          43110005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        43120005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              43130005
NR0007                                                                  43140005
NR0007     CALL W-SUBPROGRAM USING                                      43150005
NR0007          OSM-DATA-AREA                                           43160005
NR0007                                                                  43170005
NR0007     ADD +1                   TO A-TOT-HEADER-RECORDS             43180005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           43190005
MD6619     .                                                            43200005
PERIOD****/ \                                                           43210005
                                                                        43220005
      /**************************************************************** 43230005
      *          W R I T E - T R A I L E R                              43240005
      * - WRITE TRAILER WHEN PLAN ID CHANGES                            43250005
      ******************************************************************43260005
                                                                        43270005
       WRITE-TRAILER.                                                   43280005
                                                                        43290005
           MOVE SPACES TO CAREMARK-TRAILER-RECORD                       43300005
                                                                        43310005
           SET  TRAILER-REC               TO TRUE                       43320005
                                                                        43330005
AN4993                                                                  43340005
AN4993     EVALUATE TRUE                                                43350005
NR0007         WHEN  GENO-LAST-CALL                                     43360005
AN4993             IF WYETH-TRAILER-NO                                  43370005
                                                                        43380005
ANI612                ADD +1              TO                            43390005
ANI612                WK-WYETH-TRLR-CNT                                 43400005
AN4993                PERFORM WRITE-TRAILER-WYETH                       43410005
                                                                        43420005
AN4993             END-IF                                               43430005
AN4993             IF ACARE-TRAILER-NO                                  43440005
                                                                        43450005
ANI612                ADD +1              TO                            43460005
ANI612                WK-ACARE-TRLR-CNT                                 43470005
                                                                        43480005
AN4993                PERFORM WRITE-TRAILER-ACARE                       43490005
AN4993             END-IF                                               43500005
MI1323             IF MCARE-TRAILER-NO                                  43510005
                                                                        43520005
ANI612                ADD +1              TO                            43530005
ANI612                WK-MCARE-TRLR-CNT                                 43540005
                                                                        43550005
MI1323                PERFORM WRITE-TRAILER-MCARE                       43560005
MI1323             END-IF                                               43570005
NJ1164             IF NJCDH-TRAILER-NO                                  43580005
                                                                        43590005
ANI612                ADD +1              TO                            43600005
ANI612                WK-NJCDH-TRLR-CNT                                 43610005
                                                                        43620005
NJ1164                PERFORM WRITE-TRAILER-NJCDH                       43630005
NJ1164             END-IF                                               43640005
AN1887             IF AOPTM-TRAILER-NO                                  43650005
                                                                        43660005
ANI612                ADD +1              TO                            43670005
ANI612                WK-AOPTM-TRLR-CNT                                 43680005
                                                                        43690005
AN1887                PERFORM WRITE-TRAILER-AOPTM                       43700005
AN1887             END-IF                                               43710005
AN1589             IF MAGLN-TRAILER-NO                                  43720005
AN1589                                                                  43730005
AN1589                ADD +1              TO                            43740005
AN1589                WK-MAGLN-TRLR-CNT                                 43750005
AN1589                                                                  43760005
AN1589                PERFORM WRITE-TRAILER-MAGLN                       43770005
AN1589             END-IF                                               43780005
AN0127             IF BEACN-TRAILER-NO                                  43790005
AN0127                                                                  43800005
AN0127                ADD +1              TO                            43810005
AN0127                WK-BEACN-TRLR-CNT                                 43820005
AN0127                                                                  43830005
AN0127                PERFORM WRITE-TRAILER-BEACN                       43840005
AN0127             END-IF                                               43850005
AN1560             IF AOPTR-TRAILER-NO                                  43860005
AN1560                                                                  43870005
AN1560                ADD +1              TO                            43880005
AN1560                WK-AOPTR-TRLR-CNT                                 43890005
AN1560                                                                  43900005
AN1560                PERFORM WRITE-TRAILER-AOPTR                       43910005
AN1560             END-IF                                               43920005
AN1781             IF AARCH-TRAILER-NO                                  43930005
AN1781                                                                  43940005
AN1781                ADD +1              TO                            43950005
AN1781                WK-AARCH-TRLR-CNT                                 43960005
AN1781                                                                  43970005
AN1781                PERFORM WRITE-TRAILER-AARCH                       43980005
AN1781             END-IF                                               43990005
MA2010             IF MACMK-TRAILER-NO                                  44000005
                                                                        44001005
ANI612                ADD +1              TO                            44002005
ANI612                WK-MACMK-TRLR-CNT                                 44003005
                                                                        44004005
MA2010                PERFORM WRITE-TRAILER-MACMK                       44005005
MA2010             END-IF                                               44006005
MD2689             IF CCARE-TRAILER-NO                                  44007005
                                                                        44008005
ANI612                ADD +1              TO                            44009005
ANI612                WK-CCARE-TRLR-CNT                                 44010005
                                                                        44020005
MD2689                PERFORM WRITE-TRAILER-CCARE                       44030005
MD2689             END-IF                                               44040005
MD6619             IF CVSCM-TRAILER-NO                                  44050005
MD6619                                                                  44060005
MD6619                ADD +1              TO                            44070005
MD6619                WK-CVSCM-TRLR-CNT                                 44080005
MD6619                                                                  44090005
MD6619                PERFORM WRITE-TRAILER-CVSCM                       44100005
MD6619             END-IF                                               44110005
AN4993     END-EVALUATE                                                 44120005
AN4993                                                                  44130005
           .                                                            44140005
PERIOD****/ \                                                           44150005
AN4993/**************************************************************** 44160005
AN4993*          W R I T E - T R A I L E R - W Y E T H                  44170005
AN4993* - WRITE TRAILER FOR CAREMARK WYETH                              44180005
AN4993******************************************************************44190005
AN4993 WRITE-TRAILER-WYETH.                                             44200005
AN4993                                                                  44210005
AN4993     MOVE A-BAT-DETAIL-RECORDS-NJ   TO                            44220005
AN4993          CAR-CNTL-REC-COUNT                                      44230005
AN4993     MOVE A-BAT-TRL-OOP-AMT-NJ      TO                            44240005
AN4993          CAR-TOTAL-OOP-AMT                                       44250005
AN4993                                                                  44260005
NR0007     MOVE '780'                   TO OSM-PREF-PLAN                44270005
NR0007     MOVE 'WYETH'                 TO OSM-PREF-VENDOR-CODE         44280005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               44290005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              44300005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             44310005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          44320005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        44330005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              44340005
NR0007                                                                  44350005
NR0007     CALL W-SUBPROGRAM USING                                      44360005
NR0007          OSM-DATA-AREA                                           44370005
NR0007                                                                  44380005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            44390005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           44400005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-NJ          44410005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-NJ             44420005
AN4993     .                                                            44430005
PERIOD****/ \                                                           44440005
AN4993                                                                  44450005
AN4993/**************************************************************** 44460005
AN4993*          W R I T E - T R A I L E R - A C A R E                  44470005
AN4993* - WRITE TRAILER FOR CAREMARK ACARE                              44480005
AN4993******************************************************************44490005
AN4993 WRITE-TRAILER-ACARE.                                             44500005
AN4993                                                                  44510005
NR0007     MOVE A-BAT-DETAIL-RECORDS-AN   TO                            44520005
NR0007          CAR-CNTL-REC-COUNT                                      44530005
NR0007     MOVE A-BAT-TRL-OOP-AMT-AN      TO                            44540005
NR0007          CAR-TOTAL-OOP-AMT                                       44550005
NR0007                                                                  44560005
NR0007     MOVE '834'                   TO OSM-PREF-PLAN                44570005
NR0007     MOVE 'ACARE'                 TO OSM-PREF-VENDOR-CODE         44580005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               44590005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              44600005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             44610005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          44620005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        44630005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              44640005
NR0007                                                                  44650005
NR0007     CALL W-SUBPROGRAM USING                                      44660005
NR0007          OSM-DATA-AREA                                           44670005
NR0007                                                                  44680005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            44690005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           44700005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-AN          44710005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-AN             44720005
AN4993     .                                                            44730005
PERIOD****/ \                                                           44740005
AN1887                                                                  44750005
AN1887/**************************************************************** 44760005
AN1887*          W R I T E - T R A I L E R - O P T U M                  44770005
AN1887* - WRITE TRAILER FOR CAREMARK OPTUM                              44780005
AN1887******************************************************************44790005
AN1887 WRITE-TRAILER-AOPTM.                                             44800005
AN1887                                                                  44810005
NR0007     MOVE A-BAT-DETAIL-RECORDS-AN   TO                            44820005
NR0007          CAR-CNTL-REC-COUNT                                      44830005
NR0007     MOVE A-BAT-TRL-OOP-AMT-AN      TO                            44840005
NR0007          CAR-TOTAL-OOP-AMT                                       44850005
NR0007                                                                  44860005
NR0007     MOVE '834'                   TO OSM-PREF-PLAN                44870005
NR0007     MOVE 'AOPTM'                 TO OSM-PREF-VENDOR-CODE         44880005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               44890005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              44900005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             44910005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          44920005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        44930005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              44940005
NR0007                                                                  44950005
NR0007     CALL W-SUBPROGRAM USING                                      44960005
NR0007          OSM-DATA-AREA                                           44970005
NR0007                                                                  44980005
NR0007     ADD +1                   TO AN-TOT-TRAILER-RECORDS           44990005
NR0007     ADD +1                   TO AN-TOT-CRMK-OUT-RECORDS          45000005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-AN          45010005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-AN             45020005
AN1887     .                                                            45030005
PERIOD****/ \                                                           45040005
AN1589/**************************************************************** 45050005
AN1589*          W R I T E - T R A I L E R - M A G L N                  45060005
AN1589* - WRITE TRAILER FOR ANTHEM MAGELLAN                             45070005
AN1589******************************************************************45080005
AN1589 WRITE-TRAILER-MAGLN.                                             45090005
AN1589                                                                  45100005
AN1589     MOVE AN-BAT-DETAIL-RECMAGL-MG   TO                           45110005
AN1589          CAR-CNTL-REC-COUNT                                      45120005
AN1589     MOVE A-BAT-TRL-OOP-AMT-MG      TO                            45130005
AN1589          CAR-TOTAL-OOP-AMT                                       45140005
AN1589                                                                  45150005
AN1589     MOVE '834'                   TO OSM-PREF-PLAN                45160005
AN1589     MOVE 'MAGLN'                 TO OSM-PREF-VENDOR-CODE         45170005
AN1589     MOVE '1'                     TO OSM-PREF-CYCLE               45180005
AN1589     MOVE 'C'                     TO OSM-PREF-FORMAT              45190005
AN1589     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             45200005
AN1589     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          45210005
AN1589     MOVE 'W'                         TO OSM-CALL-FUNCTION        45220005
AN1589     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              45230005
AN1589                                                                  45240005
AN1589     CALL W-SUBPROGRAM USING                                      45250005
AN1589          OSM-DATA-AREA                                           45260005
AN1589                                                                  45270005
AN1589     ADD +1                   TO AN-TOT-TRAILER-RECMAGL           45280005
AN1589     ADD +1                   TO AN-TOT-CRMK-OUT-RECMAGL          45290005
AN1589     MOVE +0                  TO AN-BAT-DETAIL-RECMAGL-MG         45300005
AN1589     MOVE +0                  TO A-BAT-TRL-OOP-AMT-MG             45310005
AN1589     .                                                            45320005
PERIOD****/ \                                                           45330005
AN0127/**************************************************************** 45340005
AN0127*          W R I T E - T R A I L E R - B E A C N                  45350005
AN0127* - WRITE TRAILER FOR ANTHEM BEACON                               45360005
AN0127******************************************************************45370005
AN0127 WRITE-TRAILER-BEACN.                                             45380005
AN0127                                                                  45390005
AN0127     MOVE AN-BAT-DETAIL-RECBCN-BC    TO                           45400005
AN0127          CAR-CNTL-REC-COUNT                                      45410005
AN0127     MOVE A-BAT-TRL-OOP-AMT-BCN     TO                            45420005
AN0127          CAR-TOTAL-OOP-AMT                                       45430005
AN0127                                                                  45440005
AN0127     MOVE '834'                   TO OSM-PREF-PLAN                45450005
AN0127     MOVE 'BEACN'                 TO OSM-PREF-VENDOR-CODE         45460005
AN0127     MOVE '1'                     TO OSM-PREF-CYCLE               45470005
AN0127     MOVE 'C'                     TO OSM-PREF-FORMAT              45480005
AN0127     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             45490005
AN0127     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          45500005
AN0127     MOVE 'W'                         TO OSM-CALL-FUNCTION        45510005
AN0127     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              45520005
AN0127                                                                  45530005
AN0127     CALL W-SUBPROGRAM USING                                      45540005
AN0127          OSM-DATA-AREA                                           45550005
AN0127                                                                  45560005
AN0127     ADD +1                   TO AN-TOT-TRAILER-RECBCN            45570005
AN0127     ADD +1                   TO AN-TOT-CRMK-OUT-RECBCN           45580005
AN0127     MOVE +0                  TO AN-BAT-DETAIL-RECBCN-BC          45590005
AN0127     MOVE +0                  TO A-BAT-TRL-OOP-AMT-BCN            45600005
AN0127     .                                                            45610005
PERIOD****/ \                                                           45620005
AN1560/**************************************************************** 45630005
AN1560*          W R I T E - T R A I L E R - A O P T R                  45640005
AN1560* - WRITE TRAILER FOR ANTHEM AOPTR                                45650005
AN1560******************************************************************45660005
AN1560 WRITE-TRAILER-AOPTR.                                             45670005
AN1560                                                                  45680005
AN1560     MOVE AN-BAT-DETAIL-RECOPTR-OR TO                             45690005
AN1560          CAR-CNTL-REC-COUNT                                      45700005
AN1560     MOVE A-BAT-TRL-OOP-AMT-AOR     TO                            45710005
AN1560          CAR-TOTAL-OOP-AMT                                       45720005
AN1560                                                                  45721005
AN1560     MOVE '834'                   TO OSM-PREF-PLAN                45722005
AN1560     MOVE 'AOPTR'                 TO OSM-PREF-VENDOR-CODE         45723005
AN1560     MOVE '1'                     TO OSM-PREF-CYCLE               45724005
AN1560     MOVE 'C'                     TO OSM-PREF-FORMAT              45725005
AN1560     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             45726005
AN1560     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          45727005
AN1560     MOVE 'W'                         TO OSM-CALL-FUNCTION        45728005
AN1560     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              45729005
AN1560                                                                  45729105
AN1560     CALL W-SUBPROGRAM USING                                      45729205
AN1560          OSM-DATA-AREA                                           45729305
AN1560                                                                  45729405
AN1560     ADD +1                   TO AN-TOT-TRAILER-RECOPTR           45729505
AN1560     ADD +1                   TO AN-TOT-CRMK-OUT-RECOPTR          45729605
AN1560     MOVE +0                  TO AN-BAT-DETAIL-RECOPTR-OR         45729705
AN1560     MOVE +0                  TO A-BAT-TRL-OOP-AMT-AOR            45729805
AN1560     .                                                            45729905
PERIOD****/ \                                                           45730005
AN1781/**************************************************************** 45730105
AN1781*          W R I T E - T R A I L E R - A A R C H                  45730205
AN1781* - WRITE TRAILER FOR ANTHEM AARCH                                45730305
AN1781******************************************************************45730405
AN1781 WRITE-TRAILER-AARCH.                                             45730505
AN1781                                                                  45730605
AN1781     MOVE AN-BAT-DETAIL-RECARCH-OR TO                             45730705
AN1781          CAR-CNTL-REC-COUNT                                      45730805
AN1781     MOVE A-BAT-TRL-OOP-AMT-AAR     TO                            45730905
AN1781          CAR-TOTAL-OOP-AMT                                       45731005
AN1781                                                                  45731105
AN1781     MOVE '834'                   TO OSM-PREF-PLAN                45731205
AN1781     MOVE 'AARCH'                 TO OSM-PREF-VENDOR-CODE         45731305
AN1781     MOVE '1'                     TO OSM-PREF-CYCLE               45731405
AN1781     MOVE 'C'                     TO OSM-PREF-FORMAT              45731505
AN1781     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             45731605
AN1781     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          45731705
AN1781     MOVE 'W'                         TO OSM-CALL-FUNCTION        45731805
AN1781     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              45731905
AN1781                                                                  45732005
AN1781     CALL W-SUBPROGRAM USING                                      45732105
AN1781          OSM-DATA-AREA                                           45732205
AN1781                                                                  45732305
AN1781     ADD +1                   TO AN-TOT-TRAILER-RECARCH           45732405
AN1781     ADD +1                   TO AN-TOT-CRMK-OUT-RECARCH          45732505
AN1781     MOVE +0                  TO AN-BAT-DETAIL-RECARCH-OR         45732605
AN1781     MOVE +0                  TO A-BAT-TRL-OOP-AMT-AAR            45732705
AN1781     .                                                            45732805
PERIOD****/ \                                                           45732905
MI1323                                                                  45733005
MI1323/**************************************************************** 45733105
MI1323*          W R I T E - T R A I L E R - M C A R E                  45733205
MI1323* - WRITE TRAILER FOR CAREMARK MCARE                              45733305
MI1323******************************************************************45733405
MI1323 WRITE-TRAILER-MCARE.                                             45733505
MI1323                                                                  45733605
MI1323     MOVE A-BAT-DETAIL-RECORDS-CH   TO                            45733705
MI1323          CAR-CNTL-REC-COUNT                                      45733805
MI1323     MOVE A-BAT-TRL-OOP-AMT-CH      TO                            45733905
MI1323          CAR-TOTAL-OOP-AMT                                       45734005
MI1323                                                                  45735005
NR0007     MOVE '710'                   TO OSM-PREF-PLAN                45736005
NR0007     MOVE 'MCARE'                 TO OSM-PREF-VENDOR-CODE         45737005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               45738005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              45739005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             45740005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          45750005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        45760005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              45770005
NR0007                                                                  45780005
NR0007     CALL W-SUBPROGRAM USING                                      45790005
NR0007          OSM-DATA-AREA                                           45800005
NR0007                                                                  45810005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            45820005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           45830005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-CH          45840005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-CH             45850005
MI1323     .                                                            45860005
PERIOD****/ \                                                           45870005
MI1323*                                                                 45880005
NJ1164/**************************************************************** 45890005
NJ1164*          W R I T E - T R A I L E R - N J C D H                  45900005
NJ1164*                                                                 45910005
NJ1164******************************************************************45920005
NJ1164 WRITE-TRAILER-NJCDH.                                             45930005
NJ1164                                                                  45940005
NJ1164     MOVE A-BAT-DETAIL-RECORDS-CD   TO                            45950005
NJ1164          CAR-CNTL-REC-COUNT                                      45960005
NJ1164     MOVE A-BAT-TRL-OOP-AMT-CD      TO                            45970005
NJ1164          CAR-TOTAL-OOP-AMT                                       45980005
NJ1164                                                                  45990005
NR0007     MOVE '780'                   TO OSM-PREF-PLAN                46000005
NR0007     MOVE 'NJCDH'                 TO OSM-PREF-VENDOR-CODE         46010005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               46020005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              46030005
NJ7557     MOVE SPACES                  TO                              46040005
NJ7557          OSM-OUTBOUND-REC(2872:29)                               46050005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             46060005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          46070005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        46080005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              46090005
NR0007                                                                  46100005
NR0007     CALL W-SUBPROGRAM USING                                      46110005
NR0007          OSM-DATA-AREA                                           46120005
NR0007                                                                  46130005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            46140005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           46150005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-CD          46160005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-CD             46170005
NJ1164     .                                                            46180005
PERIOD****/ \                                                           46190005
NJ1164*                                                                 46200005
MA2010                                                                  46210005
MA2010/**************************************************************** 46220005
MA2010*         W R I T E - T R A I L E R - M A C M K                   46230005
MA2010* - WRITE TRAILER FOR CAREMARK MACMK                              46240005
MA2010***************************************************************** 46250005
MA2010 WRITE-TRAILER-MACMK.                                             46260005
MA2010                                                                  46270005
MA2010     MOVE A-BAT-DETAIL-RECORDS-CK   TO                            46280005
MA2010          CAR-CNTL-REC-COUNT                                      46290005
MA2010     MOVE A-BAT-TRL-OOP-AMT-CK      TO                            46300005
MA2010          CAR-TOTAL-OOP-AMT                                       46310005
MA2010                                                                  46320005
NR0007     MOVE '700'                   TO OSM-PREF-PLAN                46330005
NR0007     MOVE 'MACMK'                 TO OSM-PREF-VENDOR-CODE         46340005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               46350005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              46360005
MA1079     MOVE SPACES                  TO                              46370005
MA1079          OSM-OUTBOUND-REC(2863:38)                               46380005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             46390005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          46400005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        46410005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              46420005
NR0007                                                                  46430005
NR0007     CALL W-SUBPROGRAM USING                                      46440005
NR0007          OSM-DATA-AREA                                           46450005
NR0007                                                                  46460005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            46470005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           46480005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-CK          46490005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-CK             46500005
MA2010     .                                                            46510005
PERIOD****/ \                                                           46520005
MD2689                                                                  46530005
MD2689/**************************************************************** 46540005
MD2689*         W R I T E - T R A I L E R - C C A R E                   46550005
MD2689* - WRITE TRAILER FOR CAREMARK CCARE                              46560005
MD2689***************************************************************** 46570005
MD2689 WRITE-TRAILER-CCARE.                                             46580005
MD2689                                                                  46590005
MD2689     MOVE A-BAT-DETAIL-RECORDS-CF   TO                            46600005
MD2689          CAR-CNTL-REC-COUNT                                      46610005
MD2689     MOVE A-BAT-TRL-OOP-AMT-CF      TO                            46620005
MD2689          CAR-TOTAL-OOP-AMT                                       46630005
MD2689                                                                  46640005
NR0007     MOVE '690'                   TO OSM-PREF-PLAN                46650005
NR0007     MOVE 'CCARE'                 TO OSM-PREF-VENDOR-CODE         46660005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               46670005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              46680005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             46690005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          46700005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        46710005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              46720005
NR0007                                                                  46730005
NR0007     CALL W-SUBPROGRAM USING                                      46740005
NR0007          OSM-DATA-AREA                                           46750005
NR0007                                                                  46760005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            46770005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           46780005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-CF          46790005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-CF             46800005
MD2689     .                                                            46810005
PERIOD****/ \                                                           46820005
MD6619                                                                  46830005
MD6619/**************************************************************** 46840005
MD6619*         W R I T E - T R A I L E R - C V S C M                   46850005
MD6619* - WRITE TRAILER FOR CAREMARK CVSCM                              46860005
MD6619***************************************************************** 46870005
MD6619 WRITE-TRAILER-CVSCM.                                             46880005
MD6619                                                                  46890005
MD6619     MOVE A-BAT-DETAIL-RECORDS-CV   TO                            46900005
MD6619          CAR-CNTL-REC-COUNT                                      46910005
MD6619     MOVE A-BAT-TRL-OOP-AMT-CV      TO                            46920005
MD6619          CAR-TOTAL-OOP-AMT                                       46930005
MD6619                                                                  46940005
NR0007     MOVE '690'                   TO OSM-PREF-PLAN                46950005
NR0007     MOVE 'CVSCM'                 TO OSM-PREF-VENDOR-CODE         46960005
NR0007     MOVE '1'                     TO OSM-PREF-CYCLE               46970005
NR0007     MOVE 'C'                     TO OSM-PREF-FORMAT              46980005
NR0007     MOVE CAREMARK-HEADER-RECORD  TO OSM-OUTBOUND-REC             46990005
NR0007     MOVE 'HRHSAO3C'                  TO OSM-CALL-MODULE          47000005
NR0007     MOVE 'W'                         TO OSM-CALL-FUNCTION        47010005
NR0007     MOVE 'HRHSAO3X' TO W-SUBPROGRAM                              47020005
NR0007                                                                  47030005
NR0007     CALL W-SUBPROGRAM USING                                      47040005
NR0007          OSM-DATA-AREA                                           47050005
NR0007                                                                  47060005
NR0007     ADD +1                   TO A-TOT-TRAILER-RECORDS            47070005
NR0007     ADD +1                   TO A-TOT-CRMK-OUT-RECORDS           47080005
NR0007     MOVE +0                  TO A-BAT-DETAIL-RECORDS-CV          47090005
NR0007     MOVE +0                  TO A-BAT-TRL-OOP-AMT-CV             47100005
MD6619     .                                                            47110005
PERIOD****/ \                                                           47120005
      ***************************************************************** 47130005
      *    2 2 0 0 - L O A D - C A L L - S U B P R G M - T A B L E    * 47140005
      ***************************************************************** 47150005
                                                                        47160005
       CALL-SUBPRGM-BENEFITS.                                           47170005
                                                                        47180005
           SET SW-BENEFIT-DID-NOT-MATCH TO TRUE                         47190005
           MOVE GENO-PLAN-CODE         TO W-SPGMPARM-PLAN-CODE          47200005
NR0007     MOVE GENO-GROUP-NUMBER      TO W-SPGMPARM-GROUP-NUMBER       47210005
NR0007     MOVE GENO-SECTION-NUMBER    TO W-SPGMPARM-SECTION-NUMBER     47220005
           MOVE GENO-PACKAGE-CODE      TO W-SPGMPARM-PACKAGE-CODE       47230005
NR0007     MOVE GENO-SEQUENCE-NUMBER   TO W-SPGMPARM-SEQUENCE-NUMBER    47240005
NR0007     MOVE GENO-EFFECTIVE-DATE    TO W-SPGMPARM-EFFECTIVE-DATE     47250005
           MOVE GENO-CANCEL-DATE       TO W-SPGMPARM-CANCEL-DATE        47260005
NR0007     MOVE GENO-NETWORK-INDICATOR TO W-SPGMPARM-NETWORK-INDICATOR  47270005
           MOVE GENO-VENDOR-FORMAT     TO W-SPGMPARM-VENDOR-FORMAT      47280005
           MOVE GENO-CLAIM-STATUS      TO W-SPGMPARM-CLAIM-STATUS       47290005
NR0007     MOVE GENO-FREQUENCY         TO W-SPGMPARM-FREQUENCY          47300005
NR0007     MOVE GENO-COM-TIMING        TO W-SPGMPARM-COM-TIMING         47310005
NR0007     MOVE GENO-COM-CAT           TO W-SPGMPARM-COM-CAT            47320005
NR0007     MOVE GENO-COM-MAX           TO W-SPGMPARM-COM-MAX            47330005
           MOVE GENO-VENDOR-ID         TO W-SPGMPARM-VENDOR-ID          47340005
                                                                        47350005
NR0007     SET W-SPGMPARM-PROCESSING-CALL TO TRUE                       47360005
NR0007     SET W-SPGMPARM-SELECTED-LINE TO LINE-INDEX                   47370005
NR0007     MOVE 'L' TO W-SPGMPARM-FUNC-CODE                             47380005
           MOVE 'HRHSAB2K' TO W-SUBPROGRAM                              47390005
                                                                        47400005
NR0007*    DISPLAY 'CALLING LINE LEVEL BENEFITS '                       47410005
           CALL W-SUBPROGRAM USING                                      47420005
                DYNAMIC-CLAIM, CLAIM-HEADER,                            47430005
NR0007          CLAIM-DETAIL, W-SPGMPARM-TABLE-ENTRY                    47440005
                                                                        47450005
NR0007     IF  W-SPGMPARM-ERROR-CODE = SPACES                           47460005
               SET  SW-BENEFIT-MATCHED TO TRUE                          47470005
           END-IF                                                       47480005
                                                                        47490005
           .                                                            47500005
PERIOD****/ \                                                           47510005
                                                                        47520005
      /*****************************************************************47530005
      *                         CONVERT-DATE                           *47540005
      *                                                                *47550005
      *    - CALL WAASDATE                                             *47560005
      *    - IF RETURN CODE GOOD, CONTINUE                             *47570005
      *    - ELSE, ABEND U2000                                         *47580005
      ******************************************************************47590005
                                                                        47600005
       CONVERT-DATE.                                                    47610005
                                                                        47620005
NRS012     CALL  C-DATE-PROGRAM   USING  W-ENTIRE-DATE-AREA             47630005
           END-CALL                                                     47640005
                                                                        47650005
           IF  W-RETURN-CODE   =  ZEROS                                 47660005
               CONTINUE                                                 47670005
           ELSE                                                         47680005
               DISPLAY 'RETURN CODE ' W-RETURN-CODE                     47690005
NRS012         SET  BAD-RETURN-FROM-NASXDATE   TO  TRUE                 47700005
NRS012         SET  W-BAD-NASXDATE-ABND        TO  TRUE                 47710005
               PERFORM  PROGRAM-ABEND                                   47720005
           END-IF                                                       47730005
PERIOD     .                                                            47740005
PERIOD**--/ \                                                           47750005
      /*****************************************************************47760005
      *                 P R O G R A M - A B E N D                       47770005
      * - DISPLAY ABEND MESSAGE                                         47780005
      * - CALL ABEND PROGRAM                                            47790005
      ******************************************************************47800005
                                                                        47810005
       PROGRAM-ABEND.                                                   47820005
                                                                        47830005
           DISPLAY USER-ABENDS                                          47840005
                                                                        47850005
           CALL ABEND-PROGRAM                                           47860005
           .                                                            47870005
PERIOD****/ \                                                           47880005
AN4656*                                                                 47890005
AN4656* 4000-LOAD-AOPTM-BH-CLIENTS.                                     47900005
AN4656*                                                                 47910005
AN4656*    OPEN INPUT  I-AOPTM-BH-CLIENTS                               47920005
AN4656*    IF W-AOPTM-BH-GOOD-OPEN                                      47930005
AN4656*       READ I-AOPTM-BH-CLIENTS INTO WS-AOPTM-BH-CLIENTS-REC      47940005
AN4656*         AT END                                                  47950005
AN4656*            SET SW-END-OF-CLIENT-FILE TO TRUE                    47960005
AN4656*       END-READ                                                  47970005
AN4656*       IF W-AOPTM-BH-GOOD-READ                                   47980005
AN4656*          PERFORM UNTIL (NOT W-AOPTM-BH-GOOD-READ) OR            47990005
AN4656*                        SW-END-OF-CLIENT-FILE                    48000005
AN4656*             IF WS-CLIENT-FILE-CNTL = '*'                        48010005
AN4656*                OR WS-CLIENT-COUNT  = 99                         48020005
AN4656*                CONTINUE                                         48030005
AN4656*             ELSE                                                48040005
AN4656*                ADD 1                    TO WS-CLIENT-SUB        48050005
AN4656*                                            WS-CLIENT-COUNT      48060005
AN4656*                MOVE WS-AOPTM-BH-CLIENT-VALUE                    48070005
AN4656*                  TO WS-CLIENT-VALUE(WS-CLIENT-SUB)              48080005
AN4656*                MOVE WS-AOPTM-BH-PROV-ID                         48090005
AN4656*                  TO WS-CLIENT-PROV-ID(WS-CLIENT-SUB)            48100005
AN4656*             END-IF                                              48110005
AN4656*                                                                 48120005
AN4656*             READ I-AOPTM-BH-CLIENTS INTO WS-AOPTM-BH-CLIENTS-REC48130005
AN4656*               AT END                                            48140005
AN4656*                  SET SW-END-OF-CLIENT-FILE TO TRUE              48150005
AN4656*             END-READ                                            48160005
AN4656*          END-PERFORM                                            48170005
AN4656*                                                                 48180005
AN4656*          IF NOT W-AOPTM-BH-GOOD-READ                            48190005
AN4656*             MOVE 0                      TO WS-CLIENT-COUNT      48200005
AN4656*          END-IF                                                 48210005
AN4656*       END-IF                                                    48220005
AN4656*    END-IF                                                       48230005
AN4656*                                                                 48240005
AN4656*    CLOSE  I-AOPTM-BH-CLIENTS                                    48250005
      *    .                                                            48260005
PERIOD****/ \                                                           48270005
AN4656*    SKIP1                                                        48280005
AN4656* 4000-EXIT.                                                      48290005
AN4656*                                                                 48300005
AN4656* 4100-READ-AOPTM-BH-CLIENTS.                                     48310005
AN4656*                                                                 48320005
AN4656*    SET SW-NOT-BH-CLAIM        TO TRUE                           48330005
AN4656*                                                                 48340005
AN4656*    IF SW-CLIENT-TABLE-NOT-LOADED                                48350005
AN4656*       PERFORM 4000-LOAD-AOPTM-BH-CLIENTS THRU 4000-EXIT         48360005
AN4656*       SET SW-CLIENT-TABLE-LOADED TO TRUE                        48370005
AN4656*    END-IF                                                       48380005
AN4656*                                                                 48390005
AN4656*    PERFORM VARYING WS-CLIENT-SUB FROM 1 BY 1                    48400005
AN4656*      UNTIL WS-CLIENT-SUB > WS-CLIENT-COUNT                      48410005
AN4656*         OR SW-BH-CLAIM                                          48420005
AN4656**        IF PROVIDER-GROUP-NUMBER =                              48430005
AN4656**           WS-CLIENT-PROV-ID(WS-CLIENT-SUB)                     48440005
AN4656*         IF O-AOPTM-DATA(105:30) =                               48450005
AN4656*            WS-CLIENT-VALUE(WS-CLIENT-SUB)                       48460005
AN4656*            SET SW-BH-CLAIM    TO TRUE                           48470005
AN4656*         END-IF                                                  48480005
AN4656*    END-PERFORM                                                  48490005
AN4656*                                                                 48500005
PERIOD     .                                                            48510005
PERIOD****/ \                                                           48520005
AN4656*    SKIP1                                                        48530005
AN4656* 4100-EXIT.                                                      48540005
AN4656*    EXIT.                                                        48550005
